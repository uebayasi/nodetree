{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}


module Tree where

import           Control.Arrow      (second)
import           Control.Lens       ((%~), (&), (.~), (^.))
import qualified Control.Lens       as Lens

import           Control.Monad
import           Data.IntMap.Strict (IntMap, Key)
import qualified Data.IntMap.Strict as IM
import           Debug.Trace        (trace)
import           GHC.Exts           (IsString (..))
import           Text.Show.Pretty   (ppShow)


data TreeNode a = TreeNode
    { _nodeValue    :: a
    , _nodeParent   :: Maybe Key
    , _nodeChildren :: IntMap (TreeNode a)
    }
    deriving (Show, Eq)
Lens.makeLenses ''TreeNode


type NodePair a = (Key, TreeNode a)
type NodeInfo a = ([NodePair a], [NodePair a], [NodePair a])
data NodeInfo_ a = NodeInfo_
    { _niExcluding :: [NodePair a]
    , _niSubnode   :: [NodePair a]
    , _niIncluding :: [NodePair a]
    }
    deriving (Show, Eq)
Lens.makeLenses ''NodeInfo_


rootNode_ :: a -> TreeNode a
rootNode_ a = TreeNode
    { _nodeValue = a
    , _nodeParent = Nothing
    , _nodeChildren = IM.empty
    }


emptyNodeInfo :: NodeInfo_ a
emptyNodeInfo = NodeInfo_
    { _niExcluding = []
    , _niSubnode = []
    , _niIncluding = []
    }


instance (Ord a) => Ord (TreeNode a) where
    compare na nb =
        compare (na ^. nodeValue) (nb ^. nodeValue)


type NodeCompare a = a -> a -> Ordering


buildTree :: (Show a) => NodeCompare a -> [(Key, a)] -> NodePair a
buildTree cmp [] = undefined
buildTree cmp vs =
    let (x:xs) = map (second rootNode_) vs
#ifdef NODETREE_DEBUG
                & trace ("buildTree: " ++ ppShow vs)
#endif
    in foldl (flip (insertRoot cmp)) x xs


insertRoot :: (Show a) => NodeCompare a -> NodePair a -> NodePair a -> NodePair a
insertRoot cmp (newidx, newnode) (idx, node) =
    let ((idx', node'), c) =
            insertNode cmp (newidx, newnode) (idx, node)
    in case c of
        LT ->
            let newnode' = newnode
                    & nodeParent .~ (node ^. nodeParent)
                    & nodeChildren .~ IM.fromList [(idx, node')]
            in (newidx, newnode')
        _ ->
            (idx', node')


insertNode :: (Show a) => NodeCompare a -> NodePair a -> NodePair a -> (NodePair a, Ordering)
insertNode cmp (newidx, newnode) (idx, node) =
    let c = cmp (node ^. nodeValue) (newnode ^. nodeValue)
#ifdef NODETREE_DEBUG
                & trace ("children: " ++ show (node ^. nodeChildren))
#endif
    in case c of
        LT ->
            -- Direct parent to `newnode'
            let node' = node & nodeParent .~ Just newidx
            in ((idx, node'), c)

        EQ ->
            -- Nothing to do; `node' is not affected
            ((idx, node), c)

        GT ->
            -- Update children
            -- Return `node' with updated children
            let
                newnode' = newnode & nodeParent .~ Just idx
                node' = node
                    & nodeChildren %~ intMapMapPairs (updateChildren cmp idx (newidx, newnode'))
            in ((idx, node'), c)


updateParent :: Int -> NodePair a -> NodePair a
updateParent idx =
    second (nodeParent .~ Just idx)


updateChildren :: (Show a) => NodeCompare a -> Key -> NodePair a -> [NodePair a] -> [NodePair a]
updateChildren cmp idx (newidx, newnode) cs =
    let
        -- Collect updated children
        -- (newidx, newnode) is not changed during traversal
        (p, ni) = foldl (flip (insertChild cmp)) ((newidx, newnode), emptyNodeInfo) cs

        subtree =
            -- LT
            if not (null (ni ^. niSubnode)) then
                let children = ni ^. niSubnode
                        & map (updateParent newidx)
                    newnode' = newnode
                        & nodeChildren .~ IM.fromList children
                in (newidx, newnode') : (ni ^. niExcluding)

            -- GT or EQ
            else case ni ^. niIncluding of
                -- EQ
                [] ->
                    let newnode' = newnode
                            & nodeParent .~ Just idx
                    in (newidx, newnode) : (ni ^. niExcluding)

                -- GT
                -- Existing child is updated and returned instead of the adding node
                [(newidx, newnode)] ->
                    let newnode' = newnode
                            & nodeParent .~ Just idx
                    in (newidx, newnode') : (ni ^. niExcluding)

                _ ->
                    undefined
    in subtree
#ifdef NODETREE_DEBUG
            & trace ("newnode: " ++ show newnode)
            & trace ("inluding: " ++ show (ni ^. niIncluding))
            & trace ("exluding: " ++ show (ni ^. niExcluding))
            & trace ("subnode: " ++ show (ni ^. niSubnode))
            & trace ("subtree: " ++ show subtree)
#endif


insertChild :: (Show a) => NodeCompare a -> NodePair a -> (NodePair a, NodeInfo_ a) -> (NodePair a, NodeInfo_ a)
insertChild cmp (idx, node) ((newidx, newnode), ni) =
    let ((idx', node'), c) = insertNode cmp (newidx, newnode) (idx, node)
        f c =
            case c of
                LT -> niSubnode %~ ((idx, node) :)
                EQ -> niExcluding %~ ((idx, node) :)
                -- `newnode' is inserted below; `node' must be updated
                GT -> niIncluding %~ ((idx', node') :)
    -- Only update ni
    in ((newidx, newnode), ni & f c)


intMapMapPairs :: (Show a) => ([(Key, a)] -> [(Key, b)]) -> IntMap a -> IntMap b
intMapMapPairs f =
    IM.fromList . f . IM.toList
