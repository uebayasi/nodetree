module TreeSpec where

import           Test.Hspec


import           Control.Lens       ((%~), (&), (.~), (^.), (^?))
import qualified Control.Lens       as Lens
import           Data.IntMap.Strict (Key)
import qualified Data.IntMap.Strict as IM
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Debug.Trace        (trace)
import           Text.Show.Pretty   (ppShow)

import           Tree


type Range = (Int, Int)


spec =
    describe "tree" $ do
        it "tree" $
            let t = testTree testNodes
                        & trace ("testTree: " ++ ppShow (testTree testNodes))
                x = lookupChild 1 t
            in fmap _nodeValue x `shouldBe` Just (1, 2)
        it "tree" $
            let t = testTree testNodes
                x = lookupChild 1 t
            in fmap _nodeParent x `shouldBe` Just (Just 0)
        it "tree 2" $
            let t = testTree testNodes2
                        & trace ("testTree2: " ++ ppShow (testTree testNodes2))
                x = lookupChild 1 t
            in fmap _nodeParent x `shouldBe` Just (Just 0)
        it "tree 3" $
            let t = testTree testNodes3
                        & trace ("testTree3: " ++ ppShow (testTree testNodes3))
                x = lookupChild 1 t >>= lookupChild 2
            in fmap _nodeParent x `shouldBe` Just (Just 1)
        it "tree 4" $
            let t = testTree testNodes4
                        & trace ("testTree4: " ++ ppShow (testTree testNodes4))
                x = lookupChild 2 t >>= lookupChild 1
            in fmap _nodeParent x `shouldBe` Just (Just 2)
        it "tree 5" $
            let t = testTree testNodes5
                        & trace ("testTree5: " ++ ppShow (testTree testNodes5))
                x = lookupChild 0 t
            in fmap _nodeParent x `shouldBe` Just (Just 1)
        it "tree 9" $
            let t = testTree testNodes9
                        & trace ("testTree9: " ++ ppShow (testTree testNodes9))
                x = lookupChild 1 t >>= lookupChild 3 >>= lookupChild 4
            in fmap _nodeParent x `shouldBe` Just (Just 3)
        it "tree 10" $
            let t = testTree testNodes10
                        & trace ("testTree10: " ++ ppShow (testTree testNodes10))
                x = lookupChild 1 t >>= lookupChild 3 >>= lookupChild 4
            in fmap _nodeParent x `shouldBe` Just (Just 3)


lookupChild xid =
    IM.lookup xid . _nodeChildren


testNodes :: [(Key, Range)]
testNodes =
    [ ( 0, (  0,  3))
    , ( 1, (  1,  2))
    ]


testNodes2 :: [(Key, Range)]
testNodes2 =
    [ ( 0, (  0,  5))
    , ( 1, (  1,  2))
    , ( 2, (  3,  4))
    ]


testNodes3 :: [(Key, Range)]
testNodes3 =
    [ ( 0, (  0,  5))
    , ( 1, (  1,  4))
    , ( 2, (  2,  3))
    ]


testNodes4 :: [(Key, Range)]
testNodes4 =
    [ ( 0, (  0,  5))
    , ( 1, (  2,  3))
    , ( 2, (  1,  4))
    ]


testNodes5 :: [(Key, Range)]
testNodes5 =
    [ ( 0, (  1,  2))
    , ( 1, (  0,  3))
    ]


testNodes9 :: [(Key, Range)]
testNodes9 =
    [ ( 0, ( 0, 10))
    , ( 1, ( 1,  6))
    , ( 2, ( 7,  8))
    , ( 3, ( 2,  5))
    , ( 4, ( 3,  4))
    ]


testNodes10 :: [(Key, Range)]
testNodes10 =
    [ ( 0, ( 0, 10))
    , ( 4, ( 3,  4))
    , ( 1, ( 1,  6))
    , ( 3, ( 2,  5))
    , ( 2, ( 7,  8))
    ]


testTree :: [(Key, Range)] -> TreeNode Range
testTree = snd . buildTree compareRanges


compareRanges :: Range -> Range -> Ordering
compareRanges (ap, aq) (bp, bq)
    | ap > bp && aq < bq = LT
        -- & trace ("cmp: " ++ show (ap, aq) ++ "/" ++ show (bp, bq) ++ " -> LT")
    | ap < bp && aq > bq = GT
        -- & trace ("cmp: " ++ show (ap, aq) ++ "/" ++ show (bp, bq) ++ " -> GT")

    -- Ignore other error cases
    | otherwise = EQ
        -- & trace ("cmp: " ++ show (ap, aq) ++ "/" ++ show (bp, bq) ++ " -> EQ")
