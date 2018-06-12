{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Criterion.Main

import Data.List as L
import Data.Sequence as S
import Data.Traversable
import Control.DeepSeq as Deep
import GHC.Generics
import Control.Applicative as A

main :: IO ()
main =
  do let
        testRuns = [(1,1), (2,2), (4,4), (8,5), (10,3), (100,2), (100000,1)]
       
        myBenchGroup (w, h) = 
          bgroup ("children per node: " ++ show w ++ ", depth: " ++ show h)
          [ seqBench
          , seqUnstableBench
          , listBench
          ]
          where
            myBench l f g = do
              let t = Deep.force (f w h)
              bench l (nf g t)

            seqBench = myBench "Seq stableSort" tree1S edgesSeq 
            seqUnstableBench = myBench "Seq unstableSort" tree1S edgesSeq 
            listBench = myBench "List" tree1L edgesList 

     defaultMain (myBenchGroup <$> testRuns)

data MyThingy = MyThingy Int Bool Double Integer
   deriving (Read, Show, Eq, Ord, NFData, Generic)

-- ---------------------------------------------------------

data TreeS a = NodeS a (Seq (TreeS a))
   deriving (Read, Show, Eq, Ord, NFData, Generic)

edgesSeq :: Ord a => TreeS a -> Seq (a, a)
edgesSeq t = S.sort $ go t S.empty
  where 
    go (NodeS pl cs) acc = 
      foldr (\c@(NodeS cl _ccs) a -> (pl, cl) <| go c a) acc cs

edgesSeqUnstable :: Ord a => TreeS a -> Seq (a, a)
edgesSeqUnstable t = S.unstableSort $ go t S.empty
  where 
    go (NodeS pl cs) acc = 
      foldr (\c@(NodeS cl _ccs) a -> (pl, cl) <| go c a) acc cs

tree1S :: Int -> Int -> TreeS  MyThingy 
tree1S w 0 = 
  NodeS (MyThingy 0 True 3.141 (toInteger w)) S.empty
tree1S w h = 
  NodeS
   (MyThingy (fromIntegral h) (w `rem` 2 == 0) (3.141 * fromIntegral (w + h)) (fromIntegral w))
   (S.replicate w (tree1S w (h - 1)))

-- ---------------------------------------------------------

data TreeL a = NodeL a [TreeL a]
   deriving (Read, Show, Eq, Ord, NFData, Generic)

edgesList :: Ord a => TreeL a -> [(a, a)]
edgesList t = L.sort $ go t []
 where 
  go (NodeL pl cs) acc =
    foldr (\c@(NodeL cl _ccs) a -> (pl, cl) : go c a) acc cs

tree1L :: Int -> Int -> TreeL  MyThingy 
tree1L w 0 = 
  NodeL (MyThingy 0 True 3.141 (toInteger w)) []
tree1L w h = 
  NodeL
   (MyThingy (fromIntegral h) (w `rem` 2 == 0) (3.141 * fromIntegral (w + h)) (fromIntegral w))
   (L.replicate w (tree1L w (h - 1)))
