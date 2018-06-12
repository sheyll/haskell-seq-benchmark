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
        testTreeL w h = Deep.force (tree1L w h)
        testTreeS w h = Deep.force (tree1S w h) 
        w1 = 8
        h1 = 6
        w2 = 10000
        h2 = 1
        l1 = show w1 ++ " ** " ++ show h1
        l2 = show w2 ++ " ** " ++ show h2
     defaultMain 
      [
         bgroup "get tree edges as List"
               [ bench l1 $ nf edgesList (testTreeL w1 h1)
               , bench l2 $ nf edgesList (testTreeL w2 h2)
               ]
        , bgroup "get tree edges as Seq"
               [ bench l1 $ nf edgesSeq (testTreeS w1 h1)
               , bench l2 $ nf edgesSeq (testTreeS w2 h2)
               ]
      ]

data MyThingy = MyThingy Int Bool Double Integer
   deriving (Read, Show, Eq, Ord, NFData, Generic)

-- ---------------------------------------------------------

data TreeS a = NodeS a (Seq (TreeS a))
   deriving (Read, Show, Eq, Ord, NFData, Generic)

edgesSeq :: TreeS a -> Seq (a, a)
edgesSeq t = go t S.empty
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

edgesList :: TreeL a -> [(a, a)]
edgesList t = go t []
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
