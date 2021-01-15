{-# LANGUAGE InstanceSigs #-}

module Naive (
  BST,
  Naive
) where

import BST
import Data.List (sort)

newtype Naive a = Naive [a]

instance BST Naive where
  add :: Ord a => Naive a -> a -> Naive a
  add (Naive xs) x = Naive (x:xs)
  size :: Ord a => Naive a -> Int
  size (Naive xs) = length xs
  bstempty :: Ord a => Naive a
  bstempty = Naive []
  kthelem :: Ord a => Int -> Naive a -> a
  kthelem k (Naive xs) = sort xs !! k
