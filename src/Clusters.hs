-- Group coordinates if they are adjacent
module Clusters (clusters) where

import Data.List

clusters :: [(Int,Int)] -> [[(Int,Int)]]
clusters [] = []
clusters (p:ps) = cluster : clusters rest
  where
    cluster = nub $ extract ps [p]
    rest = ps \\ cluster

extract :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
extract _  [] = []
extract ps ns = ns ++ extract (ps \\ ns) ns'
  where
    accum n = (++) $ (filter . next_to) n ps
    ns' = foldr accum [] ns

next_to :: (Int,Int) -> (Int,Int) -> Bool
next_to (a,b) (c,d) = (a+1,b) == (c,d) || (a-1,b) == (c,d)
                   || (a,b+1) == (c,d) || (a,b-1) == (c,d)

