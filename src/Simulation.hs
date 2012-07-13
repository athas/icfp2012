module Simulation
  ( act
  , walk
  , step)
  where

import MineMap

act :: MineMap -> Action -> MineMap
act = undefined

walk :: MineMap -> Route -> MineMap
walk m = foldl act m

step :: MineMap -> MineMap
step = undefined
