module MineMap
  ( Pos
  , LiftState(..)
  , Cell(..)
  , MineMap(..)) where

import qualified Data.Map as M

type Pos = (Int, Int)

data LiftState = Open | Closed
                 deriving (Eq, Ord, Show)

data Cell = Earth | Robot | Wall | Rock | Lambda | LambdaLift LiftState | Empty
            deriving (Eq, Ord, Show)

data MineMap = MineMap Pos (M.Map Pos Cell)
             deriving (Eq, Ord, Show)
