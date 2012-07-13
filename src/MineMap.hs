module MineMap
  ( Pos
  , LiftState(..)
  , Cell(..)
  , MineMap(..)
  , Action(..)
  , Route
  , routeToString
  ) where

import qualified Data.Map as M

type Pos = (Int, Int)

data LiftState = Open | Closed
                 deriving (Eq, Ord, Show)

data Cell = Earth | Robot | Wall | Rock | Lambda | LambdaLift LiftState | Empty
            deriving (Eq, Ord, Show)

data MineMap = MineMap Pos (M.Map Pos Cell)
             deriving (Eq, Ord, Show)

data Action = MoveUp | MoveDown | MoveLeft | MoveRight | Wait | Abort

type Route = [Action]

routeToString :: Route -> String
routeToString = map f
  where f MoveUp = 'U'
        f MoveDown = 'D'
        f MoveLeft = 'L'
        f MoveRight = 'R'
        f Wait = 'W'
        f Abort = 'A'
