module MineMap
  ( Pos
  , LiftState(..)
  , Cell(..)
  , MineMap(..)
  , newMap
  , isEarth
  , isRobot
  , isWall
  , isRock
  , isLambda
  , isLift
  , isEmpty
  , setCell
  , Action(..)
  , Route
  , routeToString
  ) where

import qualified Data.Map as M
import qualified Data.Set as S

type Pos = (Int, Int)

data LiftState = Open | Closed
                 deriving (Eq, Ord, Show)

data Cell = Earth | Robot | Wall | Rock | Lambda | Lift LiftState | Empty
            deriving (Eq, Ord, Show)

data MineMap = MineMap { robot :: Pos
                       , cells :: M.Map Pos Cell
                       , lifts :: S.Set Pos
                       , rocks :: S.Set Pos
                       , lambdas :: S.Set Pos
                       }
             deriving (Eq, Ord, Show)

newMap :: Pos -> MineMap
newMap p = MineMap p (M.singleton p Robot) S.empty S.empty S.empty

cellProp :: (Cell -> Bool) -> Bool -> MineMap -> Pos -> Bool
cellProp f d m p = maybe d f $ M.lookup p $ cells m

isWall :: MineMap -> Pos -> Bool
isWall = cellProp (==Wall) False

isRobot :: MineMap -> Pos -> Bool
isRobot = cellProp (==Robot) False

isEarth :: MineMap -> Pos -> Bool
isEarth = cellProp (==Earth) False

isLift :: MineMap -> Pos -> Bool
isLift = cellProp f False
  where f (Lift _) = True
        f _        = False

isRock :: MineMap -> Pos -> Bool
isRock = cellProp (==Rock) False

isEmpty :: MineMap -> Pos -> Bool
isEmpty = cellProp (==Empty) False

isLambda :: MineMap -> Pos -> Bool
isLambda = cellProp (==Lambda) False

setCell :: MineMap -> Pos -> Cell -> MineMap
setCell m p c = register m' p c
  where m' = maybe m (unregister m p) $ M.lookup p $ cells m

unregister :: MineMap -> Pos -> Cell -> MineMap
unregister m p c = case c of
                     Lift _ -> m' { lifts = S.delete p (lifts m') }
                     Rock   -> m' { rocks = S.delete p (rocks m') }
                     Lambda -> m' { lambdas = S.delete p (lambdas m') }
                     _      -> m'
  where m' = m { cells = M.delete p (cells m) }

register :: MineMap -> Pos -> Cell -> MineMap
register m p c = case c of
                   Lift _ -> m' { lifts = S.insert p (lifts m') }
                   Rock   -> m' { rocks = S.insert p (rocks m') }
                   Lambda -> m' { lambdas = S.insert p (lambdas m') }
                   Robot  -> m' { robot = p }
                   _      -> m'
  where m' = m { cells = M.insert p c (cells m) }

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
