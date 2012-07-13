module MineMap
  ( Pos
  , LiftState(..)
  , Cell(..)
  , MineMap (robot, cells, lambdas, rocks)
  , newMap
  , mapBounds
  , getCell
  , isEarth
  , isRobot
  , isWall
  , isRock
  , isLambda
  , isLift
  , isOpenLift
  , isEmpty
  , setCell
  , Action(..)
  , Route
  , routeToString
  , stringToRoute
  ) where

import Control.Arrow
import Data.Maybe
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

mapBounds :: MineMap -> (Int, Int)
mapBounds = (maximum *** maximum) . unzip . M.keys . cells

getCell :: MineMap -> Pos -> Cell
getCell m p = fromMaybe (error "Out of bounds") $ M.lookup p $ cells m

cellProp :: (Cell -> Bool) -> Bool -> MineMap -> Pos -> Bool
cellProp f d m p = maybe d f $ M.lookup p $ cells m

isWall :: MineMap -> Pos -> Bool
isWall = cellProp (==Wall) True

isRobot :: MineMap -> Pos -> Bool
isRobot = cellProp (==Robot) False

isEarth :: MineMap -> Pos -> Bool
isEarth = cellProp (==Earth) False

isLift :: MineMap -> Pos -> Bool
isLift = cellProp f False
  where f (Lift _) = True
        f _        = False

isOpenLift :: MineMap -> Pos -> Bool
isOpenLift = cellProp (==Lift Open) False

isRock :: MineMap -> Pos -> Bool
isRock = cellProp (==Rock) False

isEmpty :: MineMap -> Pos -> Bool
isEmpty = cellProp (==Empty) False

isLambda :: MineMap -> Pos -> Bool
isLambda = cellProp (==Lambda) False

isWalkable :: MineMap -> Pos -> Bool
isWalkable m p = isEmpty m p || isEarth m p || isLambda m p

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
                   Robot  -> m' { robot = p
                                , cells = if isRobot m' (robot m')
                                          then M.insert (robot m') Empty (cells m')
                                          else cells m'
                                }
                   _      -> m'
  where m' = m { cells = M.insert p c (cells m) }

data Action = MoveUp | MoveDown | MoveLeft | MoveRight | Wait | Abort
              deriving (Eq, Ord, Show)

type Route = [Action]

routeToString :: Route -> String
routeToString = map f
  where f MoveUp = 'U'
        f MoveDown = 'D'
        f MoveLeft = 'L'
        f MoveRight = 'R'
        f Wait = 'W'
        f Abort = 'A'

stringToRoute :: String -> Maybe Route
stringToRoute = mapM f
  where f 'U' = Just MoveUp
        f 'D' = Just MoveDown
        f 'L' = Just MoveLeft
        f 'R' = Just MoveRight
        f 'W' = Just Wait
        f 'A' = Just Abort
        f _   = Nothing
