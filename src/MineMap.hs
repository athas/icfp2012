module MineMap
  ( Pos
  , LiftState(..)
  , Cell(..)
  , MineMap(..)
  , newMap
  , changeMap
  , mapBounds
  , getCell
  , isEarth
  , isRobot
  , isWall
  , isRock
  , isLambda
  , isWalkable
  , isLift
  , isOpenLift
  , isEmpty
  , setCell
  , Action(..)
  , Route
  , routeToString
  , stringToRoute
  ) where

import Debug.Trace
import Control.Arrow
import Data.Array
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

type Pos = (Int, Int)

data LiftState = Open | Closed
                 deriving (Eq, Ord, Show)

data Cell = Earth | Robot | Wall | Rock | Lambda | Lift LiftState | Empty
            deriving (Eq, Ord, Show)

data MineMap = MineMap { robot :: Pos
                       , cells :: Array Pos Cell
                       , lifts :: S.Set Pos
                       , rocks :: S.Set Pos
                       , lambdas :: S.Set Pos
                       , water :: Int
                       , flooding :: Int
                       , waterproof :: Int
                       }
             deriving (Eq, Ord, Show)

newMap :: (Int, Int) -> Int -> Int -> Int -> MineMap
newMap p = MineMap p m S.empty S.empty S.empty
  where m = listArray ((1,1), p) (repeat Wall) // [(p, Robot)]

mapBounds :: MineMap -> (Int, Int)
mapBounds = snd . bounds . cells

inBounds :: Pos -> MineMap -> Bool
inBounds (x,y) m = x > 0 && y > 0 && x <= mx && y <= my
  where (mx, my) = mapBounds m

getCell :: MineMap -> Pos -> Cell
getCell m p@(x,y) = cells m ! p

cellProp :: (Cell -> Bool) -> Bool -> MineMap -> Pos -> Bool
cellProp f d m p = if p `inBounds` m then f (getCell m p) else d

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
isWalkable = cellProp f False
  where f Rock = False
        f Wall = False
        f (Lift Closed) = False
        f _ = True

setCell :: MineMap -> Pos -> Cell -> MineMap
setCell m p c = changeMap m [(p,c)]

changeMap :: MineMap -> [(Pos, Cell)] -> MineMap
changeMap m l = m' { cells = cells m' // l' }
  where (m', l') = foldl change (m, []) l
        change (m'', l'') (p, c) = second (++l'') $ register m'' p c

register :: MineMap -> Pos -> Cell -> (MineMap, [(Pos, Cell)])
register m p c = case c of
                   Lift _ -> (m' { lifts = S.insert p (lifts m') }, l)
                   Rock   -> (m' { rocks = S.insert p (rocks m') }, l)
                   Lambda -> (m' { lambdas = S.insert p (lambdas m') }, l)
                   Robot  -> (m' { robot = p },
                              if isRobot m' (robot m) && p /= robot m'
                              then (robot m', Empty) : l
                              else l)
                   _      -> (m', l)
  where l = [(p,c)]
        m' = case getCell m p of
               Lift _ -> m { lifts = S.delete p (lifts m) }
               Rock -> m { rocks = S.delete p (rocks m) }
               Lambda -> m { lambdas = S.delete p (lambdas m) }
               _ -> m

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
