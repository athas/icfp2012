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
import Data.Array
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
  where m = listArray ((1,1), p) (repeat Empty) // [(p, Robot)]

mapBounds :: MineMap -> (Int, Int)
mapBounds = snd . bounds . cells

getCell :: MineMap -> Pos -> Cell
getCell m p = cells m ! p

cellProp :: (Cell -> Bool) -> MineMap -> Pos -> Bool
cellProp f m p = f (getCell m p)

isWall :: MineMap -> Pos -> Bool
isWall = cellProp (==Wall)

isRobot :: MineMap -> Pos -> Bool
isRobot = cellProp (==Robot)

isEarth :: MineMap -> Pos -> Bool
isEarth = cellProp (==Earth)

isLift :: MineMap -> Pos -> Bool
isLift = cellProp f
  where f (Lift _) = True
        f _        = False

isOpenLift :: MineMap -> Pos -> Bool
isOpenLift = cellProp (==Lift Open)

isRock :: MineMap -> Pos -> Bool
isRock = cellProp (==Rock)

isEmpty :: MineMap -> Pos -> Bool
isEmpty = cellProp (==Empty)

isLambda :: MineMap -> Pos -> Bool
isLambda = cellProp (==Lambda)

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
