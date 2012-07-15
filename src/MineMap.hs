module MineMap
  ( Pos
  , LiftState(..)
  , Cell(..)
  , MineMap(..)
  , newMap
  , changeMap
  , mapBounds
  , isValidPos
  , cellNeighbors
  , getCell
  , isEarth
  , isRobot
  , isWall
  , isRock
  , isNormalRock
  , isLambdaRock
  , isLambda
  , isLift
  , isOpenLift
  , isWalkable
  , isEmpty
  , isBeard
  , isRazor
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
          | Trampoline Char Pos | Target Int
          | Razor | Beard
          | LambdaRock
            deriving (Eq, Ord, Show)

data MineMap = MineMap { robot :: Pos
                       , cells :: Array Pos Cell
                       , lifts :: S.Set Pos
                       , rocks :: S.Set Pos
                       , hoRocks :: S.Set Pos
                       , beards :: S.Set Pos
                       , lambdas :: S.Set Pos
                       , razors :: S.Set Pos
                       , water :: Int
                       , flooding :: Int
                       , waterproof :: Int
                       , beardGrowth :: Int
                       , currentRazors :: Int
                       }
             deriving (Eq, Ord, Show)

newMap :: (Int, Int) -> Int -> Int -> Int -> Int -> Int -> MineMap
newMap p = MineMap p m S.empty S.empty S.empty S.empty S.empty S.empty
  where m = listArray ((1,1), p) (repeat Wall) // [(p, Robot)]

mapBounds :: MineMap -> (Int, Int)
mapBounds = snd . bounds . cells

isValidPos :: MineMap -> Pos -> Bool
isValidPos m (x, y) = 1 <= x && x <= mX && 1 <= y && y <= mY
  where
    (mX, mY) = mapBounds m

cellNeighbors :: MineMap -> Pos -> [Pos]
cellNeighbors m (x,y) = filter (isValidPos m) ps
  where ps = [(x+1,y+1),(x+1,y),(x+1,y-1),
              (x,y+1),(x,y-1),
              (x-1,y+1),(x-1,y),(x-y,y-1)]

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
isRock = cellProp f
  where f Rock = True
        f LambdaRock = True
        f _ = False

isNormalRock :: MineMap -> Pos -> Bool
isNormalRock = cellProp (==Rock)

isLambdaRock :: MineMap -> Pos -> Bool
isLambdaRock = cellProp (==LambdaRock)

isEmpty :: MineMap -> Pos -> Bool
isEmpty = cellProp (==Empty)

isBeard :: MineMap -> Pos -> Bool
isBeard = cellProp (==Beard)

isRazor :: MineMap -> Pos -> Bool
isRazor = cellProp (==Razor)

isLambda :: MineMap -> Pos -> Bool
isLambda = cellProp (==Lambda)

isWalkable :: MineMap -> Pos -> Bool
isWalkable = cellProp f
  where f Empty = True
        f Lambda = True
        f Earth = True
        f (Lift Open) = True
        f Razor = True
        f (Trampoline _ _) = True
        f _ = False

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
                   LambdaRock -> (m' { hoRocks = S.insert p (hoRocks m') }, l)
                   Beard  -> (m' { beards = S.insert p (beards m') }, l)
                   Lambda -> (m' { lambdas = S.insert p (lambdas m') }, l)
                   Razor  -> (m' { razors = S.insert p (razors m') }, l)
                   Robot  -> (m' { robot = p },
                              if isRobot m' (robot m) && p /= robot m'
                              then (robot m', Empty) : l
                              else l)
                   _      -> (m', l)
  where l = [(p,c)]
        m' = case getCell m p of
               Lift _ -> m { lifts = S.delete p (lifts m) }
               Rock   -> m { rocks = S.delete p (rocks m) }
               LambdaRock -> m { hoRocks = S.delete p (hoRocks m) }
               Beard  -> m { beards = S.delete p (beards m) }
               Lambda -> m { lambdas = S.delete p (lambdas m) }
               Razor  -> m { razors = S.delete p (razors m) }
               _ -> m

data Action = MoveUp | MoveDown | MoveLeft | MoveRight | Wait | Abort | ApplyRazor
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
        f ApplyRazor = 'S'

stringToRoute :: String -> Maybe Route
stringToRoute = mapM f
  where f 'U' = Just MoveUp
        f 'D' = Just MoveDown
        f 'L' = Just MoveLeft
        f 'R' = Just MoveRight
        f 'W' = Just Wait
        f 'A' = Just Abort
        f 'S' = Just ApplyRazor
        f _   = Nothing
