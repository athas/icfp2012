module Simulation
  ( StopReason(..)
  , SimState(..)
  , finished
  , aborted
  , dead
  , stable
  , stateFromMap
  , walk
  , step
  , trystep
  , score
  ) where

import MineMap

import Data.Maybe
import qualified Data.Set as S
import Control.Arrow
import Control.Monad.State

data StopReason = RobotAbort | RobotDead | RobotFinished
                  deriving (Eq, Ord, Show)

data SimState = SimState { stopReason :: Maybe StopReason
                         , underWater :: Int
                         , waterTimer :: Int
                         , mineMap :: MineMap
                         , origMap :: MineMap
                         , steps :: [Action]
                         }
                deriving (Eq, Show)

stateFromMap :: MineMap -> SimState
stateFromMap m = SimState { stopReason = Nothing
                          , underWater = 0
                          , waterTimer = 0
                          , mineMap = m
                          , origMap = m
                          , steps = []
                          }

finished :: SimState -> Bool
finished = isJust . stopReason

aborted :: SimState -> Bool
aborted = (==Just RobotAbort) . stopReason

dead :: SimState -> Bool
dead = (==Just RobotDead) . stopReason

stable :: SimState -> Bool
stable sim = sim `step` Wait == sim

move :: Pos -> Action -> Pos
move (x,y) MoveLeft = (x-1,y)
move (x,y) MoveRight = (x+1,y)
move (x,y) MoveUp = (x,y+1)
move (x,y) MoveDown = (x,y-1)
move (x,y) _ = (x,y)

step :: SimState -> Action -> SimState
step sim a = fromMaybe (mapupdate sim) $ sim `trystep` a

trystep :: SimState -> Action -> Maybe SimState
trystep sim _ | finished sim = Just sim
trystep sim a = liftM (mapupdate . \s -> s { steps = a : steps s }) sim'
  where (x,y) = robot m
        (x',y') = move (x,y) a
        m = mineMap sim
        m' = setCell (mineMap sim) (x',y') Robot
        sim' =
          case a of
            Abort -> Just sim { stopReason = Just RobotAbort }
            Wait  -> Just sim
            _
              | isOpenLift m (x',y') -> Just sim { stopReason = Just RobotFinished
                                                 , mineMap = m' }
              | isEmpty m (x',y') || isLambda m (x',y') || isEarth m (x',y') ->
                Just sim { mineMap = m' }
              | x' == x+1 && y' == y && isRock m (x',y') && isEmpty m (x+2,y) ->
                Just sim { mineMap = setCell m' (x+2,y) Rock }
              | x' == x-1 && y' == y && isRock m (x',y') && isEmpty m (x-2,y) ->
                Just sim { mineMap = setCell m' (x-2,y) Rock }
              | otherwise -> Nothing

mapupdate :: SimState -> SimState
mapupdate sim = stopCheck m $ waterflow sim { mineMap = m' }
  where m = mineMap sim
        m' = changeMap m $ map ulifts (S.toList $ lifts m) ++
                           concatMap urocks (S.toList $ rocks m)
        urocks (x,y)
          | isEmpty m (x,y-1) =
            [((x,y), Empty), ((x,y-1), Rock)]
          | isRock m (x,y-1) &&
            isEmpty m (x+1,y) && isEmpty m (x+1,y-1) =
              [((x,y), Empty), ((x+1,y-1), Rock)]
          | isRock m (x,y-1) &&
            (not (isEmpty m (x+1,y)) || not (isEmpty m (x+1,y-1))) &&
            isEmpty m (x-1,y) && isEmpty m (x-1,y-1) =
              [((x,y), Empty), ((x-1,y-1), Rock)]
          | isLambda m (x,y-1) &&
            isEmpty m (x+1,y) && isEmpty m (x+1,y-1) =
              [((x,y), Empty), ((x+1,y-1), Rock)]
          | otherwise = []
        ulifts (x,y) | S.null (lambdas m) = ((x,y), Lift Open)
                     | otherwise          = ((x,y), Lift Closed)

squashed :: MineMap -> MineMap -> Bool
squashed from to =
  isRock to (second (+1) (robot to)) && isEmpty from (second (+1) (robot to))

drowned :: SimState -> Bool
drowned sim = underWater sim > waterproof m
  where m = mineMap sim

stopCheck :: MineMap -> SimState -> SimState
stopCheck from sim | finished sim = sim
                   | squashed from to || drowned sim =
                     sim { stopReason = Just RobotDead }
                   | otherwise = sim
  where to = mineMap sim

waterflow :: SimState -> SimState
waterflow sim = sim { underWater = if snd (robot m) <= water m
                                   then underWater sim + 1 else 0
                    , waterTimer = if waterTimer sim <= 1 then flooding m
                                   else waterTimer sim - 1
                    , mineMap = m { water = water m + if waterTimer sim == 1 then 1 else 0 }
                    }
  where m = mineMap sim

walk :: SimState -> Route -> SimState
walk = foldl step

score :: SimState -> Int
score sim =
  case stopReason sim of Just RobotDead -> base
                         Just RobotFinished -> base + collected * 50
                         _ -> base + collected * 25
  where collected = S.size (lambdas $ origMap sim) - S.size (lambdas $ mineMap sim)
        base = collected * 25 - length (steps sim)
