module Simulation
  ( StopReason(..)
  , SimState(..)
  , finished
  , aborted
  , won
  , dead
  , stable
  , stateFromMap
  , walk
  , step
  , trystep
  , score
  , move
  ) where

import MineMap

import Data.Maybe
import qualified Data.Set as S
import Control.Arrow
import Control.Monad.State

data StopReason = RobotAbort | RobotDead | RobotWon
                  deriving (Eq, Ord, Show)

data SimState = SimState { stopReason :: Maybe StopReason
                         , underWater :: Int
                         , waterTimer :: Int
                         , beardTimer :: Int
                         , mineMap :: MineMap
                         , origMap :: MineMap
                         , collected :: Int
                         , steps :: [Action]
                         }
                deriving (Eq, Show)

collectable :: SimState -> Int
collectable sim = S.size (lambdas $ origMap sim)
                  + S.size (hoRocks $ origMap sim)

stateFromMap :: MineMap -> SimState
stateFromMap m = SimState { stopReason = Nothing
                          , underWater = 0
                          , waterTimer = 0
                          , beardTimer = beardGrowth m - 1
                          , mineMap = m
                          , origMap = m
                          , collected = 0
                          , steps = []
                          }

finished :: SimState -> Bool
finished = isJust . stopReason

aborted :: SimState -> Bool
aborted = (==Just RobotAbort) . stopReason

won :: SimState -> Bool
won = (==Just RobotWon) . stopReason

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
            ApplyRazor | currentRazors m > 0 ->
                         Just sim { mineMap =
                                      changeMap m { currentRazors = currentRazors m - 1} $
                                      map (\p -> (p, Empty)) $
                                      filter (isBeard m) (cellNeighbors m (x,y)) }
                       | otherwise -> Nothing
            _
              | isOpenLift m (x',y') -> Just sim { stopReason = Just RobotWon
                                                 , mineMap = m' }
              | isEmpty m (x',y') || isEarth m (x',y') ->
                Just sim { mineMap = m' }
              | x' == x+1 && y' == y && isRock m (x',y') && isEmpty m (x+2,y) ->
                Just sim { mineMap = setCell m' (x+2,y) (getCell m (x',y')) }
              | x' == x-1 && y' == y && isRock m (x',y') && isEmpty m (x-2,y) ->
                Just sim { mineMap = setCell m' (x-2,y) (getCell m (x',y')) }
              | otherwise -> case getCell m (x',y') of
                               Trampoline _ p -> Just sim { mineMap = setCell (setCell m' (x',y') Empty) p Robot }
                               Razor          -> Just sim { mineMap = m' { currentRazors = currentRazors m' + 1 } }
                               Lambda         -> Just sim { mineMap = m', collected = collected sim + 1 }
                               _              -> Just sim

mapupdate :: SimState -> SimState
mapupdate sim = stopCheck m $ waterflow sim { mineMap = m'
                                            , beardTimer =
                                              if beardTimer sim == 0
                                              then beardGrowth m - 1
                                              else beardTimer sim - 1 }
  where m = mineMap sim
        m' = changeMap m $
             map ulifts (S.toList $ lifts m)
             ++ concatMap (shatter . fall) (S.toList (rocks m) ++ S.toList (hoRocks m))
             ++ if beardTimer sim == 0 then
                  concatMap ubeard (S.toList $ beards m)
                else []
        fall (x,y)
          | isEmpty m (x,y-1) =
            Just ((x,y), (x,y-1))
          | isRock m (x,y-1) &&
            isEmpty m (x+1,y) && isEmpty m (x+1,y-1) =
              Just ((x,y), (x+1,y-1))
          | isRock m (x,y-1) &&
            isEmpty m (x-1,y) && isEmpty m (x-1,y-1) =
              Just ((x,y), (x-1,y-1))
          | isLambda m (x,y-1) &&
            isEmpty m (x+1,y) && isEmpty m (x+1,y-1) =
              Just ((x,y), (x+1,y-1))
          | otherwise = Nothing
        shatter Nothing = []
        shatter (Just (from, to@(x,y)))
          | isLambdaRock m from =
            [(from, Empty),
             (to, if not (isEmpty m (x,y-1))
                  then Lambda else LambdaRock)]
          | otherwise = [(from, Empty), (to, Rock)]
        ubeard = map (\p' -> (p', Beard)) . filter (isEmpty m) . cellNeighbors m
        ulifts p | collected sim == collectable sim = (p, Lift Open)
                 | otherwise                        = (p, Lift Closed)

squashed :: MineMap -> MineMap -> Bool
squashed from to =
  (isLambda to (second (+1) (robot to)) || isRock to (second (+1) (robot to)))
  && isEmpty from (second (+1) (robot to))

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
                         Just RobotWon  -> base + collected sim * 50
                         _ -> base + collected sim * 25
  where base = collected sim * 25 - length (steps sim)
