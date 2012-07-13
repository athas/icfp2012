module Simulation
  ( StopReason(..)
  , Simulation
  , act
  , walk
  , walkFrom
  , simState
  , simEnded
  , walkToEnd
  , step
  , score
  ) where

import MineMap

import Data.Maybe
import qualified Data.Set as S
import Control.Arrow
import Control.Monad.State
import Debug.Trace

data StopReason = RobotAbort | RobotDead | RobotFinished
                  deriving (Eq, Ord, Show)

type Simulation = State (Maybe StopReason) MineMap

move :: Pos -> Action -> Pos
move (x,y) MoveLeft = (x-1,y)
move (x,y) MoveRight = (x+1,y)
move (x,y) MoveUp = (x,y+1)
move (x,y) MoveDown = (x,y-1)
move (x,y) _ = (x,y)

act :: MineMap -> Action -> Simulation
act m Abort = put (Just RobotAbort) >> return m
act m Wait  = return m
act m a =
  case () of
    _ | isOpenLift m (x',y') -> put (Just RobotFinished) >> return m'
      | isEmpty m (x',y') || isLambda m (x',y') || isEarth m (x',y') ->
        return m'
      | x' == x+1 && y' == y && isRock m (x',y') && isEmpty m (x+2,y) ->
        return $ (setCell m' (x+2,y) Rock)
      | x' == x-1 && y' == y && isRock m (x',y') && isEmpty m (x-2,y) ->
        return $ setCell m' (x-2,y) Rock
      | otherwise -> return m
  where (x,y) = robot m
        (x',y') = move (x,y) a
        m' = setCell m (x',y') Robot

step :: MineMap -> Simulation
step m = stopCheck m $
         flip execState m $
         mapM_ update $
         liftM2 (flip (,)) [1..h] [1..w]
  where (w,h) = mapBounds m
        update (x,y)
          | isRock m (x,y) && isEmpty m (x,y-1) = do
          setCell' (x,y) Empty
          setCell' (x,y-1) Rock
          | isRock m (x,y) && isRock m (x,y-1) &&
            isEmpty m (x+1,y) && isEmpty m (x+1,y-1) = do
          setCell' (x,y) Empty
          setCell' (x+1,y-1) Rock
          | isRock m (x,y) && isRock m (x,y-1) &&
            (not (isEmpty m (x+1,y)) || not (isEmpty m (x+1,y-1))) &&
            isEmpty m (x-1,y) && isEmpty m (x-1,y-1) = do
          setCell' (x,y) Empty
          setCell' (x-1,y-1) Rock
          | isRock m (x,y) && isLambda m (x,y-1) &&
            isEmpty m (x+1,y) && isEmpty m (x+1,y-1) = do
          setCell' (x,y) Empty
          setCell' (x+1,y-1) Rock
          | isLift m (x,y) && S.null (lambdas m) =
          setCell' (x,y) $ Lift Open
          | otherwise = setCell' (x,y) $ getCell m (x,y)
        setCell' p c = modify $ \m' -> setCell m' p c

walk :: MineMap -> Route -> Simulation
walk m [] = return m
walk m (a:as) = do m' <- step =<< act m a
                   stop <- gets isJust
                   if stop then return m' else walk m' as

walkFrom :: Simulation -> Route -> Simulation
walkFrom sim r = do m <- sim
                    walk m r

simState :: Simulation -> MineMap
simState sim = fst $ runState sim Nothing

simEnded :: Simulation -> Bool
simEnded sim = isJust $ snd $ runState sim Nothing

walkToEnd :: MineMap -> Route -> (StopReason, MineMap)
walkToEnd m r = case runState (walk m r) Nothing of
                  (m', Nothing)     -> (RobotAbort, m')
                  (m', Just reason) -> (reason, m')

squashed :: MineMap -> MineMap -> Bool
squashed from to =
  isRock to (second (+1) (robot to)) && isEmpty from (second (+1) (robot to))

stopCheck :: MineMap -> MineMap -> Simulation
stopCheck from to | squashed from to = modify kill >> return to
                  | otherwise = return to
  where kill (Just RobotFinished) = Just RobotFinished
        kill _                    = Just RobotDead

cost :: Route -> Int
cost = length . takeWhile (/=Abort)

score :: MineMap -> Route -> (StopReason, MineMap) -> Int
score from r (reason, to) =
  case reason of RobotAbort -> base + collected * 25
                 RobotDead -> base
                 RobotFinished -> base + collected * 50
  where collected = S.size (lambdas from) - S.size (lambdas to)
        base = collected * 25 - cost r
