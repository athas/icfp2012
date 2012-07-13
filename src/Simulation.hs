module Simulation
  ( StopReason(..)
  , act
  , walk
  , walkToEnd
  , step
  , score
  ) where

import MineMap

import qualified Data.Set as S
import Control.Arrow
import Control.Monad.State

data StopReason = RobotAbort | RobotDead | RobotFinished
                  deriving (Eq, Ord, Show)

type Simulation = Either (StopReason, MineMap)

move :: Pos -> Action -> Pos
move (x,y) MoveLeft = (x-1,y)
move (x,y) MoveRight = (x+1,y)
move (x,y) MoveUp = (x,y+1)
move (x,y) MoveDown = (x,y-1)
move (x,y) _ = (x,y)

act :: MineMap -> Action -> Simulation MineMap
act m Abort = Left (RobotAbort, m)
act m Wait  = Right m
act m a =
  case () of
    _ | isOpenLift m (x',y') -> Left (RobotFinished, m')
      | isEmpty m (x',y') || isLambda m (x',y') ||
        isLift m (x',y') || isEarth m (x',y') ->
        Right m'
      | x' == x+1 && y' == y && isRock m (x',y') && isEmpty m (x+2,y) ->
        Right $ (setCell m' (x+2,y) Rock)
      | x' == x-1 && y' == y && isRock m (x',y') && isEmpty m (x-2,y) ->
        Right $ setCell m' (x-2,y) Rock
      | otherwise -> Right m
  where (x,y) = robot m
        (x',y') = move (x,y) a
        m' = setCell m (x',y') Robot

step :: MineMap -> Simulation MineMap
step m = stopCheck m $
         flip execState (newMap $ robot m) $
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

walk :: MineMap -> Route -> Simulation MineMap
walk = foldM (\m -> step <=< act m)

walkToEnd :: MineMap -> Route -> (StopReason, MineMap)
walkToEnd m r = case walk m r of Left e   -> e
                                 Right m' -> (RobotAbort, m')

stopCheck :: MineMap -> MineMap -> Simulation MineMap
stopCheck from to
  | isRock to (second (+1) (robot to)) &&
    isEmpty from (second (+1) (robot to)) =
      Left (RobotDead, to)
  | otherwise = Right to

cost :: Route -> Int
cost = length . takeWhile (/=Abort)

score :: MineMap -> Route -> (StopReason, MineMap) -> Int
score from r (reason, to) =
  case reason of RobotAbort -> base + collected * 25
                 RobotDead -> base
                 RobotFinished -> base + collected * 50
  where collected = S.size (lambdas from) - S.size (lambdas to)
        base = collected * 25 - cost r
