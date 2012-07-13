module Heuristics
  ( Heuristic(..)
  , runHeuristic
  , dumbHeuristic
  )
  where

import MineMap
import Simulation

import Control.Monad.Reader
import Data.List
import Data.Ord
import qualified Data.Set as S

data Heuristic = Heuristic {
    nextLambda :: MineMap -> [Pos]
  , routeTo :: MineMap -> Pos -> [Route]
  }

runHeuristic :: MineMap -> Heuristic -> Route
runHeuristic m h = maximumBy (comparing value) $
                   runReader (run (walk m []) []) m
  where value r = score m r $ walkToEnd m r
        run sim r = do let m' = simState sim
                           ls = take 3 $ nextLambda h m'
                           routes = concatMap (take 3 . routeTo h m') ls
                       if simEnded sim then return [r]
                       else liftM (r:) $ liftM concat $ forM routes $ \route ->
                         run (sim `walkFrom` route) (r++route)

dumbHeuristic :: Heuristic
dumbHeuristic = Heuristic {
                  nextLambda = S.toList . lambdas
                , routeTo = \m p -> pathsTo m (robot m) p
                }

isWalkable :: MineMap -> Pos -> Bool
isWalkable m p = isEmpty m p || isEarth m p || isLambda m p

walkRoute :: Pos -> Route -> Pos
walkRoute = foldl move
    where move (x,y) MoveUp    = (x,y+1)
          move (x,y) MoveDown  = (x,y-1)
          move (x,y) MoveLeft  = (x-1,y)
          move (x,y) MoveRight = (x+1,y)
          move (x,y) Wait      = (x,y)

pathsTo :: MineMap -> Pos -> Pos -> [Route]
pathsTo = genRoutes [[]]
    where genRoutes [] _ _ _ = [[Abort]] -- No walkable routes, abort
          genRoutes rs m s e =
            let newrs = filter (\r -> isWalkable m $ walkRoute s r) $
                        concat $ map (\r -> map (:r)
                        [MoveUp,MoveDown,MoveLeft,MoveRight,Wait]) rs
                lambdars = filter (\r -> isLambda m $ walkRoute s r) newrs
            in map reverse lambdars ++ genRoutes newrs m s e
