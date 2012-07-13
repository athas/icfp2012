module Heuristics
  ( Heuristic(..)
  , runHeuristic
  , dumbHeuristic
  )
  where

import MineMap
import Simulation
import MapPrinter

import Control.Monad.Reader
import Data.List
import Data.Ord
import Debug.Trace
import qualified Data.Set as S

data Heuristic = Heuristic {
    nextLambda :: MineMap -> [Pos]
  , routeTo :: MineMap -> Pos -> [Route]
  }

runHeuristic :: MineMap -> Heuristic -> Route
runHeuristic m h = maximumBy (comparing value) $
                   runReader (run (walk m []) []) m
  where value r = score m r $ walkToEnd m r
        run sim r =
          let m' = simState sim
          in do let ls = take 3 $ nextLambda h m'
                    dests = if null ls then S.toList $ lifts m'
                            else take 3 ls
                    routes = concatMap (take 3 . routeTo h m') dests
                if simEnded sim || all null routes then return [r]
                else liftM (r:) $ liftM concat $ forM routes $ \route ->
                  run (sim `walkFrom` route) (r++route)

dumbHeuristic :: Heuristic
dumbHeuristic = Heuristic {
                  nextLambda = S.toList . lambdas
                , routeTo = \m p -> pathsTo m (robot m) p
                }

isWalkable :: MineMap -> Pos -> Bool
isWalkable m p = isEmpty m p || isEarth m p || isLambda m p || isOpenLift m p

move :: Pos -> Action -> Pos
move (x,y) MoveUp    = (x,y+1)
move (x,y) MoveDown  = (x,y-1)
move (x,y) MoveLeft  = (x-1,y)
move (x,y) MoveRight = (x+1,y)
move (x,y) Wait      = (x,y)

walkRoute :: Pos -> Route -> Pos
walkRoute = foldl move

repeatsSelf :: Route -> Bool
repeatsSelf r = length r > (S.size $ S.fromList $ scanl move (0,0) r)

pathsTo :: MineMap -> Pos -> Pos -> [Route]
pathsTo = genRoutes [[]]
    where genRoutes [] _ _ _ = [[Abort]] -- No walkable routes, abort
          genRoutes rs m s e =
            let newrs = filter (\r -> (isWalkable m $ walkRoute s r) && (not $ repeatsSelf r)) $
                        concat $ map (\r -> map (:r)
                        [MoveUp,MoveDown,MoveLeft,MoveRight]) rs
                targetrs = filter (\r -> e == walkRoute s r) newrs
            in map reverse targetrs ++ genRoutes newrs m s e
