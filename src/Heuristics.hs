module Heuristics
  ( Heuristic(..)
  , runHeuristic
  , dumbHeuristic
  , pathTo
  )
  where

import MineMap
import Simulation

import Debug.Trace
import Data.Maybe
import Data.List
import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S

data Heuristic = Heuristic {
    nextLambda :: MineMap -> [Pos]
  , routeTo :: SimState -> Pos -> [SimState]
  }

runHeuristic :: MineMap -> Heuristic -> Route
runHeuristic m h = reverse $ steps $ maximumBy (comparing score) $
                   run $ stateFromMap m
  where run :: SimState -> [SimState]
        run sim =
          let m' = mineMap sim
              dests = case nextLambda h m' of
                        [] -> S.toList $ lifts m'
                        ls -> take 3 ls
              sims' = concatMap (take 3 . routeTo h sim) dests
          in trace ("trying lambdas " ++ show dests) $
             if finished sim || null sims' then [sim]
             else concatMap run sims'

fixProblem :: SimState -> SimState -- But better
fixProblem = id

dumbHeuristic :: Heuristic
dumbHeuristic = Heuristic {
                  nextLambda = S.toList . lambdas
                , routeTo = \sim p -> [pathTo sim (robot $ mineMap sim) p]
                }

pathTo :: SimState -> Pos -> Pos -> SimState
pathTo sim from to = go (M.singleton from (sim, 0)) [(sim, 0)]
  where m = mineMap sim
        go :: M.Map Pos (SimState, Int) -> [(SimState, Int)] -> SimState
        go seen []     = maybe (sim `step` Abort) fst (M.lookup to seen)
        go seen ((sim',cost):ns) =
          let (seen', ns') = foldl check (seen, ns) $ neighbors sim' cost
          in go seen' ns'
        check (seen, ns) (sim',cost) =
          case M.lookup (robot $ mineMap sim') seen of
            Just (_, cost') | cost >= cost' -> (seen, ns)
            _ -> (M.insert (robot $ mineMap sim') (sim',cost) seen,
                  (sim',cost):ns)
        neighbors sim' cost =
          filter worked $ map move' [MoveUp,MoveDown,MoveLeft,MoveRight]
          where move' a = (sim'', cost+stepcost (mineMap sim') (robot $ mineMap sim''))
                  where sim'' = sim' `step` a
                worked (new, _) = robot (mineMap new) /= robot (mineMap sim')
                                  && not (dead new)
        stepcost sim' p = case getCell sim' p of
                            Empty -> 1
                            Earth -> (-3)
                            Rock  -> 40
                            Lift Open -> (-20)
                            _     -> 1
