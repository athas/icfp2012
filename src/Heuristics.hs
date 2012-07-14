module Heuristics
  ( Heuristic(..)
  , runHeuristic
  , dumbHeuristic
  , pathTo
  )
  where

import MineMap
import Simulation

import Control.Monad
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
                   map fixProblem $ run $ stateFromMap m
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
fixProblem = removeUselessLoops

removeUselessLoops :: SimState -> SimState
removeUselessLoops s = trace ("Old score: " ++ (show $ score s) ++ "; Best score: " ++ (show $ score bestsim)) $
                       if score bestsim > score s then removeUselessLoops bestsim else bestsim
    where path     = reverse $ steps s
          starts   = stateFromMap $ origMap s
          visited  = map (robot . mineMap) $ scanl step starts path
          vismap   = foldl (\m (p,i) -> M.insert p (i : M.findWithDefault [] p m) m) M.empty $ zip visited [0..]
          loops    = filter (uncurry (/=)) $ concat $ map (\(_,i) -> liftM2 (,) i i) $ M.toList vismap
          newpaths = map (\(i,j) -> take i path ++ drop j path) loops
          sims     = s : map (walk starts) newpaths
          bestsim  = maximumBy (comparing score) sims

dumbHeuristic :: Heuristic
dumbHeuristic = Heuristic {
                  nextLambda = S.toList . lambdas
                , routeTo = \sim p -> [pathTo sim (robot $ mineMap sim) p]
                }

pathTo :: SimState -> Pos -> Pos -> SimState
pathTo sim from to = go (M.singleton from (sim, 0)) [(sim, 0)]
  where go :: M.Map Pos (SimState, Int) -> [(SimState, Int)] -> SimState
        go seen []     = maybe (sim `step` Abort) fst (M.lookup to seen)
        go seen ((sim',cost):ns) =
          let (seen', ns') = foldl check (seen, ns) $ neighbors sim' cost
          in go seen' ns'
        check (seen, ns) (sim',cost) =
          case M.lookup (robot $ mineMap sim') seen of
            Just (_, cost') | cost >= cost' -> (seen, ns)
            _ -> (M.insert (robot $ mineMap sim') (sim',cost) seen,
                  (sim',cost):ns)
        neighbors sim' cost = mapMaybe move' [MoveUp,MoveDown,MoveLeft,MoveRight]
          where move' a = case sim' `trystep` a of
                            Just sim'' | not (dead sim'') ->
                              Just (sim'', cost+stepcost (mineMap sim') (robot $ mineMap sim''))
                            _ -> Nothing
        stepcost sim' p = case getCell sim' p of
                            Empty -> 1
                            Earth -> (-3)
                            Rock  -> 40
                            Lift Open -> (-20)
                            _     -> 1
