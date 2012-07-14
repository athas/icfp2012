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
  , routeTo :: SimState -> Pos -> SimState
  }

runHeuristic :: MineMap -> Heuristic -> Route
runHeuristic m h = reverse $ steps $ fixProblem $ maximumBy (comparing score) $
                   run $ stateFromMap m
  where run :: SimState -> [SimState]
        run sim =
          let m' = mineMap sim
              dests = case nextLambda h m' of
                        [] -> S.toList $ lifts m'
                        ls -> ls
          in run' sim dests
        run' sim dests = let proceeded sim' = length (steps sim') > length (steps sim) + 10
                             sims' = map (routeTo h sim) $ take 3 dests
                         in trace ("trying lambdas " ++ show dests) $
                            if finished sim || null sims' then [sim]
                            else if any proceeded sims'
                                 then concatMap run sims'
                                 else run' sim (drop 3 dests)


fixProblem :: SimState -> SimState -- But better
fixProblem = removeUselessLoops2 . removeUselessLoops


removeUselessLoops2 :: SimState -> SimState
removeUselessLoops2 s = trace ("rul2: Old score: " ++ (show $ score s) ++
                               "; Best score: " ++ (show $ score bestsim)) $
                       if score bestsim > score s
                       then fixProblem bestsim
                       else bestsim
    where path     = reverse $ steps s
          starts   = stateFromMap $ origMap s
          visited  = map (robot . mineMap) $ drop 1 $ scanl step starts path
          vismap   = foldl (\m (p, i) -> M.insertWith (++) p [i] m) M.empty $ zip visited [0..]

          pis = concat $ map (\(p, is) -> zip (repeat p) is) $ M.toList vismap
          pid = [ (p, dir, i, j) | (p, i) <- pis
                                 , dir <- dirs
                                 , let q = move p dir
                                 , j <- M.findWithDefault [] q vismap
                                 , j > i + 1 ]

          toPath (p, dir, i, j) = take i path ++ [dir] ++ drop (j - 1) path
          newpaths = map toPath pid
          sims     = s : map (walk starts) newpaths
          bestsim  = maximumBy (comparing score) sims

          dirs = [MoveLeft, MoveRight, MoveUp, MoveDown]


removeUselessLoops :: SimState -> SimState
removeUselessLoops s = trace ("rul: Old score: " ++ (show $ score s) ++ "; Best score: " ++ (show $ score bestsim)) $
                       if score bestsim > score s then removeUselessLoops bestsim else bestsim
    where path     = reverse $ steps s
          starts   = stateFromMap $ origMap s
          visited  = map (robot . mineMap) $ drop 1 $ scanl step starts path
          vismap   = foldl (\m (p,i) -> M.insertWith (++) p [i] m) M.empty $ zip visited [0..]
          loops    = filter (uncurry (/=)) $ concat $ map (\(_,i) -> liftM2 (,) i i) $ M.toList vismap
          newpaths = map (\(i,j) -> take i path ++ drop j path) loops
          sims     = s : map (walk starts) newpaths
          bestsim  = maximumBy (comparing score) sims


dumbHeuristic :: Heuristic
dumbHeuristic = Heuristic {
                  nextLambda = \m -> sortBy (comparing (distTo $ robot m)) $ S.toList $ lambdas m
                , routeTo = \sim -> pathTo sim (robot $ mineMap sim)
                }
  where distTo (x1,y1) (x2,y2) = (x2-x1) + (y2-y1)

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
        neighbors sim' cost
          | robot (mineMap sim') == to = []
          | otherwise = mapMaybe move' [MoveUp,MoveDown,MoveLeft,MoveRight]
          where move' a = case sim' `trystep` a of
                            Just sim'' | not (dead sim'') ->
                              Just (sim'', cost+stepcost (mineMap sim') (robot $ mineMap sim''))
                            _ -> Nothing
        stepcost m p = case getCell m p of
                         Empty -> 1
                         Earth -> if y < h && isRock m (x,y+1)
                                  then 6
                                  else if (x > 1 && isRock m (x-1,y)) || (x < w && isRock m (x+1,y))
                                       then -3 else -1
                         Rock  -> 4000
                         Lambda -> -3
                         _     -> 1
          where (x,y) = p
                (w,h) = mapBounds m
