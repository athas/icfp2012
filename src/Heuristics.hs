module Heuristics
  ( Heuristic(..)
  , runHeuristic
  , dumbHeuristic
  , pathTo
  )
  where

import MineMap
import Simulation

import Control.Arrow
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
          in run' sim $ take 3 dests
        run' sim dests = let proceeded sim' = won sim' || lleft sim' < lleft sim
                             sims = map (routeTo h sim) $ take 3 dests
                         in trace ("trying lambdas " ++ show dests) $
                            if finished sim || null sims then [sim]
                            else case filter proceeded sims of
                                   [] -> run' sim $ drop 3 dests
                                   sims' -> concatMap run $ take 2 sims'
        lleft = S.size . lambdas . mineMap


fixProblem :: SimState -> SimState -- But better
fixProblem s = fixWith fixes
    where
        fixWith []         = s
        fixWith ((f,l):fs) = let ns = f s
                             in  if   score ns <= score s
                                 then fixWith fs
                                 else trace (l ++ ": " ++ (show $ score s) ++ " --> " ++ (show $ score ns)) $
                                      fixProblem ns

        fixes = [ (rul0, "rul0"), (rul1, "rul1"), (rul2, "rul2") ]

        rul0 = removeUselessLoops [[]]
        rul1 = removeUselessLoops $ map return dirs
        rul2 = removeUselessLoops $ filter (not . cancelOut) $ liftM2 (\x y -> [x,y]) dirs dirs

        dirs = [MoveLeft, MoveRight, MoveUp, MoveDown]
        cancelOut r = foldl move (0,0) r == (0,0)

removeUselessLoops :: [Route] -> SimState -> SimState
removeUselessLoops dirx s = if   score bestsim > score s
                            then removeUselessLoops dirx bestsim
                            else bestsim
    where path     = reverse $ steps s
          starts   = stateFromMap $ origMap s
          visited  = map (robot . mineMap) $ drop 1 $ scanl step starts path
          vismap   = foldl (\m (p, i) -> M.insertWith (++) p [i] m) M.empty $ zip visited [0..]

          pis = concat $ map (\(p, is) -> zip (repeat p) is) $ M.toList vismap
          pid = [ (p, ds, i, j) | (p, i) <- pis
                                , ds <- dirx
                                , let q = foldl move p ds
                                , j <- M.findWithDefault [] q vismap
                                , j > i + 1 ]

          toPath (p, ds, i, j) = take i path ++ ds ++ drop (j - 1) path
          newpaths = map toPath pid
          sims     = s : map (walk starts) newpaths
          bestsim  = maximumBy (comparing score) sims

dumbHeuristic :: Heuristic
dumbHeuristic = Heuristic {
                  nextLambda = targets
                , routeTo = \sim -> pathTo sim (robot $ mineMap sim)
                }


targets :: MineMap -> [Pos]
targets m =
  case ls of
    []    -> []
    ((d,_):_) -> let close = d + 2
                     approx x y
                       | fst x < close || fst y < close = False
                       | otherwise = True -- abs (fst x-fst y) < trsh
                 in map snd $ concatMap (take 1) $ groupBy approx ls
  where ls = sortBy (comparing fst) $ map (value &&& id) $ S.toList $ lambdas m
        value p = dist p
        dist = distTo (robot m)
        distTo (x1,y1) (x2,y2) = abs (x2-x1) + abs (y2-y1)
        trsh = uncurry max (mapBounds m) `div` 2

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
                         Earth | belowRock m p -> 6
                               | (x > 1 && isRock m (x-1,y)) || (x < w && isRock m (x+1,y)) -> -6
                               | otherwise -> -1
                         Rock  -> 4000
                         Lambda | belowRock m p -> 5
                                | otherwise -> -3
                         _     -> 1
          where (x,y) = p
                (w,_) = mapBounds m

belowRock :: MineMap -> Pos -> Bool
belowRock m (x,y) = y < h && isRock m (x,y+1)
  where (_,h) = mapBounds m
