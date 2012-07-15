module Heuristics
  ( Heuristic(..)
  , runHeuristic
  , searchOne
  , optimise
  , endNode
  , initSearch
  , simFromSearch
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

type SearchState = [Pos]

endNode :: SearchNode -> Bool
endNode = null . snd

initSearch :: Heuristic -> SimState -> SearchNode
initSearch h sim = (sim, nextLambda h $ mineMap sim)

simFromSearch :: SearchNode -> SimState
simFromSearch = fst

type SearchNode = (SimState, SearchState)

searchOne :: Heuristic -> SearchNode -> [SearchNode]
searchOne h (sim, dests) = search
  where search =
          let proceeded sim' = won sim' || collected sim' > collected sim
              sims = map (routeTo h sim) dests
          in if finished sim || null sims then [(sim, [])]
             else case filter proceeded sims of
                    []    -> [ (sim, drop 3 dests) ]
                    sims' -> map (initSearch h) sims'

runHeuristic :: MineMap -> Heuristic -> Route
runHeuristic m h = reverse $ steps $ optimise $ maximumBy (comparing score) $
                   run [initSearch h $ stateFromMap m]
  where run []     = []
        run (n:ns) | endNode n = simFromSearch n : run ns
                   | otherwise = simFromSearch n : run (searchOne h n ++ ns)

optimise :: SimState -> SimState -- But better
optimise s = fixWith fixes
    where
        fixWith []         = s
        fixWith ((f,l):fs) = let ns = f s
                             in  if   score ns <= score s
                                 then fixWith fs
                                 else trace (l ++ ": " ++ (show $ score s) ++ " --> " ++ (show $ score ns)) $
                                      ns

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
targets m = case ls of
    []    -> []
    ((d,_):_) -> let close = d + 2
                     approx x y
                       | fst x < close || fst y < close = False
                       | otherwise = True -- abs (fst x-fst y) < trsh
                 in map snd $ concatMap (take 1) $ groupBy approx ls
  where ls = sortBy (comparing fst) $ map (value &&& id) options
        options = S.toList (lambdas m)
                  ++ S.toList (razors m)
                  ++ filter (isWalkable m) (concatMap (cellNeighbors m) (S.toList $ hoRocks m))
                  ++ filter (isOpenLift m) (S.toList $ lifts m)
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
          | robot m == to = []
          | otherwise = mapMaybe move' [ApplyRazor,MoveUp,MoveDown,MoveLeft,MoveRight]
          where m = mineMap sim'
                move' ApplyRazor
                  | not (any (isBeard m) $ cellNeighbors m $ robot m) = Nothing
                move' a = case sim' `trystep` a of
                            Just sim'' | not (dead sim'') ->
                              Just (sim'', cost
                                           + stepcost m (robot $ mineMap sim'')
                                           - razorgain sim' sim'')
                            _ -> Nothing
        razorgain sim1 sim2 = S.size (beards $ mineMap sim1) - S.size (beards $ mineMap sim2)
        stepcost m p = case getCell m p of
                         Empty -> 1
                         Earth | belowRock m p -> 6
                               | (x > 1 && isRock m (x-1,y)) || (x < w && isRock m (x+1,y)) -> -6
                               | otherwise -> -1
                         Rock  -> 4000
                         Lambda | belowRock m p -> 5
                                | otherwise -> -3
                         Razor | belowRock m p -> 5
                               | otherwise -> -3
                         LambdaRock -> -6
                         _     -> 1
          where (x,y) = p
                (w,_) = mapBounds m

belowRock :: MineMap -> Pos -> Bool
belowRock m (x,y) = y < h && isNormalRock m (x,y+1)
  where (_,h) = mapBounds m
