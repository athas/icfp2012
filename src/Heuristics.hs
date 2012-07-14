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
  , routeTo :: MineMap -> Pos -> [Route]
  }

runHeuristic :: MineMap -> Heuristic -> Route
runHeuristic m h = reverse $ steps $ maximumBy (comparing score) $
                   concat $ run $ stateFromMap m
  where run sim =
          let m' = mineMap sim
              dests = case nextLambda h m' of
                        [] -> S.toList $ lifts m'
                        ls -> take 3 ls
              routes = concatMap (take 3 . routeTo h m') dests
          in if finished sim || all null routes then return [sim]
             else concatMap (run . walk sim) routes

dumbHeuristic :: Heuristic
dumbHeuristic = Heuristic {
                  nextLambda = S.toList . lambdas
                , routeTo = \m p -> [pathTo m (robot m) p]
                }

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

pathTo :: MineMap -> Pos -> Pos -> Route
pathTo m from to = reverse $ go (M.singleton from []) [(from, [])]
  where go seen []     = fromMaybe [] (M.lookup to seen)
        go seen ((p,path):ns) =
          let (seen', ns') = foldl check (seen, ns) $ neighbors p path
          in go seen' ns'
        check (seen, ns) (p,path) = case M.lookup p seen of
                                      Nothing -> (M.insert p path seen, (p,path):ns)
                                      Just path' | length path >= length path' -> (seen, ns)
                                                 | otherwise -> (M.insert p path seen, (p,path):ns)
        neighbors p path = filter (isWalkable m . fst) $
                           map (\a -> (move p a, a:path))
                           [MoveUp,MoveDown,MoveLeft,MoveRight]
