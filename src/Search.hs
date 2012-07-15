module Search (timedSearch) where

import MineMap (MineMap, Route, Action(Abort))
import Simulation
import Heuristics

import Control.Concurrent

data Comm = Finished | Timeout | Solution SimState

timedSearch :: Int -> Heuristic -> MineMap -> IO Route
timedSearch t h m = do resvar <- newEmptyMVar
                       commvar <- newEmptyMVar
                       _ <- forkIO $ timer commvar t
                       _ <- forkIO $ generator commvar h m
                       _ <- forkIO $ accumulator commvar resvar
                       takeMVar resvar

timer :: MVar Comm -> Int -> IO ()
timer commvar t = do threadDelay $ 1000000 * t
                     putMVar commvar Timeout

generator :: MVar Comm -> Heuristic -> MineMap -> IO ()
generator commvar h m = run [initSearch h $ stateFromMap m]
  where run []     = putMVar commvar Finished
        run (n:ns) | endNode n = save n >> run ns
                   | otherwise = save n >> run (searchOne h n ++ ns)
        save = putMVar commvar . Solution . (`step` Abort) . simFromSearch

accumulator :: MVar Comm -> MVar Route -> IO ()
accumulator commvar resvar = do
  comm <- takeMVar commvar
  case comm of Timeout    -> error "Timeout before any solution was generated"
               Finished   -> error "No solutions"
               Solution s -> go s
    where go best = do
            comm <- takeMVar commvar
            case comm of Solution s -> do go $ if score s > score best
                                               then s else best
                         _          -> do putMVar resvar $ resRoute best

resRoute :: SimState -> Route
resRoute = reverse . steps -- . optimise
