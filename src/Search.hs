module Search (interruptableSearch) where

import MineMap (MineMap, Route, Action(Abort))
import Simulation
import Heuristics

import Control.Concurrent
import Control.Monad

import System.IO
import System.Posix.Signals

import Prelude hiding (catch)

data Comm = Finished | Timeout | Solution SimState

interruptableSearch :: Heuristic -> MineMap -> IO Route
interruptableSearch h m = do
  resvar <- newEmptyMVar
  commvar <- newEmptyMVar
  let intHandler = do hPutStrLn stderr "Received SIGINT, moving to optimisatsion"
                      putMVar commvar Timeout
  _ <- installHandler sigINT (CatchOnce intHandler) Nothing
  _ <- forkIO $ generator commvar h m
  _ <- forkIO $ accumulator commvar resvar
  takeMVar resvar

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
            case comm of Solution s -> go $ if score s > score best
                                            then s else best
                         _          -> putMVar resvar =<< resRoute best

resRoute :: SimState -> IO Route
resRoute = (return . reverse . steps) <=< optimiseFor 10

optimiseFor :: Int -> SimState -> IO SimState
optimiseFor t sim = do
  commvar <- newEmptyMVar
  resvar <- newEmptyMVar
  let buffer sim' = do comm <- takeMVar commvar
                       case comm of Just sim'' -> report sim' sim'' >> buffer sim''
                                    Nothing    -> putMVar resvar sim'
  _ <- forkIO $ threadDelay (t*1000000) >> putMVar commvar Nothing
  _ <- forkIO $ buffer sim
  _ <- forkIO $ do mapM_ (putMVar commvar . Just)
                           $ whileBetter (score sim)
                           $ drop 1 $ iterate optimise sim
                   putMVar commvar Nothing
  takeMVar resvar
    where report from to = hPutStrLn stderr $
                           "Went from "
                           ++ show (score from)
                           ++ " to "
                           ++ show (score to)
          whileBetter cur (new:xs)
            | score new > cur = new : whileBetter (score new) xs
          whileBetter _ _ = []
