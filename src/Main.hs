module Main(main) where

import Heuristics
import Simulation
import MapParser
import MineMap
import MapPrinter

import Control.Concurrent
import Data.Maybe
import System.Environment

solution :: MineMap -> Route
solution m = runHeuristic m dumbHeuristic

showSteps :: SimState -> Route -> IO SimState
showSteps sim [] = return sim
showSteps sim (a:as) = do putStr "\27[2J"
                          putStr "\27[H"
                          putStrLn $ printMap $ mineMap sim'
                          threadDelay 250000
                          showSteps sim' as
  where sim' = sim `step` a

main :: IO ()
main = do args <- getArgs
          case args of
            [f, p] -> do s <- readFile f
                         let route = fromMaybe (error "Invalid route") $ stringToRoute p
                         case parseMap s of
                           Left e -> error e
                           Right from -> do
                             putStrLn $ "From:\n" ++ printMap from
                             sim <- showSteps (stateFromMap from) route
                             let reason = stopReason sim
                             putStrLn $ "Stopped because of: " ++ show reason
                             putStrLn $ "Score: " ++ show (score sim)
            [] -> do s <- getContents
                     case parseMap s of
                       Left e -> error e
                       Right m -> putStrLn $ routeToString $ solution m
            _ -> error "Wrong parameters"
