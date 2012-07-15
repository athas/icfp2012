module Main(main) where

import Search
import Heuristics
import Simulation
import MapParser
import MineMap
import MapPrinter

import Control.Concurrent
import Data.Maybe
import System.Environment

solution :: MineMap -> IO Route
solution = interruptableSearch dumbHeuristic

showSteps :: SimState -> Route -> Route -> IO SimState
showSteps sim _ [] = return sim
showSteps sim orig (a:as) = do putStr "\27[2J"
                               putStr "\27[H"
                               putStrLn $ printMap $ mineMap sim'
                               putStrLn "Route:"
                               putStrLn $ routeToString orig
                               threadDelay 25000
                               showSteps sim' orig as
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
                             sim <- showSteps (stateFromMap from) route route
                             let reason = stopReason sim
                             putStrLn $ "Stopped because of: " ++ show reason
                             putStrLn $ "Score: " ++ show (score sim)
            [f] -> do s <- readFile f
                      case parseMap s of
                        Left e -> error e
                        Right from -> do
                          route <- solution from
                          putStrLn $ "From:\n" ++ printMap from
                          sim <- showSteps (stateFromMap from) route route
                          let reason = stopReason sim
                          putStrLn $ "Stopped because of: " ++ show reason
                          putStrLn $ "Score: " ++ show (score sim)
                          putStrLn $ "Used route: " ++ routeToString route
            [f, _, _] -> do s <- readFile f
                            case parseMap s of
                              Left e -> error e
                              Right from -> do
                                route <- solution from
                                putStrLn $ "From:\n" ++ printMap from
                                let sim = walk (stateFromMap from) route
                                let reason = stopReason sim
                                putStrLn $ "Stopped because of: " ++ show reason
                                putStrLn $ "Score: " ++ show (score sim)
                                putStrLn $ "Used route: " ++ routeToString route
            [f, _, _, _] -> do s <- readFile f
                               case parseMap s of
                                 Left e -> error e
                                 Right from -> do
                                   route <- solution from
                                   putStrLn $ "From:\n" ++ printMap from
                                   putStrLn $ "Route: " ++ routeToString route
            [] -> do s <- getContents
                     case parseMap s of
                       Left e -> error e
                       Right m -> (putStrLn . routeToString) =<< solution m
            _ -> error "Wrong parameters"
