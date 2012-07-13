module Main(main) where

import Heuristics
import Simulation
import MapParser
import MineMap
import MapPrinter

import System.IO
import qualified Data.Set as S
import Data.Maybe
import System.Environment

solution :: MineMap -> Route
solution m = runHeuristic m dumbHeuristic

showSteps :: Simulation -> Route -> IO Simulation
showSteps sim [] = return sim
showSteps m (a:as) = do putStrLn $ printMap $ simState m'
                        showSteps m' as
  where m' = walkFrom m [a]

main :: IO ()
main = do args <- getArgs
          case args of
            [f, p] -> do s <- readFile f
                         let route = fromMaybe (error "Invalid route") $ stringToRoute p
                         case parseMap s of
                           Left e -> error e
                           Right from -> do
                             putStrLn $ "From:\n" ++ printMap from
                             _ <- showSteps (walk from []) route
                             let (reason, to) = walkToEnd from route
                             putStrLn $ "Stopped because of: " ++ show reason
                             putStrLn $ "Score: " ++ show (score from route (reason, to))
            [] -> do s <- getContents
                     case parseMap s of
                       Left e -> error e
                       Right m -> putStrLn $ routeToString $ solution m
            _ -> error "Wrong parameters"
