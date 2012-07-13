module Main(main) where

import Simulation
import MapParser
import MineMap

import Data.Maybe
import System.Environment

solution :: MineMap -> Route
solution _ = [Abort]

main :: IO ()
main = do args <- getArgs
          case args of
            [f, p] -> do s <- readFile f
                         let route = fromMaybe (error "Invalid route") $ stringToRoute p
                         case parseMap s of
                           Left e -> error e
                           Right from -> do
                             putStrLn $ "From: " ++ show from
                             let (reason, to) = walkToEnd from route
                             putStrLn $ "To: " ++ show to
                             putStrLn $ "Stopped because of: " ++ show reason
                             putStrLn $ "Score: " ++ show (score from route (reason, to))
            [] -> do s <- getContents
                     case parseMap s of
                       Left e -> error e
                       Right m -> putStrLn $ routeToString $ solution m
            _ -> error "Wrong parameters"

