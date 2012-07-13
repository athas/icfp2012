module MapPrinter (printMap) where

import qualified Data.Text as T
import qualified Data.Map as M
import MineMap
import MapParser

cellToChar :: Cell -> Char
cellToChar Empty  = ' '
cellToChar Rock   = '*'
cellToChar Wall   = '#'
cellToChar Lambda = '\\'
cellToChar Earth  = '.'
cellToChar (Lift Closed) = 'L'
cellToChar (Lift Open)   = 'O'
cellToChar Robot  = 'R'

chunks :: Int -> [a] -> [[a]]
chunks n = aux
  where
    aux xs = case splitAt n xs of
      (a, b) | null a -> []
             | otherwise -> a : aux b

printMap m = putStrLn $ concatMap (++ "\n") . reverse . chunks w $ chars
  where
    (w, h) = mapBounds m
    chars = [ cellToChar (getCell m (i, j)) | j <- [1..h], i <- [1..w] ]
