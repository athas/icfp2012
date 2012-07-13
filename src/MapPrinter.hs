module MapPrinter where

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

-- printMap :: MineMap -> IO ()
-- printMap m = putStrLn charMap
--   where
--     (w, h) = mapBounds m
--     charMap = concatMap (++"\n") . chunks w .  M.elems . M.map cellToChar . cells $ m

printMap m = putStrLn $ concatMap (++ "\n") . chunks w $ chars
  where
    (w, h) = mapBounds m
    chars = [ cellToChar (getCell m (i, j)) | i <- [1..w], j <- [1..h] ]

Right foo = parseMap "######\n#. *R#\n#  \\.#\n#\\ * #\nL  .\\#\n######"
