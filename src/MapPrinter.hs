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
cellToChar (Lift Closed) = 'O'
cellToChar (Lift Open)   = 'L'
cellToChar Robot  = 'R'

chunks :: Int -> [a] -> [[a]]
chunks n = aux
  where
    aux xs = case splitAt n xs of
      (a, b) | null a -> []
             | otherwise -> a : aux b

printMap :: MineMap -> IO ()
printMap m = putStrLn charMap
  where
    (w, h) = mapBounds m
    charMap = concatMap (++"\n") . chunks w .  M.elems . M.map cellToChar . cells $ m

main :: IO ()
main = do
  case parseMap "######\n#. *R#\n#  \\.#\n#\\ * #\nL  .\\#\n######" of
    Left s -> print s
    Right m -> printMap m
