module MapPrinter (printMap) where

import MineMap

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

printMap :: MineMap -> String
printMap m = unlines . reverse . zipWith addwater [1..] . chunks w $ chars
  where
    (w, h) = mapBounds m
    chars = [ cellToChar (getCell m (i, j)) | j <- [1..h], i <- [1..w] ]
    addwater i l | i <= water m = "\27[1;34m" ++ l ++ "\27[0m"
                 | otherwise = l
