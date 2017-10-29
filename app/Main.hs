module Main where

import Lib
import Data.List

f = [Point 0 0, Point 0 1, Point 1 1, Point 1 2, Point 2 1]
i = [Point 0 0, Point 0 1, Point 0 2, Point 0 3, Point 0 4]
l = [Point 0 0, Point 0 1, Point 0 2, Point 0 3, Point 1 0]
n = [Point 0 0, Point 0 1, Point 1 1, Point 1 2, Point 1 3]
p = [Point 0 0, Point 0 1, Point 0 2, Point 1 1, Point 1 2]
t = [Point 0 0, Point 0 1, Point 0 2, Point 1 1, Point 2 1]
u = [Point 0 0, Point 1 0, Point 0 1, Point 0 2, Point 1 2]
v = [Point 0 0, Point 1 0, Point 2 0, Point 0 1, Point 0 2]
w = [Point 0 0, Point 0 1, Point 1 1, Point 1 2, Point 2 2]
x = [Point 0 0, Point 1 0, Point (-1) 0, Point 0 1, Point 0 (-1)]
y = [Point 0 0, Point 0 1, Point 0 2, Point 0 3, Point 1 1]
z = [Point 0 0, Point 1 0, Point 1 1, Point 1 2, Point 2 2]

pieces = [genSet f, genSet i, genSet l, genSet n, [align p, rot90 p], genSet t, genSet u, genSet v, genSet w, genSet x, genSet y, genSet z]

board1 = [Point x y | x <- [0..9], y <- [0..5]]
board2 = [Point x y | x <- [0..5], y <- ([0..4]++[6..10])]
boardLong = [Point x y | x <- [0..19], y <- [0..2]]

strip (Just x) = x
strip Nothing = " "

main :: IO ()
main = do
  let board = board2

  let results =  solve board pieces
--  print (length results) -- outputs 2339

  let resultsChar = map (concatMap (\a -> map (\b -> (b, snd a)) (fst a))) $ map (\a -> zip a ["a","b","c","d","e","f","g","h","i","j","k","l"]) results
  print (resultsChar!!0)

  let minx = minimum $ map (\(Point x y) -> x) board
  let maxx = maximum $ map (\(Point x y) -> x) board
  let miny = minimum $ map (\(Point x y) -> y) board
  let maxy = maximum $ map (\(Point x y) -> y) board

  let area = [Point x y | x <- [minx..maxx], y <- [miny..maxy]]


  let charts = map (map (foldl (++) "" ))$ map (\r -> map (\x -> map (\y -> strip $ lookup (Point x y) r) [miny..maxy]) [minx..maxx]) resultsChar
  print (charts!!0)
  
  mapM_ (mapM_ putStrLn) charts

