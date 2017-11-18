module Main where

import Lib
import Data.List

f = ('f', [Point 0 0, Point 0 1, Point 1 1, Point 1 2, Point 2 1])
i = ('i', [Point 0 0, Point 0 1, Point 0 2, Point 0 3, Point 0 4])
l = ('l', [Point 0 0, Point 0 1, Point 0 2, Point 0 3, Point 1 0])
n = ('n', [Point 0 0, Point 0 1, Point 1 1, Point 1 2, Point 1 3])
p = ('p', [Point 0 0, Point 0 1, Point 0 2, Point 1 1, Point 1 2])
t = ('t', [Point 0 0, Point 0 1, Point 0 2, Point 1 1, Point 2 1])
u = ('u', [Point 0 0, Point 1 0, Point 0 1, Point 0 2, Point 1 2])
v = ('v', [Point 0 0, Point 1 0, Point 2 0, Point 0 1, Point 0 2])
w = ('w', [Point 0 0, Point 0 1, Point 1 1, Point 1 2, Point 2 2])
x = ('x', [Point 0 0, Point 1 0, Point (-1) 0, Point 0 1, Point 0 (-1)])
y = ('y', [Point 0 0, Point 0 1, Point 0 2, Point 0 3, Point 1 1])
z = ('z', [Point 0 0, Point 1 0, Point 1 1, Point 1 2, Point 2 2])

pieces = [genSet f, genSet i, genSet l, genSet n, ('p',[align (snd p), rot90 (snd p)]), genSet t, genSet u, genSet v, genSet w, genSet x, genSet y, genSet z]

board1 = Board [Point x y | x <- [0..9], y <- [0..5]]
board1r = Board [Point x y | x <- [0..5], y <- [0..9]]
board2 = Board [Point x y | x <- [0..5], y <- ([0..4]++[6..10])]
boardLong = Board [Point x y | x <- [0..19], y <- [0..2]]
boardMesh = Board ([Point x y | x <- [0..10], y <- [0..6]] \\ [Point 0 0, Point 0 6, Point 1 2, Point 1 4, Point 3 1, Point 3 3, Point 3 5, Point 5 1, Point 5 3, Point 5 5, Point 7 1, Point 7 3, Point 7 5, Point 9 2, Point 9 4, Point 10 0, Point 10 6])

main :: IO ()
main = do
  let board = board1r

  let results =  solve board pieces
--  print (length results) -- outputs 2339

  let charts = char board results
  
  mapM_ (\(n,t) -> do
    putStrLn $ show n ++ " --"
    mapM_ putStrLn t) $ zip [0..] charts

