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

board1 = [Point x y | x <- [0..9], y <- [0..5]]
board2 = [Point x y | x <- [0..5], y <- ([0..4]++[6..10])]
boardLong = [Point x y | x <- [0..19], y <- [0..2]]

main :: IO ()
main = do
  let board = board1

  let results =  solve board pieces
--  print (length results) -- outputs 2339
  print (results!!0)


  let charts = char board results
  print (charts!!0)
  
  mapM_ (mapM_ putStrLn) charts

