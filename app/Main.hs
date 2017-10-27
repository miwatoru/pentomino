module Main where

import Lib

main :: IO ()
main = do
  let f = [Point 0 0, Point 0 1, Point 1 1, Point 1 2, Point 2 1]
  let i = [Point 0 0, Point 0 1, Point 0 2, Point 0 3, Point 0 4]
  let l = [Point 0 0, Point 0 1, Point 0 2, Point 0 3, Point 1 0]
  let n = [Point 0 0, Point 0 1, Point 1 1, Point 1 2, Point 1 3]
  let p = [Point 0 0, Point 0 1, Point 0 2, Point 1 1, Point 1 2]
  let t = [Point 0 0, Point 0 1, Point 0 2, Point 1 1, Point 2 1]
  let u = [Point 0 0, Point 1 0, Point 0 1, Point 0 2, Point 1 2]
  let v = [Point 0 0, Point 1 0, Point 2 0, Point 0 1, Point 0 2]
  let w = [Point 0 0, Point 0 1, Point 1 1, Point 1 2, Point 2 2]
  let x = [Point 0 0, Point 1 0, Point (-1) 0, Point 0 1, Point 0 (-1)]
  let y = [Point 0 0, Point 0 1, Point 0 2, Point 0 3, Point 1 1]
  let z = [Point 0 0, Point 1 0, Point 1 1, Point 1 2, Point 2 2]

  let pieces = [genSet f, genSet i, genSet l, genSet n, [align p, rot90 p], genSet t, genSet u, genSet v, genSet w, genSet x, genSet y, genSet z]

  let board = [Point x y | x <- [0..9], y <- [0..5]]
  let board2 = [Point x y | x <- [0..5], y <- ([0..4]++[6..10])]
  let boardLong = [Point x y | x <- [0..19], y <- [0..2]]

  let results =  solve board pieces
  print (length results) -- outputs 2339
