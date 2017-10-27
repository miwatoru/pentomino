module Lib
    ( Point(..)
      , genSet
      , rot90
      , align
      , solve
    ) where

import Data.List

data Point = Point Int Int deriving (Show, Eq, Ord)

unshift :: Point -> [Point] -> [Point]
unshift (Point xo yo) = map (\(Point x y) -> Point (x-xo) (y-yo))

shift :: Point -> [Point] -> [Point]
shift (Point xo yo) = map (\(Point x y) -> Point (x+xo) (y+yo))

genSet :: [Point] -> [[Point]]
genSet p = nub ps 
  where
    ps = [align p, rot90 p, rot180 p, rot270 p, mirror p, mirror90 p, mirror180 p, mirror270 p]

align :: [Point] -> [Point]
align p = sort $ unshift (minimum p) p

mirror :: [Point] -> [Point]
mirror = align . mirror'

mirror' :: [Point] -> [Point]
mirror' = map (\(Point x y) -> Point (-x) y) 

rot90 :: [Point] -> [Point]
rot90 = align . rot90'

rot90' :: [Point] -> [Point]
rot90' = map (\(Point x y) -> Point (-y) x)

rot180 :: [Point] -> [Point]
rot180 = align . rot90' . rot90'

rot270 :: [Point] -> [Point]
rot270 = align . rot90' . rot90' . rot90'

mirror90 :: [Point] -> [Point]
mirror90 = align . mirror' . rot90'

mirror180 :: [Point] -> [Point]
mirror180 = align . mirror' . rot90' . rot90'

mirror270 :: [Point] -> [Point]
mirror270 = align . mirror' . rot90' . rot90' . rot90'

--

select :: [a] -> [(a,[a])]
select [x] = [(x,[])]
select (x:xs) = (x,xs) : map (\(y,ys) -> (y,x:ys)) (select xs)

--

inc :: [Point] -> [Point] -> Bool
inc [] _ = True
inc _ [] = False
inc (p:ps) (b:bs) 
  | p == b = inc ps bs
  | p < b = False
  | p > b = inc (p:ps) bs

except :: [Point] -> [Point] -> [Point]
except b [] = b
except [] _ = []
except (b:bs) (p:ps)
  | b == p = except bs ps
  | b < p = b : except bs (p:ps)
  | b > p = except (b:bs) ps

placeable :: [Point] -> [Point] -> ([Point],[Point])
placeable _ [] = ([],[])
placeable [] _ = ([],[])
placeable board p = if inc (shift (board!!0) p) board
--                     then if checkDivide (except board (shift (board!!0) p))
                     then if True
                       then (shift (board!!0) p, except board (shift (board!!0) p))
                       else ([],[])
                     else ([],[])

placeableset :: [Point] -> [[Point]] -> [([Point],[Point])]
placeableset board pset = filter (/= ([],[])) $ map (placeable board) pset
-- placeableset board pset = map (placeable board) pset

solve :: [Point] -> [[[Point]]] -> [[[Point]]]
solve board = solve' (sort board)

solve' :: [Point] -> [[[Point]]] -> [[[Point]]]
solve' _ [] = [[]]
solve' [] _ = [[]]
solve' board pieces = concatMap (\(p,b,ps) -> map (p:) (solve' b ps)) $ concatMap (\(p,ps)->(map (\(l) -> (fst l, snd l,ps))(placeableset board p))) $ select pieces

--

nextTo :: Point -> Point -> Bool
nextTo (Point x1 y1) (Point x2 y2) = if (x1-x2>1) || (x2-x1>1)
  then False
  else if (y1-y2>1) || (y2-y1>1)
    then False
    else  if (x1==x2)||(y1==y2)
      then True
      else False

nextToset :: [Point] -> Point -> Bool
nextToset set ele = foldr (\(p) -> (||) (nextTo ele p) ) False set

neighbors :: [Point] -> [Point] -> [Point]
neighbors set all = if length (filter (nextToset set) all)==0
  then set
  else neighbors (set ++ filter (nextToset set) all)  (all \\ (set ++ filter (nextToset set) all))

divide :: [Point] -> [[Point]]
divide [] = []
divide b = neighbors [b!!0] b : divide (b \\ (neighbors [b!!0] b))

checkDivide :: [Point] -> Bool
checkDivide b = foldr (&&) True $ map (\(x) -> mod (length x) 5 == 0) $ divide b

--

