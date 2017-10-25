module Lib
    ( Point(..)
      , genSetP
      , rot90P
      , alignP
      , solve
    ) where

import Data.List

data Point = Point Int Int deriving (Show, Eq, Ord)

unshift :: Point -> Point -> Point
unshift (Point offsetx offsety) (Point x y) = Point (x-offsetx) (y-offsety)

unshiftP :: Point -> [Point] -> [Point]
unshiftP offset = map (unshift offset)

shift :: Point -> Point -> Point
shift (Point offsetx offsety) (Point x y) = Point (x+offsetx) (y+offsety)

shiftP :: Point -> [Point] -> [Point]
shiftP offset = map (shift offset)

cornerP :: [Point] -> Point
cornerP [p] = p
cornerP (p:ps) = if p < cornerP ps then p else cornerP ps

xx:: Point -> Int
xx (Point x y) = x

yy:: Point -> Int
yy (Point x y) = y

genSetP :: [Point] -> [[Point]]
genSetP p = nub ps 
  where
    ps = [alignP p, rot90P p, rot180P p, rot270P p, flipP p, flip90P p, flip180P p, flip270P p]

alignP :: [Point] -> [Point]
alignP p = sort $ unshiftP (cornerP p) p

flipP :: [Point] -> [Point]
flipP = alignP . flipP'

flipP' :: [Point] -> [Point]
flipP' = map (\p -> Point (-xx p) (yy p)) 

rot90P :: [Point] -> [Point]
rot90P = alignP . rot90P'

rot90P' :: [Point] -> [Point]
rot90P' = map (\p -> Point (-yy p) (xx p))

rot180P :: [Point] -> [Point]
rot180P = alignP . rot90P' . rot90P'

rot270P :: [Point] -> [Point]
rot270P = alignP . rot90P' . rot90P' . rot90P'

flip90P :: [Point] -> [Point]
flip90P = alignP . flipP' . rot90P'

flip180P :: [Point] -> [Point]
flip180P = alignP . flipP' . rot90P' . rot90P'

flip270P :: [Point] -> [Point]
flip270P = alignP . flipP' . rot90P' . rot90P' . rot90P'

--

select :: [a] -> [(a,[a])]
select [x] = [(x,[])]
select (x:xs) = (x,xs) : map (\(y,ys) -> (y,x:ys)) (select xs)

--

placeable :: [Point] -> [Point] -> ([Point],[Point])
placeable _ [] = ([],[])
placeable [] _ = ([],[])
placeable board p = if (shiftP (board!!0) p) \\ board == []
--                     then if checkDivide (board \\ (shiftP (board!!0) p))
                     then if True
                       then (shiftP (board!!0) p,board \\ (shiftP (board!!0) p))
                       else ([],[])
                     else ([],[])

placeableset :: [Point] -> [[Point]] -> [([Point],[Point])]
placeableset board pset = filter (/= ([],[])) $ map (placeable board) pset
-- placeableset board pset = map (placeable board) pset

solve :: [Point] -> [[[Point]]] -> [[[Point]]]
solve _ [] = [[]]
solve [] _ = [[]]
solve board pieces = concatMap (\(p,b,ps) -> map (p:) (solve b ps)) $ concatMap (\(p,ps)->(map (\(l) -> (fst l, snd l,ps))(placeableset board p))) $ select pieces

--

nextTo :: Point -> Point -> Bool
nextTo p1 p2 = if ((xx p1)-(xx p2)>1) || ((xx p2)-(xx p1)>1)
  then False
  else if ((yy p1)-(yy p2)>1) || ((yy p2)-(yy p1)>1)
    then False
    else  if ((xx p1)==(xx p2))||((yy p1)==(yy p2))
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

