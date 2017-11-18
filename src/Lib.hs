module Lib
  ( Point(..)
    , Board(..)
    , genSet
    , rot90
    , align
    , solve
    , char
  ) where

import Data.List

data Point = Point Int Int deriving (Show, Eq, Ord)
data Board = Board [Point] deriving Show
type Piece = [Point]

unshift :: Point -> [Point] -> [Point]
unshift (Point xo yo) = map (\(Point x y) -> Point (x-xo) (y-yo))

shift :: Point -> [Point] -> [Point]
shift (Point xo yo) = map (\(Point x y) -> Point (x+xo) (y+yo))

genSet :: (Char, Piece) -> (Char, [Piece])
genSet (c, p) = (c, nub ps)
  where
    ps = [align p, rot90 p, rot180 p, rot270 p, mirror p, mirror90 p, mirror180 p, mirror270 p]

align :: [Point] -> [Point]
align p = sort $ unshift (minimum p) p

mirror :: [Point] -> [Point]
mirror p = align $ map (\(Point x y) -> Point (-x) y) p

rot90 :: [Point] -> [Point]
rot90 p = align $ map (\(Point x y) -> Point (-y) x) p

rot180 :: [Point] -> [Point]
rot180 p = align $ map (\(Point x y) -> Point (-x) (-y)) p

rot270 :: [Point] -> [Point]
rot270 p = align $ map (\(Point x y) -> Point y (-x)) p

mirror90 :: [Point] -> [Point]
mirror90 p = align $ map (\(Point x y) -> Point y x) p

mirror180 :: [Point] -> [Point]
mirror180 p = align $ map (\(Point x y) -> Point x (-y)) p

mirror270 :: [Point] -> [Point]
mirror270 p = align $ map (\(Point x y) -> Point (-y) (-x)) p

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

--

placeableset :: Board -> (Char, [Piece]) -> [(Char, [Piece])] -> [(Char,Piece,Board,[(Char,[Piece])])]
placeableset (Board []) _ _ = []
placeableset b p ps = map (\(po,bo) -> (fst p, po, bo, ps)) $ placeableset' b (snd p) 

placeableset' :: Board -> [Piece] -> [(Piece,Board)]
placeableset' (Board []) _ = []
placeableset' _ [] = []
placeableset' (Board b) (p:ps)
  | inc (shift (b!!0) p) b = (shift (b!!0) p, Board (sortedSub b (shift (b!!0) p))) : placeableset' (Board b) ps
  | otherwise = placeableset' (Board b) ps

solve :: Board -> [(Char, [Piece])] -> [[(Point, Char)]]
solve (Board bd) pss = map (concatMap (\a -> map (\b -> (b, fst a)) (snd a))) $ solve' (Board (sort bd)) pss

solve' :: Board -> [(Char, [Piece])] -> [[(Char, Piece)]]
solve' _ [] = [[]]
solve' (Board []) _ = [[]]
--solve' (Board bs) _ | not (checkDivide bs) = []
solve' board pieces = concatMap (\(c,p,b,ps) -> map ((c,p):) (solve' b ps)) $ concatMap (\(p,ps)-> (placeableset board p ps)) $ select pieces

--
strip :: Maybe Char -> Char
strip (Just x) = x
strip Nothing = ' '

char :: Board -> [[(Point, Char)]] -> [[[Char]]]
char (Board b) results = map (map (foldr (:) [])) $ map (\r -> map (\x -> map (\y -> strip $ lookup (Point x y) r) [miny..maxy]) [minx..maxx]) results
  where
    minx = minimum $ map (\(Point x y) -> x) b
    maxx = maximum $ map (\(Point x y) -> x) b
    miny = minimum $ map (\(Point x y) -> y) b
    maxy = maximum $ map (\(Point x y) -> y) b

--

nextTo :: Point -> Point -> Bool
nextTo (Point x1 y1) (Point x2 y2) = if not((x1+y1)-(x2+y2)==1 || (x2+y2)-(x1+y1)==1)
  then False
  else if not((x1-y1)-(x2-y2)==1 || (x2-y2)-(x1-y1)==1)
    then False
    else  True

nextToset :: [Point] -> Point -> Bool
nextToset set ele = foldr (\(p) -> (||) (nextTo ele p) ) False set

neighbors :: [Point] -> [Point] -> [Point]
neighbors set all = (\(b,f,r) -> b) $ neighbors' ([], set, all)

neighbors' :: ([Point],[Point],[Point]) -> ([Point],[Point],[Point])
-- neighbors' (back,front,rest) = (newback,newfront,newrest)
neighbors' (b,[],r) = (b,[],r)
neighbors' (b,f,[]) = (merge b f,[],[])
neighbors' (b,f,r) = neighbors' (merge b f, fl, sortedSub r fl)
  where
    fl = filter (\p -> nextToset f p) r
---neighbors' (b,f,r) = neighbors' (merge b f, sortedIntersect r fl, sortedSub r fl)
---  where
---    fl = nubSort $ concatMap (neighbors'') f

merge :: Ord a => [a] -> [a] -> [a]
merge [] a = a
merge a [] = a
merge (a:as) (b:bs)
  |a<b = a: (merge as (b:bs))
  |a>b = b: (merge (a:as) bs)

nubSort :: (Eq a, Ord a) => [a] -> [a]
nubSort [] = []
nubSort (x:xs) = nubSort lt ++ [x] ++ nubSort gt
  where
    lt = [l | l <- xs, l < x]
    gt = [g | g <- xs, g > x]

sortedIntersect :: (Eq a, Ord a) => [a] -> [a] -> [a]
sortedIntersect [] _ = []
sortedIntersect _ [] = []
sortedIntersect (a:as) (b:bs) 
  | a<b = sortedIntersect as (b:bs)
  | a>b = sortedIntersect (a:as) bs
  | otherwise = a : (sortedIntersect as bs)

sortedSub :: (Eq a, Ord a) => [a] -> [a] -> [a]
sortedSub [] _ = []
sortedSub l [] = l
sortedSub (a:as) (b:bs) 
  | a<b = a : (sortedSub as (b:bs))
  | a==b = sortedSub as bs
  | a>b = sortedSub (a:as) bs

neighbors'' :: Point -> [Point]
neighbors'' (Point x y) = [Point (x-1) y, Point (x+1) y, Point x (y-1), Point x (y+1)]

divide :: [Point] -> [[Point]]
divide [] = []
divide (b:bs) = seg : divide (sortedSub bs seg)
  where
    seg = neighbors [b] bs

checkDivide :: [Point] -> Bool
checkDivide b = foldl (&&) True $ map (\(x) -> mod (length x) 5 == 0) $ divide b

