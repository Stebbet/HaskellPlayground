len :: [Int] -> Int
len [] =
  0
len (x : xs) =
  1 + len xs

contains :: [String] -> String -> Bool
contains [] e =
  False
contains (x : xs) e
  | e == x = True
  | otherwise = contains xs e

set :: [String] -> Bool
set [] =
  True
set (x : xs)
  | contains xs x = False
  | otherwise = set xs

largest :: [Int] -> Int
largest [x] =
  x
largest (x : xs)
  | x > largest xs = x
  | otherwise = largest xs

zipped :: ([String], [Int]) -> [(String, Int)]
zipped ([], []) =
  []
zipped (x : xs, y : ys) =
  (x, y) : zipped (xs, ys)

insert :: Int -> [Int] -> [Int]
insert e [] =
  [e]
insert e (x : xs)
  | e < x = e : x : xs
  | e > x = x : insert e xs

isLeapYear :: Int -> Bool
isLeapYear x
  | mod x 400 == 0 = True
  | mod x 100 == 0 = False
  | mod x 4 == 0 = True
  | otherwise = False

factorial :: Int -> Int
factorial 0 =
  1
factorial n =
  n * factorial (n -1)

fromTo :: Int -> Int -> [Int]
fromTo p q
  | p <= q = p : fromTo (p + 1) q
  | otherwise = []

main :: IO ()
main =
  print (factorial 5)
