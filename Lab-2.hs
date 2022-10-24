import Data.List

memberSet :: Int -> [Int] -> Bool
memberSet e [] =
  False
memberSet e (x : xs)
  | e `elem` (x : xs) = True
  | otherwise = False

makeSet :: [Int] -> [Int]
makeSet [] = []
makeSet (x : xs)
  | memberSet x xs = makeSet xs
  | otherwise = x : makeSet xs

unionSet :: [Int] -> [Int] -> [Int]
unionSet [_] [] = []
unionSet (_ : _ : _) [] = [] -- This is annoying
unionSet [] [] = []
unionSet [] (_ : _) = []
unionSet (x : xs) (y : ys) =
  makeSet c
  where
    c = (x : xs) ++ (y : ys)

intersectSet :: [Int] -> [Int] -> [Int]
intersectSet [] _ = []
intersectSet [_] [] = []
intersectSet (_ : _ : _) [] = []
intersectSet (x : xs) (y : ys)
  | memberSet x (y : ys) = x : intersectSet xs (y : ys)
  | otherwise = intersectSet xs (y : ys)

takeList :: Int -> [Int] -> [Int]
takeList e [] = []
takeList e (x : xs)
  | e == x = [x]
  | otherwise = x : takeList e xs

dropList :: Int -> [Int] -> [Int]
dropList e [] = []
dropList e (x : xs)
  | e == x = xs
  | otherwise = dropList e xs

split :: Int -> [Int] -> ([Int], [Int])
split _ [] = ([], [])
split e (x : xs) =
  (takeList e (x : xs), dropList e (x : xs))

main :: IO ()
main =
  print (split 4 [1, 2, 3, 4, 5, 6, 7, 8])
