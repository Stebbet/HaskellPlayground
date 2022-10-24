-- Mimics launching a new thread for process b
par :: a -> b -> b
par x y = y

seqmax :: [Int] -> Int
seqmax [] = 0 --Ignore this
seqmax [x] = x
seqmax (x : xs)
  | x < seqmax xs = seqmax xs
  | otherwise = x

-- DIVIDE AND CONQUER MAX FUNCTION
dcmax :: [Int] -> Int
dcmax [] = 0
dcmax [x] = x
dcmax xs =
  max a b
  where
    mid = length xs `div` 2
    (as, bs) = splitAt mid xs
    a = dcmax as
    b = dcmax bs

-- PARALLEL DIVIDE AND CONQUER MAX FUNCTION
pdcmax :: [Int] -> Int
pdcmax [] = 0
pdcmax [x] = x
pdcmax xs =
  max c b
  where
    mid = length xs `div` 2
    (as, bs) = splitAt mid xs
    a = dcmax as
    b = dcmax bs
    c = par b a

-- SORT FUNCTION
seqsort :: [Int] -> [Int]
seqsort [] = []
seqsort (x : xs) =
  insert x (seqsort xs)

insert :: Int -> [Int] -> [Int]
insert e [] = [e]
insert e (x : xs)
  | e < x = e : x : xs
  | otherwise = x : insert e xs

-- PARTITION A LIST
part :: (Int -> Bool) -> [Int] -> ([Int], [Int])
part f [] = ([], [])
part f (x : xs)
  | f x = (x : as, bs)
  | otherwise = (as, x : bs)
  where
    (as, bs) = part f xs

-- QUICK SORT
qsort :: [Int] -> [Int]
qsort [] = []
qsort (x : xs) =
  qsort as ++ [x] ++ qsort bs
  where
    (as, bs) = part (< x) xs

-- PARALELL QUICK seqsort
pqsort :: [Int] -> [Int]
pqsort [] = []
pqsort (x : xs) =
  par qbs qas ++ [x] ++ qsort bs
  where
    qas = qsort as
    qbs = qsort bs
    (as, bs) = part (< x) xs

main = do
  print (pqsort [8, 6, 10, 1, 5, 32, 7, 2])
