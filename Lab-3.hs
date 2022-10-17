appList :: [Int] -> [Int] -> [Int]
appList x y = x ++ y

appListPoly :: [a] -> [a] -> [a]
appListPoly x y = x ++ y

appListFoldr :: [a] -> [a] -> [a]
appListFoldr [] [] = []
appListFoldr x y = foldr (++) [] [x, y]

concatList :: [[Int]] -> [Int]
concatList [] = []
concatList (x : xs) =
  x ++ concatList xs

concatListPoly :: [[a]] -> [a]
concatListPoly [] = []
concatListPoly (x : xs) =
  x ++ concatListPoly xs

concatListFoldr :: [[a]] -> [a]
concatListFoldr x =
  foldr (++) [] x

triangles = map (\x -> round(x * (x+1)/2) :: Int) [1..] 
trianglesComp = [ (\x -> round(x * (x+1) / 2) :: Int) y | y <- [1..] ]

main :: IO ()
main =
  print (take 20 trianglesComp)
