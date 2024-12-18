import Data.List

main :: IO ()
main = do
  contents <- readFile "2.txt"
  print $ length $ filter (check1 . map read . words) $ lines contents
  print $ length $ filter (check2 . map read . words) $ lines contents

check1 :: [Int] -> Bool
check1 xs =
  let ds = zipWith (-) (drop 1 xs) xs
   in all (\d -> d < 0 && d > (-4)) ds || all (\d -> d > 0 && d < 4) ds

check2 :: [Int] -> Bool
check2 xs = any check1 $ xs : withoutOne xs

withoutOne :: [Int] -> [[Int]]
withoutOne [] = []
withoutOne (x : xs) = xs : map (x :) (withoutOne xs)
