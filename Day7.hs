import Data.Char

main :: IO ()
main = do
  contents <- readFile "7.txt"
  print $ sum $ map fst $ filter (uncurry check1) $ map parse $ lines contents
  print $ sum $ map fst $ filter (uncurry check2) $ map parse $ lines contents

parse :: String -> (Int, [Int])
parse xs =
  let ys = takeWhile isDigit xs
      zs = drop (length ys + 2) xs
      n = read ys
      ms = map read $ words zs
   in (n, ms)

check1 :: Int -> [Int] -> Bool
check1 x (y : ys) = check' y ys
  where
    check' acc [] = x == acc
    check' acc (z : zs) =
      check' (acc + z) zs || check' (acc * z) zs

check2 :: Int -> [Int] -> Bool
check2 x (y : ys) = check' y ys
  where
    check' acc [] = x == acc
    check' acc (z : zs) =
      check' (acc + z) zs || check' (acc * z) zs || check' (append acc z) zs

append :: Int -> Int -> Int
append x 0 = x
append x y = append x (y `div` 10) * 10 + y `mod` 10
