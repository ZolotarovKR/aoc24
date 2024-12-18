import Data.Char
import Data.List

main :: IO ()
main = do
  contents <- readFile "1.txt"
  let (xs, ys) = unzip $ map parse $ lines contents
  let as :: [Int] = sort xs
      bs :: [Int] = sort ys
  print $ sum $ map abs $ zipWith (-) as bs
  print $ sum $ map (\x -> x * count x ys) xs

parse :: String -> (Int, Int)
parse xs =
  let ys = takeWhile isDigit xs
      zs = drop (length ys + 3) xs
      n = read ys
      m = read zs
   in (n, m)

count :: (Eq a) => a -> [a] -> Int
count x = foldl (\acc y -> acc + if x == y then 1 else 0) 0
