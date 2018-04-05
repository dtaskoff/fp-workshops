module Main where


main :: IO ()
main = do
  putStrLn "hello world"


factorial :: Word -> Word
factorial n
  | n < 2     = 1
  | otherwise = n * factorial (n - 1)

factorial' :: Word -> Word
factorial' 0 = 1
factorial' 1 = 1
factorial' n = n * factorial' (n - 1)

factorial'' :: Word -> Word
factorial'' n =
  case n of
    0 -> 1
    1 -> 1
    _ -> n * factorial'' (n - 1)

product' :: [Int] -> Int
product' (x:xs) = x * product' xs
product' [] = 1

product'' :: [Int] -> Int
product'' xs = go 1 xs
  where go acc [] = acc
        go acc (y:ys) = go (y * acc) ys
