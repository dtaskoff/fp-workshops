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
