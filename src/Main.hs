module Main where

import Text.Read (readMaybe)


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

member :: Eq a => a -> [a] -> Bool
member n (x:xs) = n == x || member n xs
member _ [] = False

-- an isogram is a word without a repeating letter
isogram :: String -> Bool
isogram (c:cs) = not (member c cs) && isogram cs
isogram _ = True

words' :: String -> [String]
words' "" = []
words' s  =
  case takeSymbols (skipSpaces s) of
    ("", rest) -> words' rest
    (taken, rest) -> taken : words' rest

takeSymbols :: String -> (String, String)
takeSymbols s = go "" s
  where go acc (' ':cs) = (acc, ' ':cs)
        go acc (c:cs)   = go (acc ++ [c]) cs
        go acc ""       = (acc, "")

skipSpaces :: String -> String
skipSpaces (' ':cs) = skipSpaces cs
skipSpaces s = s

data Expr = Val Int
          | Add Expr Expr
  deriving Show

parse :: String -> (Maybe Expr, [String])
parse s = parse' (words' s)

parse' :: [String] -> (Maybe Expr, [String])
parse' ("+":cs) =
  let (lexpr, rest) = parse' cs
      (rexpr, rest') = parse' rest
  in  case (lexpr, rexpr) of
        (Just l, Just r) -> (Just (Add l r), rest')
        _ -> (Nothing, "+":cs)
parse' (n:cs) =
  case readMaybe n of
    Just n' -> (Just (Val n'), cs)
    Nothing -> (Nothing, n:cs)
parse' other = (Nothing, other)

evaluate :: String -> Maybe Int
evaluate s =
  case parse s of
    (Just expr, []) -> Just (evaluate' expr)
    _ -> Nothing

evaluate' :: Expr -> Int
evaluate' (Val n) = n
evaluate' (Add lexpr rexpr) = evaluate' lexpr + evaluate' rexpr
