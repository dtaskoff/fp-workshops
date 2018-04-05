module WorkshopOne.Expr where

import Text.Read (readMaybe)

import WorkshopOne.WarmUp (member, words')


data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
  deriving Show

parse :: String -> (Maybe Expr, [String])
parse s = parse' (words' s)

parse' :: [String] -> (Maybe Expr, [String])
parse' (op:cs) | op `member` ["+", "-", "*"] =
  let (lexpr, rest) = parse' cs
      (rexpr, rest') = parse' rest
  in  case (lexpr, rexpr) of
        (Just l, Just r) -> (opOp op l r, rest')
        _ -> (Nothing, op:cs)
parse' (n:cs) =
  case readMaybe n of
    Just n' -> (Just (Val n'), cs)
    Nothing -> (Nothing, n:cs)
parse' other = (Nothing, other)

opOp :: String -> Expr -> Expr -> Maybe Expr
opOp "+" l r = Just (Add l r)
opOp "-" l r = Just (Sub l r)
opOp "*" l r = Just (Mul l r)
opOp _ _ _ = Nothing

evaluate :: String -> Maybe Int
evaluate s =
  case parse s of
    (Just expr, []) -> Just (evaluate' expr)
    _ -> Nothing

evaluate' :: Expr -> Int
evaluate' (Val n) = n
evaluate' (Add lexpr rexpr) = evaluate' lexpr + evaluate' rexpr
evaluate' (Sub lexpr rexpr) = evaluate' lexpr - evaluate' rexpr
evaluate' (Mul lexpr rexpr) = evaluate' lexpr * evaluate' rexpr
