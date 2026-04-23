module Calc where
import Text.Read (readEither)

data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

eval :: Expr -> Int
eval expr =
  case expr of
    Lit x -> x
    Add x y -> (eval x) + (eval y)
    Sub x y -> (eval x) - (eval y)
    Mul x y -> (eval x) * (eval y)
    Div x y -> (eval x) `div` (eval y)

parse :: String -> Either String Expr
parse str =
  case parse' (words str) of
    Left err -> Left err
    Right (e,[]) -> Right e
    Right (_,rest) -> Left $ "Found extra tokens: " <> (unwords rest)

parse' :: [String] -> Either String (Expr,[String])
parse' [] = Left "unexpected end of expression"
parse' (token:rest) =
  case token of
    "+" -> parseBinary Add rest
    "-" -> parseBinary Sub rest
    "*" -> parseBinary Mul rest
    "/" -> parseBinary Div rest
    lit ->
      case readEither lit of
        Left err -> Left err
        Right lit' -> Right (Lit lit', rest)

parseBinary :: (Expr -> Expr -> Expr) -> [String] -> Either String (Expr, [String])
parseBinary exprConstructor args =
  case parse' args of
    Left err -> Left err
    Right (firstArg,rest') ->
      case parse' rest' of
        Left err -> Left err
        Right (secondArg,rest'') ->
          Right $ (exprConstructor firstArg secondArg, rest'')

run :: String -> String
run expr =
  case parse expr of
    Left err -> "Error: " <> err
    Right expr' ->
      let
        answer = show $ eval expr'
      in
        "The answer is: " <> answer
