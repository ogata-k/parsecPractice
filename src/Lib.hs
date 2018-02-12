module Lib(
      CalcAST(..)
    , calc
    , parseCalcAST
    , mainParser
    )where

import Data.Ratio
import Text.Parsec (parse, ParseError, try, parseTest)
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Prim ((<|>))
import Text.Parsec.Combinator

data CalcAST =
          Number Rational
        | BinaryOp String CalcAST CalcAST
        | UnaryOp String CalcAST
        | Paren CalcAST
        deriving (Show)
bop0Map = [
      ("*", (*))
    , ("/", (/))
    ]
bop1Map = [
      ("+", (+))
    , ("-", (-))
    ]
uopMap = [
      ("-", ((-) 0))
    , ("abs", abs)
    , ("sign", signum)
    ]
-- 計算用関数
convertBinaryOp :: [(String, Rational -> Rational -> Rational)]
                -> String
                -> Maybe (Rational -> Rational -> Rational)
convertBinaryOp bopMap s = lookup s bopMap
convertUnaryOp :: [(String, Rational -> Rational)]
                -> String
                -> Maybe (Rational -> Rational)
convertUnaryOp uopMap s = lookup s uopMap

calc :: CalcAST -> Either String Rational
calc (Number a) = Right a
calc (BinaryOp bop expr1 expr2) = case (bop', e1, e2) of
            (Just op, Right e1', Right e2') -> Right $ op e1' e2'
            otherwise -> Left "can't calc binary operator"
    where
        e1 = calc expr1
        e2 = calc expr2
        bop' = convertBinaryOp (bop0Map ++ bop1Map) bop
calc (UnaryOp uop expr ) = case (uop', e) of
            (Just op, Right e') -> Right $ op e'
            otherwise -> Left "can't calc unary operator"
    where
        e = calc expr
        uop' = convertUnaryOp uopMap uop
calc (Paren expr) = calc expr

-- パース用関数
parseCalcAST :: Parser CalcAST -> String -> Either ParseError CalcAST
parseCalcAST p s = parse p s s

-- 数字用
ratio :: Parser Rational
ratio = do
    xs <- many1 digit
    ys <-  optionMaybe (char '.' *> many1 digit)
    return $ convert xs ys
    where
        convert :: String -> Maybe String -> Rational
        convert xs Nothing = toRational $ (read :: String -> Integer) xs
        convert xs (Just ys) = (convert xs Nothing) + (toRational $ (read :: String -> Double) $ "0." ++ ys)

num :: Parser CalcAST
num = Number <$> ratio <* spaces
-- 演算子用

symbol :: String -> Parser String
symbol xs = do
    res <- string xs
    spaces
    return res

paren :: Parser CalcAST
paren = do
    symbol "("
    res <- expr
    symbol ")"
    return $ Paren res
unary :: String -> Parser (CalcAST -> CalcAST)
unary s = do
        symbol s
        return $ UnaryOp s

binary :: String -> Parser (CalcAST -> CalcAST -> CalcAST)
binary s = do
        symbol s
        return $ BinaryOp s

term :: Parser CalcAST
term = try paren <|> num <|> withUop
    where
        uops = map unary (map fst uopMap)
        withUop = do
            uop <- choice uops
            n <- term
            return $ uop n

expr :: Parser CalcAST
expr = term `chainl1` (choice bop0) `chainl1` (choice bop1)
    where
        bop0 = map binary (map fst bop0Map)
        bop1 = map binary (map fst bop1Map)
        
mainParser = expr <* eof
