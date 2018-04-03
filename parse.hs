import Data.Char
import Data.List

data E
  = Num Int
  | Soma E E
  | Mult E E
  deriving (Show, Eq)

data Token
  = Num_T Int
  | OPar_T
  | CPar_T
  | Soma_T
  | Mult_T
  deriving (Show, Eq)

interpretExp :: E -> Int
interpretExp (Num n) = n
interpretExp (Soma e1 e2) = (interpretExp e1) + (interpretExp e2)
interpretExp (Mult e1 e2) = (interpretExp e1) * (interpretExp e2)

parseExp :: String -> E
parseExp = parseExpAux . getTokens

parseExpAux :: [Token] -> E
parseExpAux [Num_T n] = Num n
parseExpAux (OPar_T:ts) = parseExpAux (getInnerExp ts)
parseExpAux list@(Num_T n:Mult_T:ts) = Mult (Num n) (parseExpAux ts)
parseExpAux list@(Num_T n:Soma_T:ts) = Soma (Num n) (parseExpAux ts)

getInnerExp :: [Token] -> [Token]
getInnerExp = getInnerExpAux 0
  where
    getInnerExpAux 0 (CPar_T:ts) = []
    getInnerExpAux 0 (OPar_T:ts) = OPar_T:(getInnerExpAux 1     ts)
    getInnerExpAux n (CPar_T:ts) = CPar_T:(getInnerExpAux (n-1) ts)
    getInnerExpAux n (t     :ts) = t     :(getInnerExpAux n     ts)

findToken :: (Eq a) => [a] -> a -> ([a],[a])
findToken l t = case elemIndex t l of
  Just n -> splitAt n l

getTokens :: String -> [Token]
getTokens "" = []
getTokens list@(c:cs)
  | c == ' '  = getTokens cs
  | c == '('  = (OPar_T):(getTokens cs)
  | c == ')'  = (CPar_T):(getTokens cs)
  | c == '+'  = (Soma_T):(getTokens cs)
  | c == '*'  = (Mult_T):(getTokens cs)
  | otherwise = (Num_T (read num)):(getTokens res)
  where
    (num, res) = getNum list ""
    getNum [] x = (x, [])
    getNum list@(c:cs) x
      | isDigit c = getNum cs (x++[c])
      | otherwise = (x,list)

expExp1 :: E
expExp1 = Soma (Soma (Num 30) (Num 10)) (Num 2)

expStr1 :: String
expStr1 = "1 + 1"

expStr2 :: String
expStr2 = "(2 + 2)"

expStr3 :: String
expStr3 = "2 + 20 * 2"

expStr4 :: String
expStr4 = "0 + (2 + 20) * 2"
