import Data.Char

data E
  = Num Int
  | Soma E E
  | Mult E E
  deriving (Show, Eq)

interpretExp :: E -> Int
interpretExp (Num n) = n
interpretExp (Soma e1 e2) = (interpretExp e1) + (interpretExp e2)
interpretExp (Mult e1 e2) = (interpretExp e1) * (interpretExp e2)

parseExp :: String -> E
parseExp = error "Todo parseExp"

split :: String -> [String]
split "" = []
split list@(c:cs)
  | c == ' '  = split cs
  | c == '('  = "(":(split cs)
  | c == ')'  = ")":(split cs)
  | c == '+'  = "+":(split cs)
  | c == '*'  = "*":(split cs)
  | otherwise = num:(split res)
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
