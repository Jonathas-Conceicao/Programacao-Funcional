type Semana = Int

venda :: Semana -> Int
venda 0 = 10
venda 1 = 13
venda 2 = 15
venda 3 = 13
venda 4 = 17
venda 5 = 17
venda 6 = 19
venda 7 = 11
venda n = 9

vendaTotal :: Semana -> Int
vendaTotal 0 = venda 0
vendaTotal n = (venda n) + (vendaTotal $ n-1)

tabela :: Int -> String
tabela n = geraHeader ++ (geraVendas n) ++ (geraTotal n)
  where
    geraTotal n = "Total\t\t" ++ (show $ vendaTotal n) ++ "\n"
    geraVenda n = "Semana " ++ (show n) ++ "\t" ++ (show $ venda n) ++ "\n"
    geraHeader  = "Semana\t\tVenda\n"
    geraVendas 0 = geraVenda 0
    geraVendas n = (geraVendas (n-1)) ++ geraVenda n
