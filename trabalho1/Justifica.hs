module Justifica where

{--- Função Principa ---}
justifica :: String -> String
justifica text = juntaLinhas $ completaEspacosTodos n m ll
  where
    l = separaLinhas text
    n = tamanhoMaiorLinha l
    m = length $ last l
    ll = map separaPalavras l 

{--- Funções das dicas ---}
separaLinhas :: String -> [String]
separaLinhas = lines
tamanhoMaiorLinha :: [String]-> Int
tamanhoMaiorLinha = (foldr (max) 0) . (map length)
separaPalavras :: String -> [String]
separaPalavras = words
insereEspacos :: Int -> String -> String
insereEspacos = (++) . (flip replicate ' ')

{--- Divisões extras ---}
completaEspacos :: Int -> [String] -> [String]
completaEspacos _ [] = []
completaEspacos n l@(x:xs) = x:(completaEspacosAux u r xs)
  where
    m = n - (contaEspacos l)
    u = div m (length xs)
    r = rem m (length xs)

completaEspacosAux :: Int -> Int -> [String] -> [String]
completaEspacosAux _ _ [] = []
completaEspacosAux u r (x:xs) = (insereEspacos (addif r u) x):(completaEspacosAux u (subif r) xs)
  where
    addif 0 = id
    addif n = (+1)
    subif 0 = 0
    subif n = (n-1)

contaEspacos :: [String] -> Int
contaEspacos = sum . (map length)

completaEspacosTodos :: Int -> Int -> [[String]] -> [[String]]
completaEspacosTodos n m l = map (completaEspacos n) (init l) ++ [completaEspacos m (last l)]

juntaLinhas :: [[String]] -> String
juntaLinhas = (foldl (\x y -> x ++ y ++ "\n") "") . (map concat)

textoEx1 = "RUBIÃO fitava a enseada -- eram oito horas da manhã.\nQuem o visse com os polegares metidos no cordão do chambre à janela de uma\ngrande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\nquieta mas em verdade vos digo que pensava em outra coisa.\nCotejava o passado com o presente. Que era há um ano?\nProfessor. Que é agora? Capitalista! Olha para si para as chinelas\n(umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\npara o jardim para a enseada para os morros e para o céu e tudo desde as chinelas\naté o céu tudo entra namesma sensação de propriedade.\n"

textoEx2 = "Jonathas Augusto\nde Oliveira\nConceição"

textoEx3 =
  "\"Lorem ipsum\" text is derived from sections 1.10.33 of Cicero's De finibus bonorum et malorum.[1]\n\
  \\n\
  \It is not known exactly when the text obtained its current standard form; it may have been as late as the 1960s. Dr.\n\
  \Richard McClintock, a Latin scholar who was the\npublications director at Hampden–Sydney College in Virginia, discovered\n\
  \the source of the passage sometime before 1982 while searching for instances of the Latin\n\
  \word \"consectetur\" (\"that [he/she/it] pursue\", subjunctive), rarely used in classical literature.[2][a]\n\
  \The physical source of the lorem ipsum text may be the 1914 Loeb\nClassical Library Edition of the De Finibus,\n\
  \where the Latin text, presented on the left-hand (even) pages, breaks off on page 34 with \"Neque porro quisquam\n\
  \est qui do-\" and continues on page 36 with \"lorem ipsum ...\", suggesting that the galley type of that page was\n\
  \mixed up to make the dummy text seen today.[4]\n"

