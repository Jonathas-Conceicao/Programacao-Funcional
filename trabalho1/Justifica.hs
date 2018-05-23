module Justifica where

{--- Função Principa ---}
justifica :: String -> String
justifica text = juntaLinhas $ completaEspacosTodos n ll
  where
    l = separaLinhas text
    n = tamanhoMaiorLinha l
    ll = map separaPalavras l 

tmp :: String -> Int
tmp text = n
  where
    l = separaLinhas text
    ll = map separaPalavras l 
    n = foldl max 0 (map length l)


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
completaEspacos n l@(x:xs) = x:(completaEspacosAux u r xs)
  where
    m = n - (contaEspacos l)
    u = div m (length xs)
    r = rem m (length xs)
    completaEspacosAux :: Int -> Int -> [String] -> [String]
    completaEspacosAux _ _   []   = []
    completaEspacosAux u 0 (x:xs) = (insereEspacos u     x):(completaEspacosAux u 0     xs)
    completaEspacosAux u r (x:xs) = (insereEspacos (u+1) x):(completaEspacosAux u (r-1) xs)
    

contaEspacos :: [String] -> Int
contaEspacos = sum . (map length)

completaEspacosTodos :: Int -> [[String]] -> [[String]]
completaEspacosTodos n = map (completaEspacos n)

juntaLinhas :: [[String]] -> String
juntaLinhas = (foldl (\x y -> x ++ y ++ "\n") "") . (map concat)

textoEx1 = "RUBIÃO fitava a enseada -- eram oito horas da manhã.\nQuem o visse com os polegares metidos no cordão do chambre à janela de uma\ngrande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\nquieta mas em verdade vos digo que pensava em outra coisa.\nCotejava o passado com o presente. Que era há um ano?\nProfessor. Que é agora? Capitalista! Olha para si para as chinelas\n(umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\npara o jardim para a enseada para os morros e para o céu e tudo desde as chinelas\naté o céu tudo entra namesma sensação de propriedade.\n"

textoEx2 = "Jonathas Augusto\nde Oliveira\nConceição"
