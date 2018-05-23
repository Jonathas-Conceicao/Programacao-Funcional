module Justifica where

justifica :: String -> String
justifica = error "Todo: Implement justifica"

separaLinhas :: String -> [String]
separaLinhas = lines
tamanhoMaiorLinha :: [String]-> Int
tamanhoMaiorLinha = (foldr (max) 0) . (map length)
separaPalavras :: String -> [String]
separaPalavras = words
insereEspacos :: Int -> String -> String
insereEspacos = (++) . (flip replicate ' ')

textoEx1 = "RUBIÃO fitava a enseada -- eram oito horas da manhã.\nQuem o visse com os polegares metidos no cordão do chambre à janela de uma\ngrande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\nquieta mas em verdade vos digo que pensava em outra coisa.\nCotejava o passado com o presente. Que era há um ano?\nProfessor. Que é agora? Capitalista! Olha para si para as chinelas\n(umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\npara o jardim para a enseada para os morros e para o céu e tudo desde as chinelas\naté o céu tudo entra namesma sensação de propriedade."
