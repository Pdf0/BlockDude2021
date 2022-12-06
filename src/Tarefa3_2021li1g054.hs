{- |
Module      : Tarefa3_2021li1g054
Description : Representação textual do jogo
Copyright   : Pedro Eira de Sousa <a100823@alunos.uminho.pt>;
            : Vicente Costa Martins <a100713@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g054 ( 
    -- * Documentação
    -- ** Funções Principais
  showJogador , colocaJogador ,
    -- ** Funções Auxiliares
  colocaJogadorAux , colocaCaixa , unlines'
  ) where

import LI12122

instance Show Jogo where
    show jogo  = showJogador jogo

{- | A função 'showJogador' transforma o tipo de dados __Jogo__ numa instância da class __Show__ (ou seja, numa representação textual) de acordo com a seguinte formatação:
    
    1. Espaços vazios denotam-se por __“ ”__ (um espaço em branco).
    
    2. Blocos de pedra denotam-se por __“X”__.
    
    3. Caixas denotam-se por __“C”__.
    
    4. A porta denota-se por __“P”__.
    
    5. O personagem denota-se por __“>”__ (se estiver voltado para a direita) ou __“<”__ (se estiver voltado para a esquerda).
   
   Para isso, são utilizadas as seguintes funções auxiliares: 
   
   * ’colocaJogador’
   * ’colocaCaixa’ 
   * ’unlines'’
   * ’showd’
   * ’showp’
   
   == Exemplo de utilização:
   >>> showJogador (Jogo [[Vazio,Vazio,Vazio,Vazio,Bloco],
                          [Vazio,Vazio,Vazio,Vazio,Bloco],
                          [Porta,Vazio,Caixa,Vazio,Bloco],
                          [Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (3,2) Oeste False))
   "    X
        X
    P C<X
    XXXXX"
-}

showJogador :: Jogo -- ^Recebe um Jogo
            -> String -- ^Devolve a representação textual do Jogo (String)
showJogador (Jogo mapa (Jogador coordenadas direcao temCaixa)) = unlines' $ map concat $ colocaJogador (Jogador coordenadas direcao temCaixa) (map (map showp) mapa)

{- | A função 'colocaJogador' serve de função auxiliar da a função 'showJogador' e tem como função auxilar a função 'colocaJogadorAux' e a função 'colocaCaixa'. Coloca o jogador na sua respetiva posição, verificando para que lado está virado, e se carrega uma Caixa ou não.

    Se for dada a indicação de que o jogador carrega uma Caixa, é colocada também uma Caixa em SScima do Jogador (usando a função 'colocaCaixa'), logo, a caixa carregada pelo jogador não pertence ao mapa.

    O __mapa__ recebido nesta função é do tipo '[[String]]', pois ao __mapa__ do tipo 'Mapa' sao aplicadas as funções __(map (map 'showp') mapa)__

    == Exemplos de utilização:
    >>> colocaJogador (Jogador (3,2) Oeste False) [[" "," "," "," ","X"],
                                                   [" "," "," "," ","X"],
                                                   ["P"," ","C"," ","X"],
                                                   ["X","X","X","X","X"]]
    [[" "," "," "," ","X"],
     [" "," "," "," ","X"],
     ["P"," ","C","<","X"],
     ["X","X","X","X","X"]]
    
    >>> colocaJogador (Jogador (3,2) Oeste True) [[" "," "," "," ","X"],
                                                   [" "," "," "," ","X"],
                                                   ["P"," ","C"," ","X"],
                                                   ["X","X","X","X","X"]]
    [[" "," "," "," ","X"],
     [" "," "," ","C","X"],
     ["P"," ","C","<","X"],
     ["X","X","X","X","X"]] 
-}

colocaJogador :: Jogador -- ^Recebe o Jogador
              -> [[String]] -- ^Recebe o mapa aplicado às funções (map (map showp) mapa)
              -> [[String]] -- ^Devolve o mapa com o Jogador já inserido, e a carregar uma Caixa se for esse o caso
colocaJogador (Jogador c d temCaixa) m | temCaixa = colocaCaixa (Jogador c d temCaixa) (colocaJogadorAux (Jogador c d temCaixa) m 0) 0
                                       | otherwise = colocaJogadorAux (Jogador c d temCaixa) m 0

-- | A função 'colocaJogadorAux' é uma função auxiliar da função 'colocaJogador'

colocaJogadorAux :: Jogador -- Recebe o Jogador
                 -> [[String]] -- ^Recebe o mapa
                 -> Int -- ^ Recebe também um 'Int' (neste caso 0), já que a função tem que verificar em que linha se coloca o Jogador
                 -> [[String]] -- ^Devolve o mapa com o Jogador já inserido
colocaJogadorAux _ [] _ = []
colocaJogadorAux (Jogador (x,y) d temCaixa) (h:t) y1 | y == y1 = substituiJogador (Jogador (x,y) d temCaixa) h 0 : t
                                                     | otherwise = h : colocaJogadorAux (Jogador (x,y) d temCaixa) t (y1+1)
    where substituiJogador :: Jogador -> [String] -> Int -> [String]
          substituiJogador _ [] _ = []
          substituiJogador (Jogador (x,y) d temCaixa) (h:t) x1 | x == x1 = (showd d) : t  
                                                               | otherwise = h : substituiJogador (Jogador (x,y) d temCaixa) t (x1 + 1)

{- | A função 'colocaCaixa' é uma função auxiliar da função 'colocaJogador', recebendo assim o mapa com o jogador já colocado.

    Esta, só é invocada caso a variável __temCaixa__, introduzida na chamada da função 'colocaJogador', for 'True'.
    
    Quando usada, introduz uma Caixa nas coordenadas acima do Jogador.
-}    
colocaCaixa :: Jogador -- ^Recebe o jogador, de modo a saber as suas coordenadas
            -> [[String]] -- ^Recebe o mapa com o Jogador já inserido
            -> Int -- ^Recebe também um 'Int' (neste caso 0), já que a função tem que verificar em que linha se coloca a caixa
            -> [[String]] -- ^Devolve mapa com a Caixa já inserida
colocaCaixa _ [] _ = []
colocaCaixa (Jogador (x,y) d temCaixa) (h:t) y1 | (y-1) == y1 = substituiCaixa (x,y-1) h 0 : t
                                                | otherwise = h : colocaCaixa (Jogador (x,y) d temCaixa) t (y1 + 1)
    where substituiCaixa :: Coordenadas -> [String] -> Int -> [String]
          substituiCaixa _ [] _ = []
          substituiCaixa (x,y) (h:t) x1 | x == x1 = "C" : t
                                        | otherwise = h : substituiCaixa (x,y) t (x1 +1)

{- | A função 'unlines'' é uma adaptação da função predefinida 'unlines'.

    O que difere esta função da original é o facto de não mudar de linha no final da 'String'

-}

unlines' :: [String] -- ^recebe um mapa com as linhas em String diferentes
         -> String -- ^Junta as linhas para formar uma String única
unlines' [] = []
unlines' [x] = x
unlines' (h:t) = (h ++ "\n") ++ unlines' t

showd :: Direcao -- ^Recebe uma Direção
      -> [Char] -- ^Transforma-a na sua respetiva representação textual
showd d = case d of
    Este -> ">"
    Oeste -> "<"

showp :: Peca -- ^Recebe uma Peça
      -> [Char] -- ^Transforma-a na sua respetiva representação textual
showp p = case p of
    Vazio -> " "
    Bloco -> "X"
    Caixa -> "C"
    Porta -> "P"
