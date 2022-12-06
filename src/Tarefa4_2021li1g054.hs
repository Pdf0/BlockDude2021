{- |
Module      : Tarefa4_2021li1g054
Description : Movimentação do personagem
Copyright   : Pedro Eira de Sousa <a100823@alunos.uminho.pt>;
            : Vicente Costa Martins <a100713@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g054 (
    -- * Documentação
    -- ** Funções Principais
  correrMovimentos , moveJogador,
    -- ** Funções Auxiliares 
  retiraPeca , andaEsquerda , andaDireita , largaCaixa
  ) where 

import LI12122
import Tarefa1_2021li1g054
import Tarefa2_2021li1g054
import Tarefa3_2021li1g054



{- | A função 'correrMovimentos' aplica a função 'moveJogador' a todos os elementos da lista.

  == Exemplos de utilização:
  >>> correrMovimentos (Jogo [[Vazio,Vazio,Vazio,Vazio,Bloco],
                          [Vazio,Vazio,Vazio,Vazio,Bloco],
                          [Porta,Vazio,Caixa,Vazio,Bloco],
                          [Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (3,2) Oeste False)) [Trepar,AndarEsquerda]

  "   X
      X
  P<C X
  XXXXX"

  >>> correrMovimentos (Jogo [[Vazio,Vazio,Vazio,Vazio,Bloco],
                          [Vazio,Vazio,Vazio,Vazio,Bloco],
                          [Porta,Vazio,Caixa,Vazio,Bloco],
                          [Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (3,2) Oeste False)) [InterageCaixa,AndarEsquerda,InterageCaixa]

  "   X
      X
  PC< X
  XXXXX"
-}

correrMovimentos :: Jogo -- ^Recebe um Jogo
                -> [Movimento] -- ^Recebe uma lista de movimentos
                -> Jogo -- ^Devolve o jogo já com os movimentos aplicados 
correrMovimentos j [] = j
correrMovimentos j (h:t) = correrMovimentos (moveJogador j h) t

{- | A função 'moveJogador' tem como objetivo alterar o 'Mapa' de acordo com o 'Movimento' aplicado.

  Dependendo do 'Movimento', a função 'moveJogador' pode alterar de várias formas o 'Mapa':
  
  * Se o movimento escolhido for 'AndarEsquerda' ou 'AndarDireita', a função verifica se não existe nenhum Bloco ou Caixa ao lado do Jogador ou, se o jogador estiver a carregar uma Caixa, um Bloco ou uma Caixa a impossiblitar o transporte da mesma.
     Se, ao deslocar-se para um dos lados, o Jogador não tenha um Bloco a servir de chão, então cai diretamente para o Bloco abaixo.
  * Se o movimento escolhido for 'Trepar', a função verifica, dependendo do lado para que o Jogador está virado, se não existe um Bloco ou Caixa acima do que vai trepar e, se estiver a carregar uma Caixa, então verifica se não existem dois Blocos ou Caixas acima do que vai trepar.
     Se as condições anteriormente descritas forem verdadeiras e se existir um Bloco' ou Caixa para trepar, então o Jogador fá-lo.
  * Se o movimento escolhido for 'InterageCaixa', este pode ter duas ações, dependendo se o Jogador carrega uma Caixa ou não:
    
  Se o Jogador carregar uma Caixa, então a função 'moveJogador' verifica se o Jogador consegue largar a Caixa, isto é, se não existe uma parede (dois Blocos ou mais) no lado onde quer largar a Caixa e se não existe nenhum Bloco à frente da Caixa que este carrega.
  Se o Jogador conseguir largar a Caixa, então a Caixa poderá ser largada em Blocos com a mesma altura do Jogador, no chão, ou em blocos mais abaixo, caso o chão desça de nível.

  Se o Jogador não carregar uma Caixa, então a função 'moveJogador' verifica se existe uma Caixa próxima do Jogador para ele a agarrar. Se existir e não existirem Blocos ou Caixas em cima dessa Caixa, ou um bloco em cima do Jogador, então o Jogador apanha a caixa e passa a carregá-la.

  == Exemplos de utilização:
  >>> moveJogador (Jogo [[Vazio,Vazio,Vazio,Vazio,Bloco],
                          [Vazio,Vazio,Vazio,Vazio,Bloco],
                          [Porta,Vazio,Caixa,Vazio,Bloco],
                          [Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (3,2) Oeste False)) InterageCaixa
  "   X
     CX
  P  <X
  XXXXX" 

  >>> moveJogador (Jogo [[Vazio,Vazio,Vazio,Vazio,Bloco],
                          [Vazio,Vazio,Vazio,Vazio,Bloco],
                          [Porta,Vazio,Caixa,Vazio,Bloco],
                          [Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (3,2) Oeste False)) Trepar
  "   X
    < X
  P C X
  XXXXX" 

  >>> moveJogador (Jogo [[Vazio,Vazio,Vazio,Vazio,Bloco],
                          [Vazio,Vazio,Vazio,Vazio,Bloco],
                          [Porta,Vazio,Caixa,Vazio,Bloco],
                          [Bloco,Bloco,Bloco,Bloco,Bloco]] (Jogador (3,2) Oeste False)) AndarDireita
  "   X
      X
  P C>X
  XXXXX" 
-}

moveJogador :: Jogo -- ^Recebe um Jogo
               -> Movimento -- ^Recebe um movimento
               -> Jogo -- ^Retorna o jogo já com o movimento aplicado

moveJogador (Jogo mapa (Jogador (x,y) _ True)) AndarEsquerda 
  | (x-1) < 0 = Jogo mapa (Jogador (x,y) Oeste True)
  | elem (Bloco,(x-1,y)) (desconstroiMapa mapa) || elem (Caixa,(x-1,y)) (desconstroiMapa mapa) || elem (Bloco,(x-1,y-1)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) Oeste True)
  | otherwise = andaEsquerda (Jogo mapa (Jogador (x,y) Oeste True)) (x-1,y+1)
  
 
moveJogador (Jogo mapa (Jogador (x,y) _ False)) AndarEsquerda 
  | (x-1) < 0 = Jogo mapa (Jogador (x,y) Oeste False)
  | elem (Caixa,(x-1,y)) (desconstroiMapa mapa) || elem (Bloco,(x-1,y)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) Oeste False)
  | otherwise = andaEsquerda (Jogo mapa (Jogador (x,y) Oeste False)) (x-1,y+1)

moveJogador (Jogo mapa (Jogador (x,y) _ True)) AndarDireita 
  | (x+1) > xmax = Jogo mapa (Jogador (x,y) Este True)
  | elem (Bloco,(x+1,y)) (desconstroiMapa mapa) || elem (Caixa,(x+1,y)) (desconstroiMapa mapa) || elem (Bloco,(x+1,y-1)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) Este True) 
  | otherwise = andaDireita (Jogo mapa (Jogador (x,y) Este True)) (x+1,y+1)
  where (xmax,ymax) = maxCoord (map snd (desconstroiMapa mapa))

moveJogador (Jogo mapa (Jogador (x,y) _ False)) AndarDireita 
  | (x+1) > xmax = Jogo mapa (Jogador (x,y) Este False) 
  | elem (Caixa,(x+1,y)) (desconstroiMapa mapa) || elem (Bloco,(x+1,y)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) Este False)
  | otherwise = andaDireita (Jogo mapa (Jogador (x,y) Este False)) (x+1,y+1)
  where (xmax,ymax) = maxCoord (map snd (desconstroiMapa mapa))

moveJogador (Jogo mapa (Jogador (x,y) Oeste True)) Trepar
  | (x-1) < 0 = Jogo mapa (Jogador (x,y) Oeste True)
  | elem (Bloco,(x-1,y-1)) (desconstroiMapa mapa) || elem (Caixa,(x-1,y-1)) (desconstroiMapa mapa) || elem (Bloco,(x-1,y-2)) (desconstroiMapa mapa) || elem (Caixa,(x-1,y-2)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) Oeste True)
  | elem (Bloco,(x-1,y)) (desconstroiMapa mapa) || elem (Caixa,(x-1,y)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x-1,y-1) Oeste True)
  | otherwise = Jogo mapa (Jogador (x,y) Oeste True)

moveJogador (Jogo mapa (Jogador (x,y) Este True)) Trepar
  | (x+1) > xmax = Jogo mapa (Jogador (x,y) Este True)
  | elem (Bloco,(x+1,y-1)) (desconstroiMapa mapa) || elem (Caixa,(x+1,y-1)) (desconstroiMapa mapa) || elem (Bloco,(x+1,y-2)) (desconstroiMapa mapa) || elem (Caixa,(x+1,y-2)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) Este True)
  | elem (Bloco,(x+1,y)) (desconstroiMapa mapa) || elem (Caixa,(x+1,y)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x+1,y-1) Este True)
  | otherwise = Jogo mapa (Jogador (x,y) Este True)
  where (xmax,ymax) = maxCoord (map snd (desconstroiMapa mapa))

moveJogador (Jogo mapa (Jogador (x,y) Oeste False)) Trepar 
  | (x-1) < 0 = Jogo mapa (Jogador (x,y) Oeste False)
  | elem (Bloco,(x-1,y-1)) (desconstroiMapa mapa) || elem (Caixa,(x-1,y-1)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) Oeste False) 
  | elem (Bloco,(x-1,y)) (desconstroiMapa mapa) || elem (Caixa,(x-1,y)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x-1,y-1) Oeste False)
  | otherwise = Jogo mapa (Jogador (x,y) Oeste False)

moveJogador (Jogo mapa (Jogador (x,y) Este False)) Trepar  
  | (x+1) > xmax = Jogo mapa (Jogador (x,y) Este False)
  | elem (Bloco,(x+1,y-1)) (desconstroiMapa mapa) || elem (Caixa,(x+1,y-1)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) Este False)
  | elem (Bloco,(x+1,y)) (desconstroiMapa mapa) || elem (Caixa,(x+1,y)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x+1,y-1) Este False)
  | otherwise = Jogo mapa (Jogador (x,y) Este False)
  where (xmax,ymax) = maxCoord (map snd (desconstroiMapa mapa))

moveJogador (Jogo mapa (Jogador (x,y) Oeste False)) InterageCaixa 
  | elem (Bloco,(x-1,y-1)) (desconstroiMapa mapa) || elem (Caixa,(x-1,y-1)) (desconstroiMapa mapa) || elem (Bloco,(x,y-1)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) Oeste False)
  | elem (Caixa,(x-1,y)) (desconstroiMapa mapa) = Jogo (constroiMapa (retiraPeca (Caixa,(x-1,y)) (desconstroiMapa mapa))) (Jogador (x,y) Oeste True)
  | otherwise = Jogo mapa (Jogador (x,y) Oeste False)

moveJogador (Jogo mapa (Jogador (x,y) Este False)) InterageCaixa
  | elem (Bloco,(x+1,y-1)) (desconstroiMapa mapa) || elem (Caixa,(x+1,y-1)) (desconstroiMapa mapa) || elem (Bloco,(x,y-1)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) Este False)
  | elem (Caixa,(x+1,y)) (desconstroiMapa mapa) = Jogo (constroiMapa (retiraPeca (Caixa,(x+1,y)) (desconstroiMapa mapa))) (Jogador (x,y) Este True)
  | otherwise = Jogo mapa (Jogador (x,y) Este False)

moveJogador (Jogo mapa (Jogador (x,y) Oeste True)) InterageCaixa
  | elem (Bloco,(x-1,y-1)) (desconstroiMapa mapa) || elem (Caixa,(x-1,y-1)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) Oeste True)
  | elem (Bloco,(x-1,y)) (desconstroiMapa mapa) ||elem (Caixa,(x-1,y)) (desconstroiMapa mapa) = Jogo (constroiMapa (insert1 (Caixa,(x-1,y-1)) (desconstroiMapa mapa))) (Jogador (x,y) Oeste False)
  | otherwise = largaCaixa (Jogo mapa (Jogador (x,y) Oeste True)) (x-1,y+1)

moveJogador (Jogo mapa (Jogador (x,y) Este True)) InterageCaixa
  | elem (Bloco,(x+1,y-1)) (desconstroiMapa mapa) || elem (Caixa,(x+1,y-1)) (desconstroiMapa mapa) = Jogo mapa (Jogador (x,y) Este True)
  | elem (Bloco,(x+1,y)) (desconstroiMapa mapa) || elem  (Caixa,(x+1,y)) (desconstroiMapa mapa) = Jogo (constroiMapa (insert1 (Caixa,(x+1,y-1)) (desconstroiMapa mapa))) (Jogador (x,y) Este False)
  | otherwise = largaCaixa (Jogo mapa (Jogador (x,y) Este True)) (x+1,y+1)

{- | A função 'retiraPeca' reitira uma Peça de uma determinada lista de Peças.
-}

retiraPeca :: (Peca,Coordenadas) -- ^Recebe uma Peca com as suas coordenadas
           -> [(Peca,Coordenadas)] -- ^Recebe uma lista de Peças com Coordenadas
           -> [(Peca,Coordenadas)] -- ^Devolve uma lista de Peças com Coordenadas
retiraPeca p (x:xs) | p == x = xs 
                    | otherwise = x : retiraPeca p xs

{- | A função 'andaEsquerda', ao receber as Coordenadas do Bloco para onde o Jogador se deslocará (que será sempre (x-1,y+1)), verifica se este existe. Se não existir, verifica se existe um Bloco ou Caixa embaixo do anterior.
 Quando existir, então o Jogador ficará em cima da Peça em questão.
-}

andaEsquerda :: Jogo -- ^Recebe um Jogo
             -> Coordenadas -- ^Recebe as Coordenadas do Bloco de Chão à sua esquerda
             -> Jogo -- ^Devolve o Jogo com o Jogador já deslocado
andaEsquerda (Jogo mapa (Jogador (x,y) direcao temCaixa)) (x1,y1) | elem (Bloco,(x1,y1)) (desconstroiMapa mapa) || elem (Caixa,(x1,y1)) (desconstroiMapa mapa) =  Jogo mapa (Jogador (x-1,y1-1) direcao temCaixa)
                                                                  | otherwise = andaEsquerda (Jogo mapa (Jogador (x,y) direcao temCaixa)) (x1,y1+1)

{- | A função 'andaDireita', ao receber as Coordenadas do Bloco para onde o Jogador se deslocará (que será sempre (x+1,y+1)), verifica se este existe. Se não existir, verifica se existe um Bloco ou Caixa embaixo do anterior.
 Quando existir, então o Jogador ficará em cima da Peça em questão.
-}

andaDireita :: Jogo -- ^Recebe um Jogo
            -> Coordenadas -- ^Recebe as Coordenadas do Bloco de Chão à sua direita
            -> Jogo -- ^Devolve o Jogo com o Jogador já deslocado
andaDireita (Jogo mapa (Jogador (x,y) direcao temCaixa)) (x1,y1) | elem (Bloco,(x1,y1)) (desconstroiMapa mapa) || elem (Caixa,(x1,y1)) (desconstroiMapa mapa) =  Jogo mapa (Jogador (x+1,y1-1) direcao temCaixa)
                                                                 | otherwise = andaDireita (Jogo mapa (Jogador (x,y) direcao temCaixa)) (x1,y1+1)

{- | A função 'largaCaixa', ao receber as Coordenadas do Bloco para onde o Jogador largará a Caixa (que será sempre (x-1 ou x+1,y+1)), verifica se este existe. Se não existir, verifica se existe um Bloco ou Caixa embaixo do anterior.
 Quando existir, então a Caixa será inserida no Mapa, em cima da Peça em questão.
-}

largaCaixa :: Jogo -- ^Recebe um Jogo
           -> Coordenadas -- ^Recebe as Coordenadas do Bloco de Chão do lado para que está virado
           -> Jogo -- ^Devolve o Jogo já com a Caixa posicionada no Mapa
largaCaixa (Jogo mapa (Jogador (x,y) direcao temCaixa)) (x1,y1) | elem (Bloco,(x1,y1)) (desconstroiMapa mapa) || elem (Caixa,(x1,y1)) (desconstroiMapa mapa) = Jogo (constroiMapa (insert1 (Caixa,(x1,y1-1)) (desconstroiMapa mapa))) (Jogador (x,y) direcao False)
                                                                | otherwise = largaCaixa (Jogo mapa (Jogador (x,y) direcao True)) (x1,y1+1)