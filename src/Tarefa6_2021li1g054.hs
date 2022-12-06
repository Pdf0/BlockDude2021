{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- |
Module      : Tarefa6_2021li1g054
Description : Construção/Desconstrução do mapa
Copyright   : Pedro Eira de Sousa <a100823@alunos.uminho.pt>;
            : Vicente Costa Martins <a100713@alunos.uminho.pt>;

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
A estratégia que utilizamos aqui foi basicamente gerar uma árvore com uma determinada altura, dependendo do número dado, e  verificando se o jogador chega ou não na porta.
Depois de verificar que o jogador chega a porta, são geradas as listas de movimento, que são guardados numa lista. De seguida vimos qual era a lista com menos elementos.
-}

module Tarefa6_2021li1g054 where

import LI12122
import Tarefa2_2021li1g054
import Tarefa4_2021li1g054

-- | Gera uma árvore que vai até uma determinada altura, dependendo do número dado, verificando se o jogador chega ou não na porta. 
data MTree a =
          Empty
          | Node 
             (Bool,Jogo,[Movimento]) -- ^ Se o jogador esta com as mesmas coordenadas da  porta , o nível de um jogo, que inclui o puzzle (mapa) e o personagem (jogador) e lista de movimentos
             (MTree a) -- ^ gera uma arvore a partir do movimento AndarEsquerda
             (MTree a) -- ^ gera uma arvore a partir do movimento AndarDireita
             (MTree a) -- ^ gera uma arvore a partir do movimento Trepar
             (MTree a) -- ^ gera uma arvore a partir do movimento InterageCaixa
    deriving (Show)


{- | A função 'resolveJogo' é a função que recebe um Int (que é o número de movimentos) e o jogo e mostra se é possivel com o menor número de movimentos que deram a função completar o nivel.

   Para isso, são utilizadas as seguintes funções auxiliares:
   
   * 'melhorCaminho'
   * 'possiveisCaminhos'
   * 'criaCaminhos'

== Exemplos de utilização:
  >>> resolveJogo 2 (Jogo [[Bloco,Porta,Vazio,Vazio],
                           [Bloco,Bloco,Bloco,Bloco]] (Jogador (3,0) Eeste False))
  Just [AndarEsquerda,AndarEsquerda]

  >>> resolveJogo 4 (Jogo [[Bloco,Porta,Vazio,Vazio],
                           [Bloco,Bloco,Bloco,Bloco]] (Jogador (3,0) Eeste False))
  Just [AndarEsquerda,AndarEsquerda]
    
  >>> resolveJogo 1 (Jogo [[Bloco,Porta,Vazio,Vazio],
                           [Bloco,Bloco,Bloco,Bloco]] (Jogador (3,0) Eeste False))
  Nothing
-}
resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo 0 j = Nothing
resolveJogo n j = melhorCaminho $ possiveisCaminhos $ criaCaminhos (n+1) (Node (False,j,[]) Empty Empty Empty Empty)

{- | A função 'melhorCaminho' é a função que recebe uma lista de listas movimentos e devolve a lista com menos movimentos.   

   Para isso, é utilizada a seguinte função auxiliar:  
   
   * length uma função que conta o número de elementos da lista 

== Exemplos de utilização:
>>> melhorCaminho [[AndarEsquerda,AndarEsquerda][AndarEsquerda,AndarEsquerda,AndarEsquerda]]
Just [AndarEsquerda,AndarEsquerda]
-}
melhorCaminho :: [[Movimento]] -> Maybe [Movimento]
melhorCaminho [] = Nothing
melhorCaminho [x] = Just x
melhorCaminho (x:y:xs) | length x <= length y = melhorCaminho (x:xs) 
                       | otherwise = melhorCaminho (y:xs)


{- | A função 'possiveisCaminhos' é a função que através do Mtree ele dá uma lista de listas de movimentos, em que o jogador chega á porta.  
   
   Para isso, é utilizada a seguinte função auxiliar: 
   
   * reverse é uma função que inverte o a lista  
-}
possiveisCaminhos :: MTree a -> [[Movimento]]
possiveisCaminhos Empty = []
possiveisCaminhos (Node (indoor,jogo,movimentos) e d t ic) | indoor = [reverse movimentos] ++ possiveisCaminhos e ++ possiveisCaminhos d ++ possiveisCaminhos t ++ possiveisCaminhos ic
                                                           | otherwise = possiveisCaminhos e ++ possiveisCaminhos d ++ possiveisCaminhos t ++ possiveisCaminhos ic


{- | A função 'criaCaminhos' é a função que através do Int (que é o número de movimentos) e Mtree ele dá outro Mtree  
   
   Para isso, são utilizadas as seguintes funções auxiliares:
   
   * 'inDoor'
   * 'desconstroiMapa' que está na tarefa2
   * 'elem' verfica se o elemento faz parte da lista 
   * 'moveJogador' que está na tarefa4
-}
criaCaminhos :: Int -> MTree a -> MTree a
criaCaminhos 0 mTreee = Empty
criaCaminhos n (Node (indoor,Jogo m (Jogador (x,y) d c),movimentos) andarEsquerda andarDireita trepar interageCaixa) 
    | indoor = Empty

    | elem (Bloco,(x+1,y-1)) (desconstroiMapa m) && elem (Caixa,(x-1,y-1)) (desconstroiMapa m) || elem (Caixa,(x+1,y-1)) (desconstroiMapa m) && elem (Bloco,(x-1,y-1)) (desconstroiMapa m) || elem (Bloco,(x+1,y-1)) (desconstroiMapa m) && elem (Bloco,(x-1,y-1)) (desconstroiMapa m) || elem (Caixa,(x+1,y-1)) (desconstroiMapa m) && elem (Caixa,(x-1,y-1)) (desconstroiMapa m) =
      Empty

    | elem (Bloco,(x+1,y)) (desconstroiMapa m) || elem (Caixa,(x+1,y)) (desconstroiMapa m) || c && elem (Bloco,(x+1,y-1)) (desconstroiMapa m) = 
      Node (inDoor (Jogo m (Jogador (x,y) d c)),Jogo m (Jogador (x,y) d c),movimentos)
      (criaCaminhos (n-1) (Node (inDoor (Jogo m (Jogador (x,y) d c)),moveJogador (Jogo m (Jogador (x,y) d c)) AndarEsquerda, AndarEsquerda:movimentos) andarEsquerda andarDireita trepar interageCaixa))
      Empty 
      (criaCaminhos (n-1) (Node (inDoor (Jogo m (Jogador (x,y) d c)),moveJogador (Jogo m (Jogador (x,y) d c)) Trepar,Trepar:movimentos) andarEsquerda andarDireita trepar interageCaixa)) 
      (criaCaminhos (n-1) (Node (inDoor (Jogo m (Jogador (x,y) d c)),moveJogador (Jogo m (Jogador (x,y) d c)) InterageCaixa,InterageCaixa:movimentos) andarEsquerda andarDireita trepar interageCaixa))

    | elem (Bloco,(x-1,y)) (desconstroiMapa m) || elem (Caixa,(x-1,y)) (desconstroiMapa m) || c && elem (Bloco,(x-1,y-1)) (desconstroiMapa m) =
      Node (inDoor (Jogo m (Jogador (x,y) d c)),Jogo m (Jogador (x,y) d c),movimentos) 
      Empty
      (criaCaminhos (n-1) (Node (inDoor (Jogo m (Jogador (x,y) d c)),moveJogador (Jogo m (Jogador (x,y) d c)) AndarDireita,AndarDireita:movimentos) andarEsquerda andarDireita trepar interageCaixa))
      (criaCaminhos (n-1) (Node (inDoor (Jogo m (Jogador (x,y) d c)),moveJogador (Jogo m (Jogador (x,y) d c)) Trepar,Trepar:movimentos) andarEsquerda andarDireita trepar interageCaixa)) 
      (criaCaminhos (n-1) (Node (inDoor (Jogo m (Jogador (x,y) d c)),moveJogador (Jogo m (Jogador (x,y) d c)) InterageCaixa,InterageCaixa:movimentos) andarEsquerda andarDireita trepar interageCaixa))

    | elem (Bloco,(x-1,y)) (desconstroiMapa m) &&  elem (Bloco,(x-1,y-1)) (desconstroiMapa m) || elem (Caixa,(x-1,y)) (desconstroiMapa m) &&  elem (Caixa,(x-1,y-1)) (desconstroiMapa m) || elem (Bloco,(x-1,y)) (desconstroiMapa m) &&  elem (Caixa,(x-1,y-1)) (desconstroiMapa m) || elem (Caixa,(x-1,y)) (desconstroiMapa m) &&  elem (Bloco,(x-1,y-1)) (desconstroiMapa m) || notElem (Bloco,(x-1,y)) (desconstroiMapa m) || notElem (Caixa,(x-1,y)) (desconstroiMapa m) =
      Node (inDoor (Jogo m (Jogador (x,y) d c)),Jogo m (Jogador (x,y) d c),movimentos) 
      (criaCaminhos (n-1) (Node (inDoor (Jogo m (Jogador (x,y) d c)),moveJogador (Jogo m (Jogador (x,y) d c)) AndarEsquerda, AndarEsquerda:movimentos) andarEsquerda andarDireita trepar interageCaixa))
      (criaCaminhos (n-1) (Node (inDoor (Jogo m (Jogador (x,y) d c)),moveJogador (Jogo m (Jogador (x,y) d c)) AndarDireita,AndarDireita:movimentos) andarEsquerda andarDireita trepar interageCaixa))
      Empty
      (criaCaminhos (n-1) (Node (inDoor (Jogo m (Jogador (x,y) d c)),moveJogador (Jogo m (Jogador (x,y) d c)) InterageCaixa,InterageCaixa:movimentos) andarEsquerda andarDireita trepar interageCaixa))

    | elem (Bloco,(x+1,y)) (desconstroiMapa m) &&  elem (Bloco,(x+1,y-1)) (desconstroiMapa m) || elem (Caixa,(x+1,y)) (desconstroiMapa m) &&  elem (Caixa,(x+1,y-1)) (desconstroiMapa m) || elem (Bloco,(x+1,y)) (desconstroiMapa m) &&  elem (Caixa,(x+1,y-1)) (desconstroiMapa m) || elem (Caixa,(x+1,y)) (desconstroiMapa m) &&  elem (Bloco,(x+1,y-1)) (desconstroiMapa m) || notElem (Bloco,(x+1,y)) (desconstroiMapa m) || notElem (Caixa,(x+1,y)) (desconstroiMapa m) =
      Node (inDoor (Jogo m (Jogador (x,y) d c)),Jogo m (Jogador (x,y) d c),movimentos) 
      (criaCaminhos (n-1) (Node (inDoor (Jogo m (Jogador (x,y) d c)),moveJogador (Jogo m (Jogador (x,y) d c)) AndarEsquerda, AndarEsquerda:movimentos) andarEsquerda andarDireita trepar interageCaixa))
      (criaCaminhos (n-1) (Node (inDoor (Jogo m (Jogador (x,y) d c)),moveJogador (Jogo m (Jogador (x,y) d c)) AndarDireita,AndarDireita:movimentos) andarEsquerda andarDireita trepar interageCaixa))
      Empty
      (criaCaminhos (n-1) (Node (inDoor (Jogo m (Jogador (x,y) d c)),moveJogador (Jogo m (Jogador (x,y) d c)) InterageCaixa,InterageCaixa:movimentos) andarEsquerda andarDireita trepar interageCaixa))
  
    | otherwise = 
      Node (inDoor (Jogo m (Jogador (x,y) d c)),Jogo m (Jogador (x,y) d c),movimentos) 
      (criaCaminhos (n-1) (Node (inDoor (Jogo m (Jogador (x,y) d c)),moveJogador (Jogo m (Jogador (x,y) d c)) AndarEsquerda, AndarEsquerda:movimentos) andarEsquerda andarDireita trepar interageCaixa)) 
      (criaCaminhos (n-1) (Node (inDoor (Jogo m (Jogador (x,y) d c)),moveJogador (Jogo m (Jogador (x,y) d c)) AndarDireita,AndarDireita:movimentos) andarEsquerda andarDireita trepar interageCaixa)) 
      (criaCaminhos (n-1) (Node (inDoor (Jogo m (Jogador (x,y) d c)),moveJogador (Jogo m (Jogador (x,y) d c)) Trepar,Trepar:movimentos) andarEsquerda andarDireita trepar interageCaixa)) 
      (criaCaminhos (n-1) (Node (inDoor (Jogo m (Jogador (x,y) d c)),moveJogador (Jogo m (Jogador (x,y) d c)) InterageCaixa,InterageCaixa:movimentos) andarEsquerda andarDireita trepar interageCaixa))
      

{- | A função 'inDoor' é a função que recebe o jogo e verifica se o jogador esta com as mesmas coordenadas da  porta ou z
   Para isso, são utilizadas as seguintes funções auxiliares: 
   
   * 'doorCoords'
   * 'desconstroiMapa' que está na tarefa2

  == Exemplos de utilização:
  
  >>> inDoor (Jogo [[Bloco,Porta,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco]] (Jogador (1,0) Este False))
  True

  >>> inDoor (Jogo [[Bloco,Porta,Vazio,Vazio],[Bloco,Bloco,Bloco,Bloco]] (Jogador (3,0) Este False))
  False
-}
inDoor :: Jogo -> Bool
inDoor (Jogo mapa (Jogador (x,y) _ _)) = doorCoords(desconstroiMapa mapa) == (x,y)
    where
        doorCoords :: [(Peca,Coordenadas)] -> Coordenadas
        doorCoords [] = (0,0)
        doorCoords ((p,c):xs) | p == Porta = c
                              | otherwise = doorCoords xs


{- | 'm1r'' é o mapa da da 1ºnivel. 
-}
m1r' :: Mapa
m1r' =
  [ [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio],
    [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio],
    [Bloco, Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio],
    [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Caixa, Vazio, Caixa, Caixa, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Caixa, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
  ]

{- | 'm1e1'' é o jogo com o mapa do 1ºnivel com as coordenadas do jogador(18,6), virado para Oeste e sem a caixa. 
-}
m1e1' :: Jogo
m1e1' = Jogo m1r' (Jogador (18,6) Oeste False)


{- | 'm2r'' é o mapa da da 2ºnivel. 
-}
m2r' :: Mapa
m2r' =
  [ [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],
    [Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
    [Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Vazio,Bloco,Bloco,Vazio],
    [Bloco,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio],
    [Bloco,Vazio,Bloco,Vazio,Bloco,Caixa,Caixa,Vazio,Bloco,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Porta,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Bloco,Bloco,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
  ]

{- | 'm2e1'' é o jogo com o mapa do 2ºnivel com as coordenadas do jogador(9,6), virado para Oeste e sem a caixa. 
-}
m2e1' :: Jogo
m2e1' = Jogo m2r' (Jogador (9,6) Oeste False)

{- | 'm3r'' é o mapa da da 3ºnivel. 
-}
m3r' :: Mapa
m3r' =
  [ [Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio],
    [Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Caixa,Caixa,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Porta,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Bloco,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
    [Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
    [Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Caixa,Caixa,Caixa,Bloco],
    [Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],
    [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
  ]

{- | 'm3e1'' é o jogo com o mapa do 3ºnivel com as coordenadas do jogador(12,8), virado para Oeste e sem a caixa. 
-}
m3e1' :: Jogo
m3e1' = Jogo m3r' (Jogador (12,8) Oeste False)

{- | 'm4r' é o mapa do 4ºnivel. 
-}
m4r :: Mapa
m4r =
  [ [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio],
    [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio],
    [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio],
    [Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio],
    [Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
    [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
    [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco],
    [Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio],
    [Bloco,Porta,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio],
    [Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Bloco,Vazio,Caixa,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Caixa,Vazio,Bloco,Vazio,Bloco,Caixa,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
    [Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio]
  ]

{- | 'm4e1' é o jogo com o mapa do 4ºnivel com as coordenadas do jogador(17,8), virado para Este e sem a caixa. 
-}
m4e1 :: Jogo
m4e1 = Jogo m4r (Jogador (17,8) Este False)

{- | 'm5r' é o mapa do 5ºnivel. 
-}
m5r :: Mapa
m5r =
  [ [Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Vazio,Vazio],
    [Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Caixa,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Bloco],
    [Bloco,Caixa,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Bloco,Vazio,Vazio,Bloco],
    [Bloco,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Porta,Vazio,Bloco],
    [Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Bloco],
    [Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Vazio,Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
    [Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Bloco,Bloco,Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Bloco],
    [Bloco,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Vazio,Vazio,Vazio,Vazio,Vazio,Caixa,Caixa,Caixa,Bloco],
    [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]
  ]

{- | 'm5e1' é o jogo com o mapa do 5ºnivel com as coordenadas do jogador(19,15), virado para Este e sem a caixa. 
-}
m5e1 :: Jogo
m5e1 = Jogo m5r (Jogador (19,15) Este False)

m6r :: Mapa
m6r =
  [ [Bloco,Porta,Vazio,Vazio],
    [Bloco,Bloco,Bloco,Bloco]
  ]


m6e1 :: Jogo
m6e1 = Jogo m6r (Jogador (3,0) Este False)