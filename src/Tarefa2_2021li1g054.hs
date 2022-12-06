{- |
Module      : Tarefa2_2021li1g054
Description : Construção/Desconstrução do mapa
Copyright   : Pedro Eira de Sousa <a100823@alunos.uminho.pt>;
            : Vicente Costa Martins <a100713@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g054 where

import LI12122
import Tarefa1_2021li1g054

-- Constrói Mapa
{- | A função ’constroiMapa’ que recebe uma lista de peças com as suas respetivas coordenadas e devolve um mapa, que é uma lista com listas de peças (que estão inseridos os Vazios) 
   organizados pela suas coordenadas
    
   
   Para isso, são utilizadas as seguintes funções auxiliares: 
   
   * 'constroiListaV'
   * 'substVazios' 
   * 'constroiListas'
   * 'retiraCoords'
   
   == Exemplo de utilização:
   >>> constroiMapa [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,2))]
   [[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio],[Porta,Vazio,Vazio,Caixa],[Bloco,Bloco,Bloco,Bloco]]
-}
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa pecas = retiraCoords (constroiListas (substVazios pecas (constroiListaV pecas 0)) 0)


{- | A função ’constroiListaV’ que recebe uma lista de peças com as suas respetivas coordenadas e um Inteiro e devolve uma uma lista de Vazios e suas coordenadas em que o último elemento
tem de ser o Vazio com a maior coordenada possivel que o mapa tem.
    
   
   Para isso, são utilizadas as seguintes funções auxiliares: 
   
   * 'constroiListaV2'
   * 'maxCoord' 
   * 'coords'
   
   == Exemplo de utilização:
   >>> constroiListaV [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,2))] 0
   [(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(2,0)),(Vazio,(3,0)),(Vazio,(0,1)),(Vazio,(1,1)),(Vazio,(2,1)),(Vazio,(3,1)),(Vazio,(0,2)),(Vazio,(1,2)),(Vazio,(2,2)),(Vazio,(3,2)),(Vazio,(0,3)),(Vazio,(1,3)),(Vazio,(2,3)),(Vazio,(3,3))]
-}
constroiListaV :: [(Peca, Coordenadas)] -> Int -> [(Peca, Coordenadas)]
constroiListaV [] y1 = []
constroiListaV m y1 | y1 > ymax = []
                    | otherwise = constroiListaV2 m 0 y1 ++ constroiListaV m (y1+1)
                where (xmax,ymax) = maxCoord (coords m)


{- | A função ’constroiListaV2’ que recebe uma lista de peças com as suas respetivas coordenadas e dois Inteiro e devolve uma uma lista de Vazios e suas coordenadas, em que as coordeandas estão
   ordenadas pelo valor do primeiro elemento da coordenadas e o valor do segundo elemento é igual ao segundo Inteiro, em que nem o primeiro nem o segundo valor das coordenadas pode ser maior
   do que o valor da maior coordenada possivel que o mapa tem.
    
   
   Para isso, são utilizadas as seguintes funções auxiliares: 

   * 'maxCoord' 
   * 'coords'
   
   == Exemplo de utilização:
   >>> constroiListaV2 [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,2))] 0 0
   [(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(2,0)),(Vazio,(3,0))]
-}
constroiListaV2 :: [(Peca, Coordenadas)] -> Int -> Int -> [(Peca, Coordenadas)]
constroiListaV2 m x1 y1 |  x1 > xmax = []
                        | otherwise = (Vazio,(x1,y1)) : constroiListaV2 m (x1+1) y1
                    where (xmax,ymax) = maxCoord (coords m)


{- | A função ’substVazios’ que recebe duas lista de peças com as suas respetivas coordenadas (em que a primeira lista é a lista com os __Blocos__, __Caixas__ e a __Porta__ e a segunda lista é o mapa todo
vazio) em que devolve o lista de peças com as suas respetivas coordenadas (em que substitui os __Vazios__ da segunda lista pela as peças da primeira lista)
   
   Para isso, é utilizada a seguinte função auxiliar:

   * 'substVazios2'

   == Exemplo de utilização:
   >>> substVazios [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3))]  [(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(2,0)),(Vazio,(0,1)),(Vazio,(1,1)),(Vazio,(2,1)),(Vazio,(0,2)),(Vazio,(1,2)),(Vazio,(2,2)),(Vazio,(0,3)),(Vazio,(1,3)),(Vazio,(2,3))]
   [(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(2,0)),(Vazio,(0,1)),(Vazio,(1,1)),(Vazio,(2,1)),(Porta,(0,2)),(Vazio,(1,2)),(Vazio,(2,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]
-}
substVazios :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
substVazios [] a = a
substVazios (x:xs) l = substVazios xs (substVazios2 x l)


{- | A função ’substVazios2’ que recebe uma peça e a sua coordenada e uma lista de vazios e as suas coordenadas já ordenada e devolve uma lista de peças e coordenadas, em que
substitui o __Vazio__ pela __Peça__ que tem a mesma coordenada.

   == Exemplo de utilização:
   >>> substVazios2 (Porta, (0,2)) [(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(2,0)),(Vazio,(0,1)),(Vazio,(1,1)),(Vazio,(2,1)),(Vazio,(0,2)),(Vazio,(1,2)),(Vazio,(2,2))]
   [(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(2,0)),(Vazio,(0,1)),(Vazio,(1,1)),(Vazio,(2,1)),(Porta,(0,2)),(Vazio,(1,2)),(Vazio,(2,2))]
-}
substVazios2 :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
substVazios2 a [] = []
substVazios2 (p1,(x1,y1)) ((p2,(x2,y2)):h) | x1==x2 && y1==y2 = (p1,(x1,y1)):h
                                           | otherwise = (p2,(x2,y2)) : substVazios2 (p1,(x1,y1)) h



{- | A função ’constroiListas’ que recebe uma lista de peças com as suas respetivas coordenadas e um Inteiro e devolve uma lista de listas de peças com as suas respetivas coordenadas,
em que cada lista dentro da lista, todos as suas peças tem o mesmo valor no segundo elemento das coordenadas. 
    
   
   Para isso, são utilizadas as seguintes funções auxiliares: 
   
   * 'constroiLista'
   * 'maxCoord'
   * 'coords'
   
   == Exemplo de utilização:
   >>> constroiListas [(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(2,0)),(Vazio,(0,1)),(Vazio,(1,1)),(Vazio,(2,1)),(Porta,(0,2)),(Vazio,(1,2)),(Vazio,(2,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))] 0
   [[(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(2,0))],[(Vazio,(0,1)),(Vazio,(1,1)),(Vazio,(2,1))],[(Porta,(0,2)),(Vazio,(1,2)),(Vazio,(2,2))],[(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]]
-}
constroiListas :: [(Peca,Coordenadas)] -> Int -> [[(Peca,Coordenadas)]]
constroiListas m y1 | y1 > ymax = []
                    | otherwise = reverse(constroiLista m [] y1) : constroiListas m (y1 + 1)
                    where (xmax,ymax) = maxCoord (coords m)


{- | A função ’constroiListas’ que recebe uma listas de peças com as suas respetivas coordenadas (já ordenada), uma lista vazia e um Inteiro e devolve uma lista de listas de peças
   com as com as suas ordenadas, em que o numnero Inteiro é o valor do segundo elemento das coordenadas em todas a peças. 

   
   == Exemplo de utilização:
   >>> constroiLista [(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(2,0)),(Vazio,(0,1)),(Vazio,(1,1)),(Vazio,(2,1)),(Porta,(0,2)),(Vazio,(1,2)),(Vazio,(2,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))] [] 0
   [(Vazio,(2,0)),(Vazio,(1,0)),(Vazio,(0,0))]
-}
constroiLista :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)] -> Int -> [(Peca,Coordenadas)]
constroiLista [] ac y1 = ac
constroiLista ((p,(x,y)):t) ac y1 | y /= y1 = constroiLista t ac y1
                                  | otherwise = constroiLista t ((p,(x,y)):ac) y1


{- | A função ’retiraCoords’ que recebe uma lista de listas de peças com as suas respetivas coordenadas (já ordenada) e devolve uma lista de listas de peças.
   
   Para isso, é utilizada a seguinte função auxiliar: 

   * retiraCoordsAux que pega numa lista de peças e as suas coordenadas e devolve a lista só com peças.
   
   == Exemplo de utilização:
   >>> retiraCoords [[(Vazio,(0,0)),(Vazio,(1,0)),(Vazio,(2,0))],[(Vazio,(0,1)),(Vazio,(1,1)),(Vazio,(2,1))],[(Porta,(0,2)),(Vazio,(1,2)),(Vazio,(2,2))],[(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3))]]
   [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Porta,Vazio,Vazio],[Bloco,Bloco,Bloco]]
-}
retiraCoords :: [[(Peca,Coordenadas)]] -> [[Peca]]
retiraCoords m = map retiraCoordsAux m
    where retiraCoordsAux :: [(Peca,Coordenadas)] -> [Peca]
          retiraCoordsAux [] = []
          retiraCoordsAux ((p,c):t) = p : retiraCoordsAux t

-- Desconstrói Mapa


{- | A função ’desconstroiMapa’ que recebe um mapa, e devolve uma lista de peças e coordenadas (sem __Vazios__).
    
   
   Para isso, são utilizadas as seguintes funções auxiliares: 
   
   * 'desconstroiMapaAux'
   * 'retiraListas' 
   
   == Exemplo de utilização:
   >>> desconstroiMapa [[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio],[Porta,Vazio,Vazio,Caixa],[Bloco,Bloco,Bloco,Bloco]]
   [(Porta,(0,2)),(Caixa,(3,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3)),(Bloco,(3,3))]
-}
desconstroiMapa :: Mapa -> [(Peca,Coordenadas)]
desconstroiMapa m = retiraListas (desconstroiMapaAux m 0 0)


{- | A função ’retiraListas’ que recebe uma lista  de listas de peças e suas coordenadas (sem as peças __Vazios__), e devolve uma lista de peças e coordenadas (sem __Vazios__).
     
   == Exemplo de utilização:
   >>> retiraListas [[],[],[(Porta,(0,2)),(Caixa,(3,2))],[(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3)),(Bloco,(3,3))]]
   [(Porta,(0,2)),(Caixa,(3,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3)),(Bloco,(3,3))]
-}
retiraListas :: [[(Peca,Coordenadas )]] -> [(Peca,Coordenadas)]
retiraListas [] = []
retiraListas ([]:t) = retiraListas t
retiraListas ((x:xs):t) = x : retiraListas (xs:t)


{- | A função ’desconstroiMapaAux’ que recebe um mapa e dois interios, que vão servir para descobrir as coordenadas das peças, e devolve uma lista de peças e coordenadas (sem __Vazios__).
    
   
   Para isso, é utilizada a seguinte função auxiliar: 
   
   * 'desconstroiLinha' 
   
   == Exemplo de utilização:
   >>> desconstroiMapaAux [[Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio],[Porta,Vazio,Vazio,Caixa],[Bloco,Bloco,Bloco,Bloco]] 0 0
   [[],[],[(Porta,(0,2)),(Caixa,(3,2))],[(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3)),(Bloco,(3,3))]]
-}
desconstroiMapaAux :: Mapa -> Int -> Int -> [[(Peca,Coordenadas)]]
desconstroiMapaAux [] _ _ = []
desconstroiMapaAux (h:t) y x = desconstroiLinha h y x : desconstroiMapaAux t (y+1) x

{- | A função ’desconstroiLinha’ que recebe uma lista de peças e dois interios, que vão servir para descobrir as coordenadas das peças, e devolve uma lista de peças e coordenadas (sem __Vazios__).
    
   
   Para isso, é utilizada a seguinte função auxiliar:  
   
   == Exemplo de utilização:
   >>> desconstroiLinha [Porta,Vazio,Vazio,Caixa] 2 0
   [(Porta,(0,2)),(Caixa,(3,2))]
-}
desconstroiLinha :: [Peca] -> Int -> Int -> [(Peca,Coordenadas)]
desconstroiLinha [] _ _ = []
desconstroiLinha (h:t) y x 
    | h == Vazio = desconstroiLinha t y (x+1)
    | otherwise = (h,(x,y)) : desconstroiLinha t y (x+1)


