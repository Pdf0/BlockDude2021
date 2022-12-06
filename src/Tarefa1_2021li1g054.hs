{- |
Module      : Tarefa1_2021li1g054
Description : Validação de um potencial mapa
Copyright   : Pedro Eira de Sousa <a100823@alunos.uminho.pt>;
            : Vicente Costa Martins <a100713@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g054 where

import LI12122
import System.Directory.Internal.Prelude (Bool)
import Data.Bool (Bool)
import GHC.Types (Bool)
import Control.Applicative (ZipList)


{- | A função ’validaPotencialMapa’ verifica se dada uma lista de peças e respectivas coordenadas, testa se estas definem
   correctamente um mapa.
   Para isso, são utilizadas as funções: 
   
   * 'ponto1'
   * 'umaPorta'
   * 'caixasNaoFlutuantes'
   * 'existemEspacosVazios'
   * 'chaoContinuo' 
   
   
   
   == Exemplos de utilização:
   >>> validaPotencialMapa [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,2))]
   True
   >>> validaPotencialMapa [(Porta, (0,2)), (Bloco, (0,2)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,2))]
   False
   >>> validaPotencialMapa [(Porta, (0,2)), (Porta, (1,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Caixa, (3,2))]
   False 
   >>> validaPotencialMapa [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,1))]
   False
   >>> existemEspacosVazios [(Porta, (0,1)), (Bloco, (1,1)),(Bloco, (1,0)),(Caixa, (0,0))]
   False
   >>> validaPotencialMapa [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)), (Bloco, (3,3)), (Caixa, (3,2))]
   False
-}

validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa pecas = (ponto1 pecas) && (umaPorta pecas) && (caixasNaoFlutuantes pecas) && (existemEspacosVazios pecas) && (chaoContinuo pecas)



{- | A função ’ponto1’ recebe uma lista de peças e as suas respetivas coordenadas e verifica se há mais do que uma declaração de peça para a mesma posição. 
   
   A função ’ponto1’ é constituida por duas funções auxiliares:

   * ’daCoordenadas’ que recebe uma lista de peças e as suas respetivas coordenadas, em que, separa as coordenadas da lista e cria uma lista só de coordenadas.
   * ’mesmaPosicao’ verifica se a primeira coordenada da lista é igual a alguma coordenada do resto da lista.



   == Exemplos de utilização:
   >>> ponto1 [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3))]
   True
   >>> ponto1 [(Porta, (0,2)), (Bloco, (0,2)), (Bloco, (1,3)),(Bloco, (2,3))]
   False
-}
-- 1
ponto1 :: [(Peca, Coordenadas)] -> Bool
ponto1 l = mesmaPosicao (daCoordenadas l)
      where
            daCoordenadas :: [(Peca, Coordenadas)] -> [Coordenadas] 
            daCoordenadas [] = []
            daCoordenadas ((_,b):l) = b : daCoordenadas l


            mesmaPosicao:: [Coordenadas] -> Bool 
            mesmaPosicao []= True
            mesmaPosicao [x]= True
            mesmaPosicao (x:xs) | x `elem` xs = False
                                | otherwise = mesmaPosicao xs


--2

{- | A função ’umaPorta’ recebe uma lista de peças e as suas respetivas coordenadas e verifica se existe apenas uma porta.
   A função ’umaPorta’ utiliza as funções auxiliares: 
   
   * 'retiraPorta'
   * 'aux'
   

  
   == Exemplos de utilização:
   >>> umaPorta [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Caixa, (3,2))]
   True
   >>> umaPorta [(Porta, (0,2)), (Porta, (1,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Caixa, (3,2))]
   False   
   >>> umaPorta [(Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)),(Caixa, (3,2))]
   False   
-}
umaPorta :: [(Peca,Coordenadas)] -> Bool
umaPorta l = retiraPorta l == 1

{- | A função ’retiraPorta’ recebe a lista de peças e as suas respetivas coordenadas e dá o numero de portas que essa lista apresenta.
   A função ’retiraPorta’ utiliza a função auxiliar:

   * 'aux'

   
   == Exemplos de utilização:
   >>> retiraPorta [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Caixa, (3,2))]
   1
   >>> retiraPorta [(Porta, (0,2)), (Porta, (1,2)) (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Caixa, (3,2))]
   2  
-}
retiraPorta :: [(Peca,Coordenadas)] -> Int
retiraPorta l = aux l 0

{- | A função ’aux’ recebe a lista de peças e as suas respetivas coordenadas e um numero Int e dá o numero de Portas que essa lista tem.

   
   == Exemplos de utilização:
   >>> aux [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Caixa, (3,2))]
   1
   >>> aux [(Porta, (0,2)), (Porta, (1,2)), (Bloco, (1,3)),(Bloco, (2,3)), (Caixa, (3,2))]
   2  
-}
aux :: [(Peca,Coordenadas)] -> Int -> Int
aux [] p = p
aux ((a,b):t) p | a == Porta = aux t (p+1)
                | otherwise = aux t p

-- 3.

{- | A função ’caixasNaoFlutuantes’ recebe uma lista de peças e as suas respetivas coordenadas e verifica se todas as caixas estão posicionadas em cima de outra caixa ou bloco.
   
   A função ’caixasNaoFlutuantes’ utiliza as funções auxiliares:

   * 'caixasNaoFlutuantesAux'
   * 'soBlocoseCaixas'
   * 'soCaixas'

   
   
   == Exemplos de utilização:
   >>> caixasNaoFlutuantes [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,2))]
   True
   >>> caixasNaoFlutuantes [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,1))]
   False
-}
caixasNaoFlutuantes :: [(Peca,Coordenadas)] -> Bool 
caixasNaoFlutuantes l = caixasNaoFlutuantesAux (soBlocoseCaixas l) (soCaixas l)

{- | A função ’caixasNaoFlutuantesAux’ recebe duas listas de peças e as suas respetivas coordenadas e verifica se as peças não flutuam comparando as coordenas da primeira lista com
   as coordenadas do elemento da segunda lista.
   A função ’caixasNaoFlutuantesAux’ utiliza as funções auxiliares:

   * 'verificaSeNaoFlutua'
   * 'coords'
   

   
   == Exemplos de utilização:
   >>> caixasNaoFlutuantesAux [(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3)),(Bloco,(3,3)),(Caixa,(3,2))] [(Caixa,(3,2))]
   True
   >>> caixasNaoFlutuantesAux [(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3)),(Bloco,(3,3)),(Caixa,(3,1))] [(Caixa,(3,1))]
   False
-}
caixasNaoFlutuantesAux :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)] -> Bool
caixasNaoFlutuantesAux b [] = True
caixasNaoFlutuantesAux b (x:xs) | verificaSeNaoFlutua (coords b) (snd x) = caixasNaoFlutuantesAux b xs
                                | otherwise = False


{- | A função ’verificaSeNaoFlutua’ recebe uma lista de coordenadas e uma coordenada (x,y) e verifica se na lista há alguma coordenada (x,(y+1)). 
   
   
   == Exemplos de utilização:
   >>> verificaSeNaoFlutua [(0,3),(1,3),(2,3),(3,3),(3,2)] (3,2)
   True
   >>> verificaSeNaoFlutua [(0,3),(1,3),(2,3),(3,3),(3,1)] (3,1)
   False
-}
verificaSeNaoFlutua :: [Coordenadas] -> Coordenadas -> Bool
verificaSeNaoFlutua [] _ = False
verificaSeNaoFlutua  ((x1,y1):xs) (x2,y2) | (y1 == (y2+1)) && x1 == x2 = True
                                          | otherwise = verificaSeNaoFlutua xs (x2,y2) 

{- | A função ’soCaixas’ recebe uma lista de peças e as suas respetivas coordenadas e dá uma lista de só de caixas com as suas coordenadas.
   

   
   == Exemplos de utilização:
   >>> soCaixas [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,2))]
   [(Caixa,(3,2))]
-}
soCaixas :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
soCaixas [] = []
soCaixas ((p,c):t) | p == Caixa = (p,c) : soCaixas t
                   | otherwise = soCaixas t 


{- |  A função ’soBlocoseCaixas’ recebe uma lista de peças e as suas respetivas coordenadas e dá uma lista de Blocos e Caixas com as suas respetivas coordenadas.


   
   == Exemplos de utilização:
   >>> soBlocoseCaixas [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,2))]
   [(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3)),(Bloco,(3,3)),(Caixa,(3,2))]
-}
soBlocoseCaixas :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)] 
soBlocoseCaixas [] = []
soBlocoseCaixas ((p,c):t) | (p == Bloco || p == Caixa) = (p,c) : soBlocoseCaixas t
                          | otherwise = soBlocoseCaixas t

{- | A função ’coords’ recebe uma lista de peças e as suas respetivas coordenadas e dá a lista só com as coordenadas.


   
   == Exemplos de utilização:
   >>> coords [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,2))]
   [(0,2),(0,3),(1,3),(2,3),(3,3),(3,2)]
-}
coords :: [(Peca,Coordenadas)] -> [(Coordenadas)]
coords [] = []
coords ((p,c):t) = c : coords t

-- 4.
{- | A função ’existemEspacosVazios’ recebe uma lista de peças e as suas respetivas coordenadas e verifica se o mapa tem espaços vazios, ou seja que nao o mapa não pode
estar totalmente preenchido por caixas, blocos e porta.

    A função ’caixasNaoFlutuantes’ utiliza as funções auxiliares:

    *'maxCoord'
    *'coords'
    *'length' em que dá o número de elementos da lista.

   
   == Exemplos de utilização:
   >>> existemEspacosVazios [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,2))]
   True
   >>> existemEspacosVazios [(Porta, (0,1)), (Bloco, (1,1)),(Bloco, (1,0)),(Caixa, (0,0))]
   False
-}
existemEspacosVazios :: [(Peca,Coordenadas)] -> Bool
existemEspacosVazios l = ((fst (maxCoord (coords l))+1)) * ((snd (maxCoord (coords l))+1)) > (length l) 

{- | A função ’maxCoord’ recebe uma lista de coordenadas e dá um coordenada (x,y) que é a maior coordenada possivel que o mapa tem.
   
   A função ’maxCoord’ utiliza as funções auxiliares:

   * 'x's'
   * 'y's'
   * 'max1'

   
   == Exemplos de utilização:
   >>> maxCoord [(0,2),(0,3),(1,3),(2,3),(3,3),(3,2),(4,1)]
   (4,3)
-}
maxCoord :: [Coordenadas] -> Coordenadas
maxCoord l = ((max1 (x's l)),(max1 (y's l)))

{- | A função ’x's’ recebe uma lista de coordenadas [(x,y)] e da uma lista de inteiros que são os primeiros elementos das coordenadas

   
   == Exemplos de utilização:
   >>> x's [(0,2),(0,3),(1,3),(2,3),(3,3),(3,2),(4,1)]
   [0,0,1,2,3,3,4]
-}
x's :: [Coordenadas] -> [Int]
x's [] = []
x's ((x,y):t) = x : x's t

{- | A função ’y's’ recebe uma lista de coordenadas e da uma lista de inteiros que são os segundos elementos das coordenadas.

   
   == Exemplos de utilização:
   >>> y's [(0,2),(0,3),(1,3),(2,3),(3,3),(3,2),(4,1)]
   [2,3,3,3,3,2,1]
-}
y's :: [Coordenadas] -> [Int]
y's [] = []
y's ((x,y):t) = y : y's t


{- | A função ’max1’ recebe uma lista inteiros e dá o maior inteiro da lista.

   
   == Exemplos de utilização:
   >>> max1 [2,3,3,3,3,2,1]
   3
-}
max1 :: Num a => Ord a => [a] -> a
max1 [] = 0
max1 [x]       = x
max1 (x:y:xs) | x > y = max1 (x:xs)
              | otherwise = max1 (y:xs)

-- 5.

{- | A função ’chaoContinuo’ recebe uma lista de peças e as suas respetivas coordenadas e verifica a base do mapa deve ser composta por blocos, em que deve existir um
chão ao longo do mapa.

   A função ’chaoContinuo’ utiliza as funções auxiliares:

   * 'chaoContinuoAux'
   * 'primeiroBloco'
   * 'coords'
   * 'soBlocos'


   
   == Exemplos de utilização:
   >>> chaoContinuo [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,2))]
   True
   >>> chaoContinuo [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)), (Bloco, (3,3)), (Caixa, (3,2))]
   False
-}

chaoContinuo :: [(Peca,Coordenadas)] -> Bool
chaoContinuo m = chaoContinuoAux (primeiroBloco m) (coords (soBlocos m))


{- | A função ’chaoContinuoAux’ recebe uma coordenada e uma lista de coordenadas (que são dos Blocos) e verifica se essa coordenada é igual à última coordenada da lista. Se for uma coordenada antes da última 
coluna vai ver se essa coordenada tem caminho até á última coluna e ai verifica se essa coordenada é igual à última coordenada da lista. Se, apesar disso não existir caminho para chegar a última coluna
e se existir uma coordena em baixo dessa então faz o mesmo processo de antes. Se apesar disso não existir então vai usar a função ’chaoContinuoAux'’.

   A função ’chaoContinuoAux’ utiliza as funções auxiliares:

   * 'ultimoBloco'
   * 'existeSegundoBloco'
   * 'segundoBloco'
   * 'chaoContinuoAux''

   
   == Exemplos de utilização:
   >>> chaoContinuoAux (0,3) [(0,3),(1,3),(2,3),(3,3)]
   True
   >>> chaoContinuoAux (0,3) [(0,3),(1,3),(3,3)]
   False

-}
chaoContinuoAux :: Coordenadas -> [Coordenadas] -> Bool
chaoContinuoAux (x,y) c | (x,y) == ultimoBloco c = True
                        | existeSegundoBloco (x,y) c = chaoContinuoAux (segundoBloco (x,y) c) c
                        | elem (x,y+1) c = chaoContinuoAux (x,y+1) c
                        | otherwise = chaoContinuoAux' (x,y) c

{- | A função ’chaoContinuoAux'’ recebe uma coordenada e uma lista de coordenadas (que são dos Blocos) e verifica se essa coordenada é igual à última coordenada da lista. Se for uma coordenada antes da última 
coluna vai ver se essa coordenada tem caminho até á última coluna e ai verifica se essa coordenada é igual à última coordenada da lista. Se, apesar disso não existir caminho para chegar a ultima coluna
e se existir uma coordena em acima dessa então faz o mesmo processo de antes.

   A função ’chaoContinuoAux'’ utiliza as funções auxiliares:

   * 'ultimoBloco'
   * 'existeSegundoBloco'
   * 'segundoBloco'

   
   == Exemplos de utilização:
   >>> chaoContinuoAux (0,3) [(0,3),(1,3),(2,3),(3,3)]
   True
   >>> chaoContinuoAux (0,3) [(0,3),(1,3),(3,3)]
   False
-}
chaoContinuoAux' :: Coordenadas -> [Coordenadas] -> Bool
chaoContinuoAux' (x,y) c | (x,y) == ultimoBloco c = True
                         | existeSegundoBloco (x,y) c = chaoContinuoAux (segundoBloco (x,y) c) c
                         | elem (x,y-1) c = chaoContinuoAux' (x,y-1) c 
                         | otherwise = False

{- | A função ’segundoBloco’ recebe uma coordenada (x,y) (que é de um bloco) e uma lista de coordenadas (que são dos Blocos) e dá uma coordenada da lista (x+1,y+1). Se não, dá a coordenada (x+1,y) e se ainda
der então dá a coordenada (x+1,y-1).

   A função ’segundoBloco’ utiliza a função auxiliar:

   * 'elem' que verifica se a coordena faz parte da lista

   
   == Exemplos de utilização:
   >>> segundoBloco (0,3) [(0,3),(1,3),(2,3),(3,3)]
   (1,3)
-}
segundoBloco :: Coordenadas -> [Coordenadas] -> Coordenadas
segundoBloco (x,y) c = if elem (x+1,y+1) c 
                       then (x+1,y+1)
                       else if elem (x+1,y) c
                            then (x+1,y)
                            else (x+1,y-1)

{- |  A função ’existeSegundoBloco’ recebe uma coordenada (x,y) (que é de um bloco) e uma lista de coordenadas (que são dos Blocos) e verifica se a coordenada (x+1,y+1) ou (x+1,y) ou (x+1,y-1)
existe na lista.
   
   A função ’existeSegundoBloco’ utiliza uma função auxiliar:

   * 'elem' que verifica se a coordena faz parte da lista

   
   == Exemplos de utilização:
   >>> existeSegundoBloco (0,3) [(0,3),(1,3),(2,3),(3,3)]
   True
   >>> existeSegundoBloco (0,3) [(0,3),(1,1),(2,3),(3,3)]
   False
-}
existeSegundoBloco :: Coordenadas -> [Coordenadas] -> Bool
existeSegundoBloco (x,y) m = (elem (x+1,y+1) m) || (elem (x+1,y) m) || (elem (x+1,y-1) m)

{- | A função ’primeiroBloco’ recebe uma lista de peças e as suas respetivas coordenadas e dá a coordenada do bloco da primeira coluna, em que o seguendo elemento da coordenada maior.

   A função ’primeiroBloco’ utiliza as funções auxiliares:

   * 'soBlocos'
   * 'primeiraColuna'
   * 'iSort1'
   * 'last' que dá o último elemento da lista
   * 'coord'

   == Exemplos de utilização:
   >>> primeiroBloco [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,2))]
   (0,3)
-}
primeiroBloco :: [(Peca,Coordenadas)] -> Coordenadas
primeiroBloco m = coord (last ( iSort1 (primeiraColuna (soBlocos m)))) 



{- | A função ’ultimoBloco’ recebe uma lista de coordenadas e dá a coordenada do bloco da primeira coluna.

   A função ’ultimoBloco’ utiliza as funções auxiliares:

   * 'ultimaColuna_Coords'
   * 'iSort1_Coords'
   * 'last' que dá o último elemento da lista
   
   == Exemplos de utilização:
   >>> ultimoBloco [(0,3),(1,3),(2,3),(3,3)]
   (3,3)
-}
ultimoBloco :: [Coordenadas] -> Coordenadas
ultimoBloco m = last (iSort1_Coords (ultimaColuna_Coords m))



{- | A função ’primeiraColuna’ recebe uma lista de peças e as suas respetivas coordenadas e dá uma lista com peças e as coordenadas da primeira coluna.

   == Exemplos de utilização:
   >>> primeiraColuna  [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,2))]
   [(Porta,(0,2)),(Bloco,(0,3))]
-}
primeiraColuna :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
primeiraColuna [] = []
primeiraColuna ((p,(x,y)):t) | x == 0    = (p,(x,y)) : primeiraColuna t
                             | otherwise = primeiraColuna t 


{- | A função ’ultimaColuna’ recebe uma lista de peças e as suas respetivas coordenadas e dá uma lista de peças e as suas coordenadas da última coluna.

   A função ’ultimaColuna’ utiliza as funções auxiliares:

   * 'maxCoord'
   * 'coords'


   == Exemplos de utilização:
   >>> ultimaColuna  [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,2))]
   [(Bloco,(3,3)),(Caixa,(3,2))]
-}
ultimaColuna :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
ultimaColuna [] = []
ultimaColuna ((p,(x,y)):t) | x == xmax    = (p,(x,y)) : ultimaColuna t
                           | otherwise = ultimaColuna t 
                           where (xmax,ymax) = maxCoord (coords ((p,(x,y)):t))


{- |  A função ’ultimaColuna_Coords’ recebe a lista de coordenadas e dá a lista de coordenadas da última coluna.

   A função ’ultimaColuna_Coords’ utiliza a função auxiliar:

   * 'maxCoord' 

   == Exemplos de utilização:
   >>> ultimaColuna_Coords [(0,2),(0,3),(1,3),(2,3),(3,3),(3,2),(4,1),(4,2)]
   [(4,1),(4,2)]
-}
ultimaColuna_Coords :: [Coordenadas] -> [Coordenadas]
ultimaColuna_Coords [] = []
ultimaColuna_Coords ((x,y):t) | x == xmax = (x,y) : ultimaColuna_Coords t
                              | otherwise = ultimaColuna_Coords t
                              where (xmax,ymax) = maxCoord ((x,y):t)


{- | A função ’soBlocos’ recebe uma lista de peças e as suas respetivas coordenadas e dá uma lista de Blocos e as suas coordenadas.

   == Exemplos de utilização:
   >>> soBlocos  [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,2))]
   [(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3)),(Bloco,(3,3))]
-}

soBlocos :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
soBlocos [] = []
soBlocos ((p,c):t) | p == Bloco = (p,c) : soBlocos t
                   | otherwise  = soBlocos t

{- |  A função ’iSort1_Coords’ recebe a lista de coordenadas e dá a lista de coordenadas ordenadas pelo segundo elemento das cooordenadas.

   A função ’iSort1_Coords’ utiliza a função auxiliar:

   * 'insert1_Coords'

   == Exemplos de utilização:
   >>> iSort1_Coords [(0,2),(0,3),(1,3),(2,3),(3,3),(3,2),(4,1),(4,2)]
   [(4,1),(0,2),(3,2),(4,2),(0,3),(1,3),(2,3),(3,3)]
-}
iSort1_Coords :: [Coordenadas] -> [Coordenadas]
iSort1_Coords [] = []
iSort1_Coords (x:xs) = insert1_Coords x (iSort1_Coords xs)


{- |  A função ’insert1_Coords’ recebe a coordenada e dá a lista de coordenadas ordenadas pelo segundo elemento das cooordenadas.

   == Exemplos de utilização:
   >>> insert1_Coords (0,3) [(4,1),(0,2),(3,2),(4,2),(1,3),(2,3),(3,3)]
   [(4,1),(0,2),(3,2),(4,2),(0,3),(1,3),(2,3),(3,3)]
-}
insert1_Coords :: Coordenadas -> [Coordenadas] -> [Coordenadas]
insert1_Coords c [] = [c]
insert1_Coords (x1,y1) ((x2,y2):t) | y1 > y2   = (x2,y2) :insert1_Coords (x1,y1) t
                                   | otherwise = ((x1,y1):(x2,y2):t)



{- | A função ’iSort1' recebe uma lista de peças e as suas respetivas coordenadas e dá a lista de peças e suas coordenadas ordenadas pelo segundo elemento das coordenadas.

   A função ’iSort1’ utiliza a função auxiliar:

   * 'insert1'

   == Exemplos de utilização:
   >>> iSort1  [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (3,2))]
   [(Porta,(0,2)),(Caixa,(3,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(2,3)),(Bloco,(3,3))]
-}
iSort1 :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
iSort1 [] = []
iSort1 (x:xs) = insert1 x (iSort1 xs) 

{- |  A função ’insert1’ recebe uma peça e a sua coordenada e uma lista de peças e as suas respetivas coordenadas em que através do segundo elemento da coordenada
   insere a peça e a coordenada na lista de forma a que a lista fique ordenada pelo segundo elemento das coordenadas. 

   == Exemplos de utilização:
   >>> insert1  (Bloco, (2,2)) [(Porta, (0,1)), (Bloco, (0,3)), (Bloco, (1,3)), (Bloco, (3,3)), (Caixa, (3,4))]
   [(Porta,(0,1)),(Bloco,(2,2)),(Bloco,(0,3)),(Bloco,(1,3)),(Bloco,(3,3)),(Caixa,(3,4))]
-}

insert1 :: (Peca, Coordenadas) -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
insert1 p [] = [p]
insert1 (p1,(x1,y1)) ((p2,(x2,y2)):t) | y1 > y2   = (p2,(x2,y2)) : insert1 (p1,(x1,y1)) t
                                      | otherwise = (((p1,(x1,y1)):(p2,(x2,y2)):t))



{- |  A função 'coord’ recebe uma peça e a sua coordenada e da a coordenada.
   
   == Exemplos de utilização:
   >>> coord (Bloco, (2,2))
   (2,2) 
-}
coord :: (Peca,Coordenadas) -> Coordenadas
coord (p,c) = c
