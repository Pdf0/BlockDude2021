module Fixtures where

import LI12122

mCaixaFlutuante :: [(Peca, Coordenadas)]
mCaixaFlutuante = 
  [ (Bloco,(0,3)),
    (Bloco,(0,2)),
    (Porta,(1,2)),
    (Bloco,(1,3)),
    (Bloco,(2,3)),
    (Bloco,(3,3)),
    (Bloco,(0,1)),
    (Bloco,(3,2)),
    (Caixa,(2,1))
  ]

mChaoDescontinuo :: [(Peca, Coordenadas)]
mChaoDescontinuo = 
  [ (Bloco,(0,3)),
    (Bloco,(1,3)),
    (Porta,(0,2)),
    (Bloco,(1,4)),
    (Bloco,(2,4)),
    (Bloco,(3,3)),
    (Bloco,(4,1))
  ]

m1 :: [(Peca, Coordenadas)]
m1 =
  [ (Porta, (0, 3)),
    (Bloco, (0, 4)),
    (Bloco, (1, 4)),
    (Bloco, (2, 4)),
    (Bloco, (3, 4)),
    (Bloco, (4, 4)),
    (Caixa, (4, 3)),
    (Bloco, (5, 4)),
    (Bloco, (6, 4)),
    (Bloco, (6, 3)),
    (Bloco, (6, 2)),
    (Bloco, (6, 1))
  ]

m1r :: Mapa
m1r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
  ]

m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (6, 0) Oeste False)

m1e2 :: Jogo
m1e2 = Jogo m1r (Jogador (2, 3) Oeste False)

m1e3 :: Jogo
m1e3 = Jogo m1r (Jogador (4, 2) Este True)

m2r :: Mapa
m2r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio],
    [Porta, Vazio, Vazio, Caixa, Vazio, Vazio, Bloco, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio]
  ]

m3r :: Mapa
m3r =
  [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
    [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio],
    [Vazio, Vazio, Vazio, Caixa, Vazio, Vazio, Bloco, Vazio],
    [Porta, Vazio, Vazio, Caixa, Vazio, Vazio, Bloco, Vazio],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio]
  ]

m2e1 :: Jogo
m2e1 = Jogo m2r (Jogador (7,1) Oeste False)

m3e1 :: Jogo 
m3e1 = Jogo m3r (Jogador (4,5) Oeste False)