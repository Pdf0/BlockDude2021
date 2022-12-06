module Tarefa1_2021li1g054_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g054
import Fixtures
import Tarefa1_2021li1g054 (validaPotencialMapa)

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa m1r" ~: True ~=? validaPotencialMapa m1
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: False ~=? validaPotencialMapa []
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: False ~=? validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))]
    , "Tarefa 1 - Teste Valida Mapa sem espaços vazios" ~: False ~=? validaPotencialMapa [(Bloco, (0,1)), (Porta,(0,0)), (Bloco, (1,1)), (Bloco, (1,0))]
    , "Tarefa 1 - Teste Valida Mapa com uma caixa a flutuar" ~: False ~=? validaPotencialMapa mCaixaFlutuante
    , "Tarefa 1 - Teste Valida Mapa com um chão inválido" ~: False ~=? validaPotencialMapa mChaoDescontinuo
    ]
