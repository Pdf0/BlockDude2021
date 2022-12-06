{- |
Module      : Main
Description : Construção/Desconstrução do mapa
Copyright   : Pedro Eira de Sousa <a100823@alunos.uminho.pt>;
            : Vicente Costa Martins <a100713@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.

== Bibliografia
. https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss.html
. https://hackage.haskell.org/package/gloss-1.13.2.1/docs/Graphics-Gloss-Interface-IO-Game.html
. https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html
. https://hackage.haskell.org/package/directory-1.3.7.0/docs/System-Directory.html
. https://www.youtube.com/playlist?list=PLadvWyx_6w6XiJ95A4MqSfmIaRVbXWFGS

== Introdução:
  Nesta introução, pretendemos explicar melhor alguns aspetos do jogo.
  O jogo é composto por dois modos: um Modo Campanha e um Modo Casual. No Modo Campanha, o jogador escolje um pokemon inicial e tem 5 níveis para completar. Em cada nível é gerado um pokemon aleatório dependendo do nível em questão e este vai ter a mesma função da 'Porta'. No fim dos 5 níveis, são apresentados os 6 pokemons ao jogador (o seu pokeomon inicial e os 5 de cada nível), fazendo com que cada experiência no Modo Campanha resulte numa equipa diferente. No Modo Casual é possível escolhermos o nível que queremos jogar, voltando ao menu se completarmos o nível, ou se pressioarmos a tecla Esc.
  No Modo Campanha exite também um sistema de save automático, ou seja, o jogo é guardado sempre que o jogador pressiona a tecla Esc, podendo fechar o jogo e voltar a jogá-lo escolhendo a opção "Continuar" no menu do Modo Campanha. Caso não exista nenhum save, uma mensagem de erro é apresentada.
-}

module Main where

import LI12122
import Tarefa2_2021li1g054
import Tarefa4_2021li1g054
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import System.Exit
import Data.Maybe
import Data.Char
import System.Random
import System.Directory

-- | Uma representação do estado do Jogo. (Janela onde se encontra, o jogo atual, o nivel inicial)
type Estado = (Janela,Jogo,Jogo)
-- | 
type EstadoGloss = (Estado, [Picture], Float,[Int])
-- | Opções do menu do Modo Campanha.
data Opcoes = Continuar -- ^Continuar o último Jogo guardado no ficheiro 'save.txt'.
            | ContinuarErro -- ^Se não existeir o ficheiro 'save.txt', então uma mensagem de erro aparece.
            | NovoJogo -- ^Começar um novo Jogo.
            | Sair1 -- ^Sair do jogo.

data Menu = Campanha -- ^Avançar para o modo Campanha.
          | Casual -- ^Avançar para o modo Casual.
          | Sair -- ^Sair do Jogo.

data Nivel = Nivel1
           | Nivel2
           | Nivel3
           | Nivel4
           | Nivel5

data Personagem = Charmander
                | Bulbasaur
                | Squirtle

data Janela = Controlador Menu -- ^Menu Principal.
            | Pause Opcoes -- ^Menu do Modo Campanha.
            | Escolher Personagem -- ^Menu para escolher o Pokemon Inicial.
            | EscolherNivel Nivel -- ^Menu para escolher o Nivel do Modo Casual.
            | ModoCampanha Nivel -- ^Modo Campanha.
            | ModoCasual Nivel -- ^Modo Casual.
            | VenceuJogo -- ^Cena final quando o Modo Camapanha é terminado.
-- | Lista dos níveis disponíveis.
niveis :: [Jogo]
niveis = [m1e1,m2e1,m3e1,m4e1,m5e1]
-- | Estado inicial
estadoGlossInicial :: [Picture] -> EstadoGloss
estadoGlossInicial p = (estadoInicial,p, 0.0,[])
-- | Estado Inicial
estadoInicial :: Estado
estadoInicial = (Controlador Campanha,nivelAtual 0 niveis,nivelAtual 0 niveis)
-- | Escolhe o nível atual
nivelAtual :: Int -- ^Índice do jogo
           -> [Jogo] -- ^Lista dos jogos
           -> Jogo  -- ^Jogo pretendido
nivelAtual x m = m !! x
-- | Desenha o estado atual do jogo
draw :: EstadoGloss -- ^Estado a desenhar
     -> IO Picture -- ^Estado desenhado
draw ((Controlador Campanha,jogoAtual,jogoInicial),imagens,time,equipa) =
  return $ Pictures [imagem "background" imagens,Translate 0 (-80) (imagem "modocasual" imagens),Translate 0 50 (imagem "modocampanha" imagens),Translate 0 (-220) (imagem "sair" imagens),Translate 0 300 (imagem "title1" imagens),Scale 0.5 0.5 $ Translate (-960) 135 (imagem "arrowright" imagens)]
draw ((Controlador Casual,jogoAtual,jogoInicial),imagens,time,equipa) =
   return $ Pictures [imagem "background" imagens,Translate 0 (-80) (imagem "modocasual" imagens),Translate 0 50 (imagem "modocampanha" imagens),Translate 0 (-220) (imagem "sair" imagens),Translate 0 300 (imagem "title1" imagens),Scale 0.5 0.5 $ Translate (-840) (-145) (imagem "arrowright" imagens)]
draw ((Controlador Sair,jogoAtual,jogoInicial),imagens,time,equipa) =
   return $ Pictures [imagem "background" imagens,Translate 0 (-80) (imagem "modocasual" imagens),Translate 0 50 (imagem "modocampanha" imagens),Translate 0 (-220) (imagem "sair" imagens),Translate 0 300 (imagem "title1" imagens),Scale 0.5 0.5 $ Translate (-400) (-410) (imagem "arrowright" imagens)]
draw ((Escolher Charmander, jogoAtual,jogoInicial),imagens,time,equipa) =
  drawPokemonAnimado ((Escolher Charmander, jogoAtual,jogoInicial),imagens,time,equipa)
draw ((Escolher Bulbasaur, jogoAtual,jogoInicial),imagens,time,equipa) =
  drawPokemonAnimado ((Escolher Bulbasaur, jogoAtual,jogoInicial),imagens,time,equipa)
draw ((Escolher Squirtle, jogoAtual,jogoInicial),imagens,time,equipa) =
  drawPokemonAnimado ((Escolher Squirtle, jogoAtual,jogoInicial),imagens,time,equipa)
draw ((Pause Continuar,jogoAtual,jogoInicial),imagens,time,equipa) =
  return $ Pictures [imagem "background" imagens,Translate 0 50 (imagem "continuar" imagens),Translate 0 (-75) (imagem "novojogo" imagens),Translate 0 (-200) (imagem "sair" imagens),Translate 0 300 (imagem "title1" imagens),Scale 0.5 0.5 $ Translate (-710) 130 (imagem "arrowright" imagens)]
draw ((Pause ContinuarErro,jogoAtual,jogoInicial),imagens,time,equipa) =
  return $ Pictures [imagem "background" imagens,Translate 0 50 (imagem "continuar" imagens),Translate 0 (-75) (imagem "novojogo" imagens),Translate 0 (-200) (imagem "sair" imagens),Translate 0 300 (imagem "title1" imagens),Scale 0.5 0.5 $ Translate (-710) 130 (imagem "arrowright" imagens),Translate 550 50 (Scale 0.4 0.4 (imagem "savenaoencontrado" imagens))]
draw ((Pause NovoJogo,jogoAtual,jogoInicial),imagens,time,equipa) =
  return $ Pictures [imagem "background" imagens,Translate 0 50 (imagem "continuar" imagens),Translate 0 (-75) (imagem "novojogo" imagens),Translate 0 (-200) (imagem "sair" imagens),Translate 0 300 (imagem "title1" imagens),Scale 0.5 0.5 $ Translate (-710) (-110) (imagem "arrowright" imagens)]
draw ((Pause Sair1,jogoAtual,jogoInicial),imagens,time,equipa) =
  return $ Pictures [imagem "background" imagens,Translate 0 50 (imagem "continuar" imagens),Translate 0 (-75) (imagem "novojogo" imagens),Translate 0 (-200) (imagem "sair" imagens),Translate 0 300 (imagem "title1" imagens),Scale 0.5 0.5 $ Translate (-400) (-370) (imagem "arrowright" imagens)]

draw ((ModoCasual n,jogoAtual,jogoInicial),imagens,time,equipa) =
  return $ Pictures $ imagem "background" imagens : drawJogoCasual [imagem "traineroeste" imagens,imagem "trainereste" imagens,imagem "caixa" imagens,imagem "terrarelva" imagens,imagem "porta" imagens,imagem "terra" imagens] jogoAtual
draw ((EscolherNivel n,jogoAtual,jogoInicial),imagens,time,equipa) =
  return $ drawCasual n imagens
draw ((VenceuJogo,jogoAtual,jogoInicial),imagens,time,equipa) =
  drawVenceuJogo imagens equipa

draw ((ModoCampanha Nivel1,jogoAtual,jogoInicial),imagens,time,equipa) =
    drawJogoCampanha equipa 1 imagens jogoAtual
draw ((ModoCampanha Nivel2,jogoAtual,jogoInicial),imagens,time,equipa) =
    drawJogoCampanha equipa 2 imagens jogoAtual
draw ((ModoCampanha Nivel3,jogoAtual,jogoInicial),imagens,time,equipa) =
    drawJogoCampanha equipa 3 imagens jogoAtual
draw ((ModoCampanha Nivel4,jogoAtual,jogoInicial),imagens,time,equipa) =
    drawJogoCampanha equipa 4 imagens jogoAtual
draw ((ModoCampanha Nivel5,jogoAtual,jogoInicial),imagens,time,equipa) =
    drawJogoCampanha equipa 5 imagens jogoAtual
-- | Desenha o Estado 'VenceuJogo', de acordo com os índices dados
drawVenceuJogo :: [Picture] -- ^Lista de texturas
               -> [Int] -- ^Indice dos pokemons a escolher
               -> IO Picture -- ^Resultado
drawVenceuJogo imagens equipa =
  do pokemons <- extraiPokemons imagens equipa
     drawVenceuJogo' (imagem "background" imagens:imagem "parabens" imagens:imagem "primeenter" imagens:pokemons)
  where
    drawVenceuJogo' :: [Picture] -> IO Picture
    drawVenceuJogo' (background:frase1:frase2:pokemons) =
      return $ Pictures $ background : Translate 0 350  (Scale 0.8 0.8 frase1) : drawPokemon1 ((!!) pokemons 0) : Translate 0 (-450) (Scale 0.6 0.6 frase2) : drawPokemon2 ((!!) pokemons 5) : drawPokemon3 ((!!) pokemons 4) : drawPokemon4 ((!!) pokemons 3) : drawPokemon5 ((!!) pokemons 2) : [drawPokemon6 ((!!) pokemons 1)]
      where
        drawPokemon1 :: Picture -> Picture
        drawPokemon1 pokemon = Translate (-250) 150 $ Scale 0.3 0.3 pokemon
        drawPokemon2 :: Picture -> Picture
        drawPokemon2 pokemon = Translate 0 150 $ Scale 0.3 0.3 pokemon
        drawPokemon3 :: Picture -> Picture
        drawPokemon3 pokemon = Translate 250 150 $ Scale 0.3 0.3 pokemon
        drawPokemon4 :: Picture -> Picture
        drawPokemon4 pokemon = Translate (-250) (-150) $ Scale 0.3 0.3 pokemon
        drawPokemon5 :: Picture -> Picture
        drawPokemon5 pokemon = Translate 0 (-150) $ Scale 0.3 0.3 pokemon
        drawPokemon6 :: Picture -> Picture
        drawPokemon6 pokemon = Translate 250 (-150) $ Scale 0.3 0.3 pokemon
-- | Escolhe os pokemons a serem usados no Estado 'VenceuJogo', de acordo com os índices dados
extraiPokemons :: [Picture] -- ^Lista de texturas
               -> [Int] -- ^Indices dos pokemons a escolher
               -> IO [Picture] -- ^Lista de pokemons já desenhados
extraiPokemons imagens (p5:p4:p3:p2:p1:[pInicial]) = return $ pokemonInicial : pokemon5 : pokemon4 : pokemon3 : pokemon2 : [pokemon1]
  where
    pokemonInicial = (!!) [imagem "charmander1" imagens,imagem "bulbasaur1" imagens,imagem "squirtle1" imagens] (pInicial-1)
    pokemon5 = (!!) [imagem "articuno" imagens,imagem "dragonite" imagens,imagem "mewtwo" imagens,imagem "moltres" imagens,imagem "zapdos" imagens] p5
    pokemon4 = (!!) [imagem "alakazam" imagens,imagem "arcanine" imagens,imagem "blastoise" imagens,imagem "charizard" imagens,imagem "gyarados" imagens,imagem "lapras" imagens,imagem "machamp" imagens,imagem "magmar" imagens,imagem "rhydon" imagens,imagem "venusaur" imagens] p4
    pokemon3 = (!!) [imagem "fearow" imagens,imagem "golem" imagens,imagem "hitmonchan" imagens,imagem "machoque" imagens,imagem "nidoking" imagens,imagem "nidoqueen" imagens,imagem "onyx" imagens,imagem "pidgeot" imagens,imagem "polywrath" imagens,imagem "sandslash" imagens] p3
    pokemon2 = (!!) [imagem "abra" imagens,imagem "geodude" imagens,imagem "mankey" imagens,imagem "meow" imagens,imagem "pikachu" imagens,imagem "ponyta" imagens,imagem "psyduck" imagens,imagem "tentacool" imagens,imagem "vulpix" imagens,imagem "zubat" imagens] p2
    pokemon1 = (!!) [imagem "butterfie" imagens,imagem "caterpie" imagens,imagem "ekans" imagens,imagem "kakuna" imagens,imagem "magikarp" imagens,imagem "metapod" imagens,imagem "pidgey" imagens,imagem "rattata" imagens,imagem "spearow" imagens,imagem "weedle" imagens] p1
-- | Desenha o Estado 'Escolher', usando tempo para desenhar animações
drawPokemonAnimado :: EstadoGloss -- ^Estado a desenhar
                   -> IO Picture -- ^Estado desenhado
drawPokemonAnimado ((Escolher Charmander,jogoAtual,jogoInicial),imagens,time,equipa)
 | mod (milissecondsPassed) 200 < 100 = return $ Pictures $ imagem "background" imagens : [Scale 0.5 0.5 $ Pictures [Translate (-900) (-350) (imagem "charmander" imagens),Translate 900 (-350) (imagem "squirtle" imagens),Translate 0 (-350) (imagem "bulbasaur" imagens), Translate (-1000) 250 (imagem "arrowdown" imagens),Scale 1.5 1.5 $ Translate 50 480 (imagem "escolheopokemon" imagens),Translate (-1000) 0 (imagem "charmandernome" imagens)]]
 | otherwise = return $ Pictures $ imagem "background" imagens : [Scale 0.5 0.5 $ Pictures [Translate (-900) (-400) (imagem "charmander" imagens),Translate 900 (-350) (imagem "squirtle" imagens),Translate 0 (-350) (imagem "bulbasaur" imagens), Translate (-1000) 250 (imagem "arrowdown" imagens),Scale 1.5 1.5 $ Translate 50 480 (imagem "escolheopokemon" imagens),Translate (-1000) 0 (imagem "charmandernome" imagens)]]
 where milissecondsPassed = round (time*1000)

drawPokemonAnimado ((Escolher Bulbasaur,jogoAtual,jogoInicial),imagens,time,equipa)
 | mod (milissecondsPassed) 200 < 100 = return $ Pictures $ imagem "background" imagens : [Scale 0.5 0.5 $ Pictures [Translate (-900) (-350) (imagem "charmander" imagens),Translate 900 (-350) (imagem "squirtle" imagens),Translate 0 (-350) (imagem "bulbasaur" imagens), Translate 0 250 (imagem "arrowdown" imagens),Scale 1.5 1.5 $ Translate 50 480 (imagem "escolheopokemon" imagens),imagem "bulbasaurnome" imagens]]
 | otherwise = return $ Pictures $ imagem "background" imagens : [Scale 0.5 0.5 $ Pictures [Translate (-900) (-350) (imagem "charmander" imagens),Translate 900 (-350) (imagem "squirtle" imagens),Translate 0 (-400) (imagem "bulbasaur" imagens), Translate 0 250 (imagem "arrowdown" imagens),Scale 1.5 1.5 $ Translate 50 480 (imagem "escolheopokemon" imagens),imagem "bulbasaurnome" imagens]]
 where milissecondsPassed = round (time*1000)

drawPokemonAnimado ((Escolher Squirtle,jogoAtual,jogoInicial),imagens,time,equipa)
 | mod (milissecondsPassed) 200 < 100 = return $ Pictures $ imagem "background" imagens : [Scale 0.5 0.5 $ Pictures [Translate (-900) (-350) (imagem "charmander" imagens),Translate 900 (-350) (imagem "squirtle" imagens),Translate 0 (-350) (imagem "bulbasaur" imagens), Translate 850 250 (imagem "arrowdown" imagens),Scale 1.5 1.5 $ Translate 50 480 (imagem "escolheopokemon" imagens),Translate 850 0 (imagem "squirtlenome" imagens)]]
 | otherwise = return $ Pictures $ imagem "background" imagens : [Scale 0.5 0.5 $ Pictures [Translate (-900) (-350) (imagem "charmander" imagens),Translate 900 (-400) (imagem "squirtle" imagens),Translate 0 (-350) (imagem "bulbasaur" imagens), Translate 850 250 (imagem "arrowdown" imagens),Scale 1.5 1.5 $ Translate 50 480 (imagem "escolheopokemon" imagens),Translate 850 0 (imagem "squirtlenome" imagens)]]
 where milissecondsPassed = round (time*1000)
-- | Desenha o menu do Modo Casual
drawCasual :: Nivel -- ^Nível que se pretende desenhar
           -> [Picture] -- ^Lista de texturas
           -> Picture -- ^Nível já desenhado
drawCasual Nivel1 imagens =
   Pictures [imagem "background" imagens,Translate 0 300 (imagem "title1" imagens),imagem "nivel1" imagens,Translate 0 (-200) (imagem "jogar" imagens),Translate 300 10 $ Scale 0.5 0.5 (imagem "arrowright" imagens),Translate (-180) (-140) rect]
drawCasual Nivel2 imagens =
   Pictures [imagem "background" imagens,Translate 0 300 (imagem "title1" imagens),imagem "nivel2" imagens,Translate 0 (-200) (imagem "jogar" imagens),Translate 300 10 $ Scale 0.5 0.5 (imagem "arrowright" imagens),Translate (-300) 10 $ Scale 0.5 0.5 (imagem "arrowleft" imagens),Translate (-180) (-140) rect]
drawCasual Nivel3 imagens =
   Pictures [imagem "background" imagens,Translate 0 300 (imagem "title1" imagens),imagem "nivel3" imagens,Translate 0 (-200) (imagem "jogar" imagens),Translate 300 10 $ Scale 0.5 0.5 (imagem "arrowright" imagens),Translate (-300) 10 $ Scale 0.5 0.5 (imagem "arrowleft" imagens),Translate (-180) (-140) rect]
drawCasual Nivel4 imagens =
   Pictures [imagem "background" imagens,Translate 0 300 (imagem "title1" imagens),imagem "nivel4" imagens,Translate 0 (-200) (imagem "jogar" imagens),Translate 300 10 $ Scale 0.5 0.5 (imagem "arrowright" imagens),Translate (-300) 10 $ Scale 0.5 0.5 (imagem "arrowleft" imagens),Translate (-180) (-140) rect]
drawCasual Nivel5 imagens =
   Pictures [imagem "background" imagens,Translate 0 300 (imagem "title1" imagens),imagem "nivel5" imagens,Translate 0 (-200) (imagem "jogar" imagens),Translate (-300) 10 $ Scale 0.5 0.5 (imagem "arrowleft" imagens),Translate (-180) (-140) rect]
-- | Desenha um retângulo
rect :: Picture
rect = Pictures [Polygon [(0,0),(0,5),(350,5),(350,0)],Polygon [(0,-100),(0,-95),(350,-95),(350,-100)],Polygon [(0,-100),(5,-100),(5,0),(0,0)],Polygon [(350,-100),(355,-100),(355,5),(350,5)]]
-- | Desenha o Estado 'ModoCasual'
drawJogoCasual :: [Picture] -- ^Lista de Texturas
               -> Jogo -- ^Jogo a desenhar
               -> [Picture] -- ^Jogo já desenhado
drawJogoCasual [p13,p14,p15,p16,p17,p19] (Jogo m (Jogador (x,y) d c)) = map (Translate (fromIntegral (fst (findMiddle m) * (-50))) (fromIntegral (snd (findMiddle m)) * 50)) $  drawJogador [p13,p14,p15] (Jogador (x,y) d c) ++ drawMapaCasual [p15,p16,p17,p19] (desconstroiMapa m) (desconstroiMapa m)
-- | Desenha o Estado 'ModoCampanha'. Cada nível tem uma lista de pokemons que podem ser usados como porta naquele nível, e um é escolhido aleatoriamente. Este é duardado para depois ser colocado no Estado 'VenceuJogo'.
drawJogoCampanha :: [Int] -> Int -> [Picture] -> Jogo -> IO Picture
drawJogoCampanha equipa nivel imagens jogo
  | nivel == 1 = return $ Pictures $ imagem "background" imagens : drawJogoCampanha' x (pokemonsNivel1 imagens) (pecas imagens) jogo
  | nivel == 2 = return $ Pictures $ imagem "background" imagens : drawJogoCampanha' x (pokemonsNivel2 imagens) (pecas imagens) jogo
  | nivel == 3 = return $ Pictures $ imagem "background" imagens : drawJogoCampanha' x (pokemonsNivel3 imagens) (pecas imagens) jogo
  | nivel == 4 = return $ Pictures $ imagem "background" imagens : drawJogoCampanha' x (pokemonsNivel4 imagens) (pecas imagens) jogo
  | nivel == 5 = return $ Pictures $ imagem "background" imagens : drawJogoCampanha' x (pokemonsNivel5 imagens) (pecas imagens) jogo
    where
      pokemonsNivel1 imagens = [imagem "butterfie" imagens,imagem "caterpie" imagens,imagem "ekans" imagens,imagem "kakuna" imagens,imagem "magikarp" imagens,imagem "metapod" imagens,imagem "pidgey" imagens,imagem "rattata" imagens,imagem "spearow" imagens,imagem "weedle" imagens]
      pokemonsNivel2 imagens = [imagem "abra" imagens,imagem "geodude" imagens,imagem "mankey" imagens,imagem "meow" imagens,imagem "pikachu" imagens,imagem "ponyta" imagens,imagem "psyduck" imagens,imagem "tentacool" imagens,imagem "vulpix" imagens,imagem "zubat" imagens]
      pokemonsNivel3 imagens = [imagem "fearow" imagens,imagem "golem" imagens,imagem "hitmonchan" imagens,imagem "machoque" imagens,imagem "nidoking" imagens,imagem "nidoqueen" imagens,imagem "onyx" imagens,imagem "pidgeot" imagens,imagem "polywrath" imagens,imagem "sandslash" imagens]
      pokemonsNivel4 imagens = [imagem "alakazam" imagens,imagem "arcanine" imagens,imagem "blastoise" imagens,imagem "charizard" imagens,imagem "gyarados" imagens,imagem "lapras" imagens,imagem "machamp" imagens,imagem "magmar" imagens,imagem "rhydon" imagens,imagem "venusaur" imagens]
      pokemonsNivel5 imagens = [imagem "articuno" imagens,imagem "dragonite" imagens,imagem "mewtwo" imagens,imagem "moltres" imagens,imagem "zapdos" imagens]
      pecas imagens = [imagem "traineroeste" imagens,imagem "trainereste" imagens,imagem "caixa" imagens,imagem "terrarelva" imagens,imagem "porta" imagens,imagem "terra" imagens]
      x = head equipa
-- | Desenha o mapa no Estado 'ModoCampanha', sem os pokemons 
drawJogoCampanha' :: Int -- ^Índice do pokemon
                  -> [Picture] -- ^Lista de Imagens dos pokemons do respetivo 'Nivel'
                  -> [Picture] -- ^Lista das texturas necessárias para desenhar o mapa
                  -> Jogo -- ^Jogo a desenhar
                  -> [Picture] -- ^Jogo já desenhado
drawJogoCampanha' indice pokemons [p13,p14,p15,p16,p17,p19] (Jogo m (Jogador (x,y) d c)) = map (Translate (fromIntegral (fst (findMiddle m) * (-50))) (fromIntegral (snd (findMiddle m)) * 50)) $  drawJogador [p13,p14,p15] (Jogador (x,y) d c) ++ drawMapaCampanha indice pokemons [p15,p16,p17,p19] (desconstroiMapa m) (desconstroiMapa m)
-- | Desenha o Jogador
drawJogador :: [Picture] -- ^Lista com o Jogador e com a Caixa
            -> Jogador -- ^Jogador a desenhar
            -> [Picture] -- ^Jogador já desenhado
drawJogador [p13,p14,p15] (Jogador (x,y) Oeste False) = [Translate (50*fromIntegral x) ((-50)*fromIntegral y) $ Scale 1.1 1.1 p13]
drawJogador [p13,p14,p15] (Jogador (x,y) Este False) = [Translate (50*fromIntegral x) ((-50)*fromIntegral y) $ Scale 1.1 1.1 p14]
drawJogador [p13,p14,p15] (Jogador (x,y) Oeste True) = [Translate (50*fromIntegral x) ((-50)*fromIntegral y) $ Scale 1.1 1.1 p13,Translate (50*fromIntegral x) (((-50)*fromIntegral y)+50) p15]
drawJogador [p13,p14,p15] (Jogador (x,y) Este True) = [Translate (50*fromIntegral x) ((-50)*fromIntegral y) $ Scale 1.1 1.1 p14,Translate (50*fromIntegral x) (((-50)*fromIntegral y)+50) p15]
-- | Desenha o mapa do Estado 'ModoCasual'
drawMapaCasual :: [Picture] -- ^Lista de texturas necessárias para desenhar o mapa
               -> [(Peca,Coordenadas)] -- ^Mapa desconstruído uqe vai servir de suporte para verificarmos se existe uma peça no mapa original
               -> [(Peca,Coordenadas)] -- ^Mapa desconstruído que vai ser desenhado 
               -> [Picture] -- ^Mapa já desenhado
drawMapaCasual [p15,p16,p17,p19] m ((p,(x,y)):t) | p == Bloco = if elem (Bloco,(x,y-1)) m then Translate (50 * fromIntegral x) (50*(- (fromIntegral y))) (Scale 0.6 0.6 p19) : drawMapaCasual [p15,p16,p17,p19] m t else Translate (50 * fromIntegral x) (50*(- (fromIntegral y))) (Scale 0.6 0.6 p16) : drawMapaCasual [p15,p16,p17,p19] m t
                                                 | p == Caixa = Translate (50 * fromIntegral x) (50*(- (fromIntegral y))) p15 : drawMapaCasual [p15,p16,p17,p19] m t
                                                 | p == Porta = Translate (50 * fromIntegral x) (50*(- (fromIntegral y))) p17 : drawMapaCasual [p15,p16,p17,p19] m t
                                                 | otherwise = drawMapaCasual [p15,p16,p17,p19] m t
drawMapaCasual _ _ _ = []
-- | Desenha o mapa do Estado 'ModoCampanha', desenhando só os blocos, as caixas, e o pokemon que vai servir de porta.
drawMapaCampanha :: Int -- ^Índice do pokemon
                 -> [Picture] -- ^Lista de Imagens dos pokemons do respetivo 'Nível' 
                 -> [Picture] -- ^Lista das texturas necessárias para desenhar o mapa
                 -> [(Peca,Coordenadas)] -- ^Mapa desconstruído uqe vai servir de suporte para verificarmos se existe uma peça no mapa original
                 -> [(Peca,Coordenadas)] -- ^Mapa desconstruído que vai ser desenhado 
                 -> [Picture] -- ^Mapa já desenhado
drawMapaCampanha indice pokemons [p15,p16,p17,p19] m ((p,(x,y)):t)
                                                   | p == Bloco = if elem (Bloco,(x,y-1)) m then Translate (50 * fromIntegral x) (50*(- (fromIntegral y))) (Scale 0.6 0.6 p19) : drawMapaCampanha indice pokemons [p15,p16,p17,p19] m t else Translate (50 * fromIntegral x) (50*(- (fromIntegral y))) (Scale 0.6 0.6 p16) : drawMapaCampanha indice pokemons [p15,p16,p17,p19] m t
                                                   | p == Caixa = Translate (50 * fromIntegral x) (50*(- (fromIntegral y))) p15 : drawMapaCampanha indice pokemons [p15,p16,p17,p19] m t
                                                   | p == Porta = Translate (50 * fromIntegral x) (50*(- (fromIntegral y))) (Scale 0.1 0.1 ((!!) pokemons indice)) : drawMapaCampanha indice pokemons [p15,p16,p17,p19] m t
                                                   | otherwise = drawMapaCampanha indice pokemons [p15,p16,p17,p19] m t
drawMapaCampanha _ _ _ _ _ = []
-- | Encontra as coordenadas do meio do mapa
findMiddle :: Mapa -- ^Mapa
           -> (Int,Int) -- ^Coordenadas do meio do mapa
findMiddle (h:t) = (length h `div` 2,length (h:t) `div` 2)
-- | Altera o Estado dependendo do evento
event :: Event -- ^Evento
      -> EstadoGloss -- ^Estado 
      -> IO EstadoGloss -- ^Novo Estado
event (EventKey (SpecialKey KeyDown) Down _ _) ((Controlador Campanha,jogoAtual,jogoInicial),textures,time,equipa) = return ((Controlador Casual,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyUp) Down _ _) ((Controlador Campanha,jogoAtual,jogoInicial),textures,time,equipa) = return ((Controlador Sair,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyDown) Down _ _) ((Controlador Casual,jogoAtual,jogoInicial),textures,time,equipa) = return ((Controlador Sair,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyUp) Down _ _) ((Controlador Casual,jogoAtual,jogoInicial),textures,time,equipa) = return ((Controlador Campanha,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyDown) Down _ _) ((Controlador Sair,jogoAtual,jogoInicial),textures,time,equipa) = return ((Controlador Campanha,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyUp) Down _ _) ((Controlador Sair,jogoAtual,jogoInicial),textures,time,equipa) = return ((Controlador Casual,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyEnter) Down _ _) ((Controlador Campanha,jogoAtual,jogoInicial),textures,time,equipa) = return ((Pause Continuar,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyEnter) Down _ _) ((Controlador Casual,jogoAtual,jogoInicial),textures,time,equipa) = return ((EscolherNivel Nivel1,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyEnter) Down _ _) ((Controlador Sair,jogoAtual,jogoInicial),textures,time,equipa) = exitSuccess

event (EventKey (SpecialKey KeyRight) Down _ _) ((Escolher Charmander,jogoAtual,jogoInicial),textures,time,equipa) = return ((Escolher Bulbasaur,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyRight) Down _ _) ((Escolher Bulbasaur,jogoAtual,jogoInicial),textures,time,equipa) = return ((Escolher Squirtle,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyRight) Down _ _) ((Escolher Squirtle,jogoAtual,jogoInicial),textures,time,equipa) = return ((Escolher Charmander,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyLeft) Down _ _) ((Escolher Charmander,jogoAtual,jogoInicial),textures,time,equipa) = return ((Escolher Squirtle,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyLeft) Down _ _) ((Escolher Bulbasaur,jogoAtual,jogoInicial),textures,time,equipa) = return ((Escolher Charmander,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyLeft) Down _ _) ((Escolher Squirtle,jogoAtual,jogoInicial),textures,time,equipa) = return ((Escolher Bulbasaur,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyEnter) Down _ _) ((Escolher Charmander,jogoAtual,jogoInicial),textures,time,equipa) = do x <- randomRIO (0,9)
                                                                                                                        return ((ModoCampanha Nivel1,jogoAtual,jogoInicial),textures,time,[x,1])
event (EventKey (SpecialKey KeyEnter) Down _ _) ((Escolher Squirtle,jogoAtual,jogoInicial),textures,time,equipa) = do x <- randomRIO (0,9)
                                                                                                                      return ((ModoCampanha Nivel1,jogoAtual,jogoInicial),textures,time,[x,2])
event (EventKey (SpecialKey KeyEnter) Down _ _) ((Escolher Bulbasaur,jogoAtual,jogoInicial),textures,time,equipa) = do x <- randomRIO (0,9)
                                                                                                                       return ((ModoCampanha Nivel1,jogoAtual,jogoInicial),textures,time,[x,3])


event (EventKey (SpecialKey KeyRight) Down _ _ ) ((EscolherNivel Nivel1,jogoAtual,jogoInicial),textures,time,equipa) = return ((EscolherNivel Nivel2,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyRight) Down _ _ ) ((EscolherNivel Nivel2,jogoAtual,jogoInicial),textures,time,equipa) = return ((EscolherNivel Nivel3,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyLeft) Down _ _ ) ((EscolherNivel Nivel2,jogoAtual,jogoInicial),textures,time,equipa) = return ((EscolherNivel Nivel1,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyRight) Down _ _ ) ((EscolherNivel Nivel3,jogoAtual,jogoInicial),textures,time,equipa) = return ((EscolherNivel Nivel4,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyLeft) Down _ _ ) ((EscolherNivel Nivel3,jogoAtual,jogoInicial),textures,time,equipa) = return ((EscolherNivel Nivel2,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyRight) Down _ _ ) ((EscolherNivel Nivel4,jogoAtual,jogoInicial),textures,time,equipa) = return ((EscolherNivel Nivel5,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyLeft) Down _ _ ) ((EscolherNivel Nivel4,jogoAtual,jogoInicial),textures,time,equipa) = return ((EscolherNivel Nivel3,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyLeft) Down _ _ ) ((EscolherNivel Nivel5,jogoAtual,jogoInicial),textures,time,equipa) = return ((EscolherNivel Nivel4,jogoAtual,jogoInicial),textures,time,equipa)

event (EventKey (SpecialKey KeyEnter) Down _ _ ) ((EscolherNivel Nivel1,jogoAtual,jogoInicial),textures,time,equipa) = return ((ModoCasual Nivel1,nivelAtual 0 niveis,nivelAtual 0 niveis),textures,time,equipa)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) ((EscolherNivel Nivel2,jogoAtual,jogoInicial),textures,time,equipa) = return ((ModoCasual Nivel1,nivelAtual 1 niveis,nivelAtual 1 niveis),textures,time,equipa)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) ((EscolherNivel Nivel3,jogoAtual,jogoInicial),textures,time,equipa) = return ((ModoCasual Nivel1,nivelAtual 2 niveis,nivelAtual 2 niveis),textures,time,equipa)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) ((EscolherNivel Nivel4,jogoAtual,jogoInicial),textures,time,equipa) = return ((ModoCasual Nivel1,nivelAtual 3 niveis,nivelAtual 3 niveis),textures,time,equipa)
event (EventKey (SpecialKey KeyEnter) Down _ _ ) ((EscolherNivel Nivel5,jogoAtual,jogoInicial),textures,time,equipa) = return ((ModoCasual Nivel1,nivelAtual 4 niveis,nivelAtual 4 niveis),textures,time,equipa)

event (EventKey (SpecialKey KeyEsc) Down _ _) ((Pause _,jogoAtual,jogoInicial),textures,time,equipa) = return ((Controlador Campanha,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyEsc) Down _ _) ((Escolher _,jogoAtual,jogoInicial),textures,time,equipa) = return ((Controlador Campanha,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyEsc) Down _ _) ((EscolherNivel n,jogoAtual,jogoInicial),textures,time,equipa) = return ((Controlador Casual,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyEsc) Down _ _) ((ModoCampanha n,jogoAtual,jogoInicial),textures,time,equipa) = do fileexists <- doesFileExist "save/save.txt"
                                                                                                                 if fileexists
                                                                                                                 then do removeFile "save/save.txt"
                                                                                                                         saveGame (( ModoCampanha n,jogoAtual,jogoInicial),textures,time,equipa)
                                                                                                                         return ((Pause Continuar,jogoAtual,jogoInicial),textures,time,equipa)
                                                                                                                 else do saveGame (( ModoCampanha n,jogoAtual,jogoInicial),textures,time,equipa)
                                                                                                                         return ((Pause Continuar,jogoAtual,jogoInicial),textures,time,equipa)

event (EventKey (SpecialKey KeyEsc) Down _ _) ((ModoCasual _,jogoAtual,jogoInicial),textures,time,equipa) = return ((EscolherNivel Nivel1,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyDown) Down _ _) ((Pause Continuar,jogoAtual,jogoInicial),textures,time,equipa) = return ((Pause NovoJogo,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyDown) Down _ _) ((Pause NovoJogo,jogoAtual,jogoInicial),textures,time,equipa) = return ((Pause Sair1,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyDown) Down _ _) ((Pause Sair1,jogoAtual,jogoInicial),textures,time,equipa) = return ((Pause Continuar,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyUp) Down _ _) ((Pause Continuar,jogoAtual,jogoInicial),textures,time,equipa) = return ((Pause Sair1,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyUp) Down _ _) ((Pause Sair1,jogoAtual,jogoInicial),textures,time,equipa) = return ((Pause NovoJogo,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyUp) Down _ _) ((Pause NovoJogo,jogoAtual,jogoInicial),textures,time,equipa) = return ((Pause Continuar,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyEnter) Down _ _) ((Pause Sair1,jogoAtual,jogoInicial),textures,time,equipa) = exitSuccess
event (EventKey (SpecialKey KeyEnter) Down _ _) ((Pause NovoJogo,jogoAtual,jogoInicial),textures,time,equipa) = return ((Escolher Charmander,nivelAtual 0 niveis,nivelAtual 0 niveis),textures,time,equipa)
event (EventKey (SpecialKey KeyEnter) Down _ _) ((Pause Continuar,jogoAtual,jogoInicial),textures,time,equipa) = do fileexists <- doesFileExist "save/save.txt"
                                                                                                                    if fileexists
                                                                                                                    then loadGame textures time
                                                                                                                    else return ((Pause ContinuarErro,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyDown) Down _ _) ((Pause ContinuarErro,jogoAtual,jogoInicial),textures,time,equipa) = return ((Pause NovoJogo,jogoAtual,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyUp) Down _ _) ((Pause ContinuarErro,jogoAtual,jogoInicial),textures,time,equipa) = return ((Pause Sair1,jogoAtual,jogoInicial),textures,time,equipa)

event (EventKey (Char 'r' ) Down _ _) ((ModoCampanha n,jogoAtual,jogoInicial),textures,time,equipa) = return ((ModoCampanha n,jogoInicial,jogoInicial),textures,time,equipa)
event (EventKey (Char 'r' ) Down _ _) ((ModoCasual n,jogoAtual,jogoInicial),textures,time,equipa) = return ((ModoCasual n,jogoInicial,jogoInicial),textures,time,equipa)

event (EventKey (SpecialKey KeyLeft) Down _ _) ((ModoCampanha nivel,jogoAtual,jogoInicial),textures,time,equipa) = return ((ModoCampanha nivel,moveJogador jogoAtual AndarEsquerda,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyRight) Down _ _) ((ModoCampanha nivel,jogoAtual,jogoInicial),textures,time,equipa) = return ((ModoCampanha nivel,moveJogador jogoAtual AndarDireita,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyUp) Down _ _) ((ModoCampanha nivel,jogoAtual,jogoInicial),textures,time,equipa) = return ((ModoCampanha nivel,moveJogador jogoAtual Trepar,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyDown) Down _ _) ((ModoCampanha nivel,jogoAtual,jogoInicial),textures,time,equipa) = return ((ModoCampanha nivel,moveJogador jogoAtual InterageCaixa,jogoInicial),textures,time,equipa)

event (EventKey (SpecialKey KeyLeft) Down _ _) ((ModoCasual nivel,jogoAtual,jogoInicial),textures,time,equipa) = return ((ModoCasual nivel,moveJogador jogoAtual AndarEsquerda,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyRight) Down _ _) ((ModoCasual nivel,jogoAtual,jogoInicial),textures,time,equipa) = return ((ModoCasual nivel,moveJogador jogoAtual AndarDireita,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyUp) Down _ _) ((ModoCasual nivel,jogoAtual,jogoInicial),textures,time,equipa) = return ((ModoCasual nivel,moveJogador jogoAtual Trepar,jogoInicial),textures,time,equipa)
event (EventKey (SpecialKey KeyDown) Down _ _) ((ModoCasual nivel,jogoAtual,jogoInicial),textures,time,equipa) = return ((ModoCasual nivel,moveJogador jogoAtual InterageCaixa,jogoInicial),textures,time,equipa)

event (EventKey (SpecialKey KeyInsert) Down _ _) ((ModoCampanha Nivel1,jogoAtual,jogoInicial),textures,time,equipa) = do x <- randomRIO (0,9)
                                                                                                                         return ((ModoCampanha Nivel2,nivelAtual 1 niveis,nivelAtual 1 niveis),textures,time,x:equipa)
event (EventKey (SpecialKey KeyInsert) Down _ _) ((ModoCampanha Nivel2,jogoAtual,jogoInicial),textures,time,equipa) = do x <- randomRIO (0,9)
                                                                                                                         return ((ModoCampanha Nivel3,nivelAtual 2 niveis,nivelAtual 2 niveis),textures,time,x:equipa)
event (EventKey (SpecialKey KeyInsert) Down _ _) ((ModoCampanha Nivel3,jogoAtual,jogoInicial),textures,time,equipa) = do x <- randomRIO (0,9)
                                                                                                                         return ((ModoCampanha Nivel4,nivelAtual 3 niveis,nivelAtual 3 niveis),textures,time,x:equipa)
event (EventKey (SpecialKey KeyInsert) Down _ _) ((ModoCampanha Nivel4,jogoAtual,jogoInicial),textures,time,equipa) = do x <- randomRIO (0,4)
                                                                                                                         return ((ModoCampanha Nivel5,nivelAtual 4 niveis,nivelAtual 4 niveis),textures,time,x:equipa)
event (EventKey (SpecialKey KeyInsert) Down _ _) ((ModoCampanha Nivel5,jogoAtual,jogoInicial),textures,time,equipa) = return ((VenceuJogo,nivelAtual 4 niveis,nivelAtual 4 niveis),textures,time,equipa)

event _ j@((ModoCampanha Nivel1,jogoAtual,jogoInicial),textures,time,equipa) | inDoor jogoAtual = do x <- randomRIO (0,9)
                                                                                                     return ((ModoCampanha Nivel2,nivelAtual 1 niveis,nivelAtual 1 niveis),textures,time,x:equipa)
                                                                             | otherwise = return j
event _ j@((ModoCampanha Nivel2,jogoAtual,jogoInicial),textures,time,equipa) | inDoor jogoAtual = do x <- randomRIO (0,9)
                                                                                                     return ((ModoCampanha Nivel3,nivelAtual 2 niveis,nivelAtual 2 niveis),textures,time,x:equipa)
                                                                             | otherwise = return j
event _ j@((ModoCampanha Nivel3,jogoAtual,jogoInicial),textures,time,equipa) | inDoor jogoAtual = do x <- randomRIO (0,9)
                                                                                                     return ((ModoCampanha Nivel4,nivelAtual 3 niveis,nivelAtual 3 niveis),textures,time,x:equipa)
                                                                             | otherwise = return j
event _ j@((ModoCampanha Nivel4,jogoAtual,jogoInicial),textures,time,equipa) | inDoor jogoAtual = do x <- randomRIO (0,4)
                                                                                                     return ((ModoCampanha Nivel5,nivelAtual 4 niveis,nivelAtual 4 niveis),textures,time,x:equipa)
                                                                             | otherwise = return j
event _ j@((ModoCampanha Nivel5,jogoAtual,jogoInicial),textures,time,equipa) | inDoor jogoAtual = do fileexists <- doesFileExist "save/save.txt"
                                                                                                     if fileexists
                                                                                                     then do removeFile "save/save.txt"
                                                                                                             return ((VenceuJogo,nivelAtual 4 niveis,nivelAtual 4 niveis),textures,time,equipa)
                                                                                                     else return ((VenceuJogo,nivelAtual 4 niveis,nivelAtual 4 niveis),textures,time,equipa)
                                                                              | otherwise = return j

event _ j@((ModoCasual n,jogoAtual,jogoInicial),textures,time,equipa) | inDoor jogoAtual = return ((EscolherNivel Nivel1,jogoAtual,jogoInicial),textures,time,equipa)
                                                                      | otherwise = return j

event (EventKey (SpecialKey KeyEnter) Down _ _) ((VenceuJogo,jogoAtual,jogoInicial),textures,time,equipa) = return ((Controlador Campanha,nivelAtual 0 niveis,nivelAtual 0 niveis),textures,time,[])

event _ w = return w
-- | Dá load ao Estado, ou seja, carrega o ficheiro "save.txt" e lê o Estado que lá se encontra, substituindo o Estado anterior
loadGame :: [Picture] -- ^Lista das texturas
         -> Float -- ^Tempo
         -> IO EstadoGloss -- ^Estado
loadGame textures time = do (h:t) <- readFile "save/save.txt"
                            janela <- loadJanela h
                            (mapa,resto) <- return (loadMapa t)
                            (x,resto1) <- return (loadCoord resto)
                            (y,resto2) <- return (loadCoord resto1)
                            (direcao,resto3) <- return (loadDirecao resto2)
                            (temCaixa,resto4) <- return (loadTemCaixa resto3)
                            nivelInicial <- loadNivelInicial h
                            equipa <- return $ loadEquipa resto4
                            return ((janela,Jogo mapa (Jogador (x,y) direcao temCaixa),nivelInicial),textures,time,equipa)
  where
    loadNivelInicial :: Char -> IO Jogo 
    loadNivelInicial nivel
      | nivel == '1' = return (nivelAtual 0 niveis)
      | nivel == '2' = return (nivelAtual 1 niveis)
      | nivel == '3' = return (nivelAtual 2 niveis)
      | nivel == '4' = return (nivelAtual 3 niveis)
      | otherwise = return (nivelAtual 4 niveis)
    loadJanela :: Char -> IO Janela
    loadJanela nivel
      | nivel == '1' = return (ModoCampanha Nivel1)
      | nivel == '2' = return (ModoCampanha Nivel2)
      | nivel == '3' = return (ModoCampanha Nivel3)
      | nivel == '4' = return (ModoCampanha Nivel4)
      | otherwise = return (ModoCampanha Nivel5)
    loadCoord :: String -> (Int,String) 
    loadCoord string = (read (loadCoord' string)::Int,resto string)
      where
        loadCoord' :: String -> String
        loadCoord' (x:xs) | x == ' ' = ""
                          | otherwise = x : loadCoord' xs
    loadDirecao :: String -> (Direcao,String)
    loadDirecao string = ((\(x:xs) -> if x == 'O' then Oeste else Este) string,resto string)
    loadTemCaixa :: String -> (Bool,String)
    loadTemCaixa string = ((\(x:xs) -> x == 'T') string,resto string)
    loadEquipa :: String -> [Int]
    loadEquipa l = map (\x -> read [x]::Int) l
    loadMapa :: String -> (Mapa,String)
    loadMapa string = (loadMapa' [] string,resto string)
      where
        loadMapa' :: [Peca] -> String -> Mapa
        loadMapa' ac [] = []
        loadMapa' ac (x:xs) | x == ' ' = []
                            | x == ']' = reverse ac : loadMapa' [] xs
                            | x == 'B' = loadMapa' (Bloco : ac) xs
                            | x == 'C' = loadMapa' (Caixa : ac) xs
                            | x == 'V' = loadMapa' (Vazio : ac) xs
                            | x == 'P' = loadMapa' (Porta : ac) xs
                            | otherwise = loadMapa' ac xs
-- | Corta uma String no caráter ' ' mais próximo e retorna o resto da String
resto :: String -> String 
resto (x:xs) | x == ' ' = xs
             | otherwise = resto xs
-- | Guarda o Estado no ficheiro "save.txt"
saveGame :: EstadoGloss -- ^Estado para ser guardado
         -> IO () -- ^Cria o ficheiro "save.txt" com o Estado guardado
saveGame ((ModoCampanha nivel,Jogo mapa (Jogador (x,y) direcao temCaixa),jogoInicial),textures,time,equipa) = writeFile "save/save.txt" (saveNivel nivel ++ saveMapa mapa ++ saveCoords (x,y) ++ saveDirecao direcao ++ saveTemCaixa temCaixa ++ saveEquipa equipa)
  where
    saveEquipa :: [Int] -> String
    saveEquipa [] = ""
    saveEquipa (x:xs) = show x ++ saveEquipa xs
    saveNivel :: Nivel -> String 
    saveNivel Nivel1 = "1"
    saveNivel Nivel2 = "2"
    saveNivel Nivel3 = "3"
    saveNivel Nivel4 = "4"
    saveNivel Nivel5 = "5"
    saveCoords :: Coordenadas -> String
    saveCoords (x,y) = show x ++ " " ++ show y ++ " "
    saveDirecao :: Direcao -> String
    saveDirecao Oeste = "O "
    saveDirecao Este = "E "
    saveTemCaixa :: Bool -> String
    saveTemCaixa True = "T "
    saveTemCaixa False = "F "
    saveMapa :: Mapa -> String
    saveMapa [] = " "
    saveMapa [x] = "[" ++ pecasToString x ++ " "
    saveMapa (x:xs) = ("[" ++ pecasToString x ++ ",") ++ saveMapa xs
    pecasToString :: [Peca] -> String
    pecasToString [] = []
    pecasToString [x] = case x of
      Bloco -> "B]"
      Caixa -> "C]"
      Porta -> "P]"
      Vazio -> "V]"
    pecasToString (x:xs) = case x of
      Bloco -> "B" ++ pecasToString xs
      Caixa -> "C" ++ pecasToString xs
      Porta -> "P" ++ pecasToString xs
      Vazio -> "V" ++ pecasToString xs
-- | Reage ao passar do tempo
time :: Float -> EstadoGloss -> IO EstadoGloss
time n ((janela,jogoAtual,jogoInicial),textures,time,equipa) = return ((janela,jogoAtual,jogoInicial),textures,time+n/2,equipa)
-- | Função Principal que chama o jogo
main :: IO ()
main = do imagens <- loadImagens
          playIO FullScreen -- Modo de Representação
                 (greyN 0.8) -- Côr do fundo da janela  
                 20 -- Framerate
                 (estadoGlossInicial imagens) -- Estado Inicial
                 draw -- Desenha um Estado
                 event -- Reage a um determinado evento
                 time -- Reage ao passar do tempo
-- | Carrega as texturas necessárias para o jogo
loadImagens :: IO [Picture] -- ^Lista das texturas
loadImagens = do
    p1 <- loadBMP "textures/charmander.bmp"
    p2 <- loadBMP "textures/squirtle.bmp"
    p3 <- loadBMP "textures/bulbasaur.bmp"
    p4 <- loadBMP "textures/arrowdown.bmp"
    p5 <- loadBMP "textures/blockdudetitle.bmp"
    p6 <- loadBMP "textures/escolheoteupokemoninicial.bmp"
    p7 <- loadBMP "textures/jogar.bmp"
    p8 <- loadBMP "textures/sair.bmp"
    p9 <- loadBMP "textures/charmander1.bmp"
    p10 <- loadBMP "textures/bulbasaur1.bmp"
    p11 <- loadBMP "textures/squirtle1.bmp"
    p12 <- loadBMP "textures/arrowright.bmp"
    p13 <- loadBMP "textures/traineroeste.bmp"
    p14 <- loadBMP "textures/trainereste.bmp"
    p15 <- loadBMP "textures/caixa.bmp"
    p16 <- loadBMP "textures/terrarelva.bmp"
    p17 <- loadBMP "textures/porta.bmp"
    p18 <- loadBMP "textures/pokemonBackground.bmp"
    p19 <- loadBMP "textures/terra.bmp"
    p20 <- loadBMP "textures/continuar.bmp"
    p21 <- loadBMP "textures/novojogo.bmp"
    p22 <- loadBMP "textures/modocampanha.bmp"
    p23 <- loadBMP "textures/modocasual.bmp"
    p24 <- loadBMP "textures/arrowleft.bmp"
    p25 <- loadBMP "textures/nivel1.bmp"
    p26 <- loadBMP "textures/nivel2.bmp"
    p27 <- loadBMP "textures/nivel3.bmp"
    p28 <- loadBMP "textures/nivel4.bmp"
    p29 <- loadBMP "textures/nivel5.bmp"
    p30 <- loadBMP "textures/nivel1Sprites/butterfie.bmp"
    p31 <- loadBMP "textures/nivel1Sprites/caterpie.bmp"
    p32 <- loadBMP "textures/nivel1Sprites/ekans.bmp"
    p33 <- loadBMP "textures/nivel1Sprites/kakuna.bmp"
    p34 <- loadBMP "textures/nivel1Sprites/magikarp.bmp"
    p35 <- loadBMP "textures/nivel1Sprites/metapod.bmp"
    p36 <- loadBMP "textures/nivel1Sprites/pidgey.bmp"
    p37 <- loadBMP "textures/nivel1Sprites/rattata.bmp"
    p38 <- loadBMP "textures/nivel1Sprites/spearow.bmp"
    p39 <- loadBMP "textures/nivel1Sprites/weedle.bmp"
    p40 <- loadBMP "textures/nivel2Sprites/abra.bmp"
    p41 <- loadBMP "textures/nivel2Sprites/geodude.bmp"
    p42 <- loadBMP "textures/nivel2Sprites/mankey.bmp"
    p43 <- loadBMP "textures/nivel2Sprites/meow.bmp"
    p44 <- loadBMP "textures/nivel2Sprites/pikachu.bmp"
    p45 <- loadBMP "textures/nivel2Sprites/ponyta.bmp"
    p46 <- loadBMP "textures/nivel2Sprites/psyduck.bmp"
    p47 <- loadBMP "textures/nivel2Sprites/tentacool.bmp"
    p48 <- loadBMP "textures/nivel2Sprites/vulpix.bmp"
    p49 <- loadBMP "textures/nivel2Sprites/zubat.bmp"
    p50 <- loadBMP "textures/nivel3Sprites/fearow.bmp"
    p51 <- loadBMP "textures/nivel3Sprites/golem.bmp"
    p52 <- loadBMP "textures/nivel3Sprites/hitmonchan.bmp"
    p53 <- loadBMP "textures/nivel3Sprites/machoque.bmp"
    p54 <- loadBMP "textures/nivel3Sprites/nidoking.bmp"
    p55 <- loadBMP "textures/nivel3Sprites/nidoqueen.bmp"
    p56 <- loadBMP "textures/nivel3Sprites/onyx.bmp"
    p57 <- loadBMP "textures/nivel3Sprites/pidgeot.bmp"
    p58 <- loadBMP "textures/nivel3Sprites/poliwrath.bmp"
    p59 <- loadBMP "textures/nivel3Sprites/sandslash.bmp"
    p60 <- loadBMP "textures/nivel4Sprites/alakazam.bmp"
    p61 <- loadBMP "textures/nivel4Sprites/arcanine.bmp"
    p62 <- loadBMP "textures/nivel4Sprites/blastoise.bmp"
    p63 <- loadBMP "textures/nivel4Sprites/charizard.bmp"
    p64 <- loadBMP "textures/nivel4Sprites/gyarados.bmp"
    p65 <- loadBMP "textures/nivel4Sprites/lapras.bmp"
    p66 <- loadBMP "textures/nivel4Sprites/machamp.bmp"
    p67 <- loadBMP "textures/nivel4Sprites/magmar.bmp"
    p68 <- loadBMP "textures/nivel4Sprites/rhydon.bmp"
    p69 <- loadBMP "textures/nivel4Sprites/venusaur.bmp"
    p70 <- loadBMP "textures/nivel5Sprites/articuno.bmp"
    p71 <- loadBMP "textures/nivel5Sprites/dragonite.bmp"
    p72 <- loadBMP "textures/nivel5Sprites/mewtwo.bmp"
    p73 <- loadBMP  "textures/nivel5Sprites/moltres.bmp"
    p74 <- loadBMP "textures/nivel5Sprites/zapdos.bmp"
    p75 <- loadBMP "textures/charmander800x800.bmp"
    p76 <- loadBMP "textures/bulbasaur800x800.bmp"
    p77 <- loadBMP "textures/squirtle800x800.bmp"
    p78 <- loadBMP "textures/parabens.bmp"
    p79 <- loadBMP "textures/primeenter.bmp"
    p80 <- loadBMP "textures/savenaoencontrado.bmp"
    return [p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,p31,p32,p33,p34,p35,p36,p37,p38,p39,p40,p41,p42,p43,p44,p45,p46,p47,p48,p49,p50,p51,p52,p53,p54,p55,p56,p57,p58,p59,p60,p61,p62,p63,p64,p65,p66,p67,p68,p69,p70,p71,p72,p73,p74,p75,p76,p77,p78,p79,p80]
-- | Invoca uma imagem de acordo com a String dada
imagem :: String -- ^Nome da imagem
       -> [Picture] -- ^Lista das Imagens
       -> Picture -- ^Imagem pretendida 
imagem nome imagens = case nome of
                        "charmander" -> imagens !! 0
                        "squirtle" -> imagens !! 1
                        "bulbasaur" -> imagens !! 2
                        "arrowdown" -> imagens !! 3
                        "title1" -> imagens !! 4
                        "escolheopokemon" -> imagens !! 5
                        "jogar" -> imagens !! 6
                        "sair" -> imagens !! 7
                        "charmandernome" -> imagens !! 8
                        "bulbasaurnome" -> imagens !! 9
                        "squirtlenome" -> imagens !! 10
                        "arrowright" -> imagens !! 11
                        "traineroeste" -> imagens !! 12
                        "trainereste" -> imagens !! 13
                        "caixa" -> imagens !! 14
                        "terrarelva" -> imagens !! 15
                        "porta" -> imagens !! 16
                        "background" -> imagens !! 17
                        "terra" -> imagens !! 18
                        "continuar" -> imagens !! 19
                        "novojogo" -> imagens !! 20
                        "modocampanha" -> imagens !! 21
                        "modocasual" -> imagens !! 22
                        "arrowleft" -> imagens !! 23
                        "nivel1" -> imagens !! 24
                        "nivel2" -> imagens !! 25
                        "nivel3" -> imagens !! 26
                        "nivel4" -> imagens !! 27
                        "nivel5" -> imagens !! 28
                        "butterfie" -> imagens !! 29
                        "caterpie" -> imagens !! 30
                        "ekans" -> imagens !! 31
                        "kakuna" -> imagens !! 32
                        "magikarp" -> imagens !! 33
                        "metapod" -> imagens !! 34
                        "pidgey" -> imagens !! 35
                        "rattata" -> imagens !! 36
                        "spearow" -> imagens !! 37
                        "weedle" -> imagens !! 38
                        "abra" -> imagens !! 39
                        "geodude" -> imagens !! 40
                        "mankey" -> imagens !! 41
                        "meow" -> imagens !! 42
                        "pikachu" -> imagens !! 43
                        "ponyta" -> imagens !! 44
                        "psyduck" -> imagens !! 45
                        "tentacool" -> imagens !! 46
                        "vulpix" -> imagens !! 47
                        "zubat" -> imagens !! 48
                        "fearow" -> imagens !! 49
                        "golem" -> imagens !! 50
                        "hitmonchan" -> imagens !! 51
                        "machoque" -> imagens !! 52
                        "nidoking" -> imagens !! 53
                        "nidoqueen" -> imagens !! 54
                        "onyx" -> imagens !! 55
                        "pidgeot" -> imagens !! 56
                        "polywrath" -> imagens !! 57
                        "sandslash" -> imagens !! 58
                        "alakazam" -> imagens !! 59
                        "arcanine" -> imagens !! 60
                        "blastoise" -> imagens !! 61
                        "charizard" -> imagens !! 62
                        "gyarados" -> imagens !! 63
                        "lapras" -> imagens !! 64
                        "machamp" -> imagens !! 65
                        "magmar" -> imagens !! 66
                        "rhydon" -> imagens !! 67
                        "venusaur" -> imagens !! 68
                        "articuno" -> imagens !! 69
                        "dragonite" -> imagens !! 70
                        "mewtwo" -> imagens !! 71
                        "moltres" -> imagens !! 72
                        "zapdos" -> imagens !! 73
                        "charmander1" -> imagens !! 74
                        "bulbasaur1" -> imagens !! 75
                        "squirtle1" -> imagens !! 76
                        "parabens" -> imagens !! 77
                        "primeenter" -> imagens !! 78
                        "savenaoencontrado" -> imagens !! 79

m1r :: Mapa
m1r =
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

m1e1 :: Jogo
m1e1 = Jogo m1r (Jogador (18,6) Oeste False)

inDoor :: Jogo -> Bool
inDoor (Jogo mapa (Jogador (x,y) _ _)) = doorCoords(desconstroiMapa mapa) == (x,y)
    where
        doorCoords :: [(Peca,Coordenadas)] -> Coordenadas
        doorCoords [] = (0,0)
        doorCoords ((p,c):xs) | p == Porta = c
                              | otherwise = doorCoords xs

m2r :: Mapa
m2r =
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

m2e1 :: Jogo
m2e1 = Jogo m2r (Jogador (9,6) Oeste False)

m3r :: Mapa
m3r =
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

m3e1 :: Jogo
m3e1 = Jogo m3r (Jogador (12,8) Oeste False)

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

m4e1 :: Jogo
m4e1 = Jogo m4r (Jogador (17,8) Este False)

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

m5e1 :: Jogo
m5e1 = Jogo m5r (Jogador (19,15) Este False)