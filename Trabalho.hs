{- ==============================================================================
   UNIVERSIDADE FEDERAL DE LAVRAS - UFLA
   Departamento de Computação Aplicada
   Paradigmas de Linguagens de Programação
   
   TRABALHO PRÁTICO - PROGRAMAÇÃO FUNCIONAL
   
   GRUPO: 'O'
   
   INTEGRANTES:
   1. Arthur Fazzio Mendes | Matrícula: 202520154
   2. Livia Rezende Masson | Matrícula: [00000000]
   3. Natan Amaral Uchôa | Matrícula: 202610131
   ============================================================================== -}

{- Opção para o GHC suprimir alertas sobre pragmas não reconhecidos. Necessário
para as diretivas do Linter -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- Opção para o Linter ignorar padrão de camelCase na nomeação de funções
{-# HLINT ignore "Use camelCase" #-}
-- Opção para o Linter não sugerir `foldr`
{-# HLINT ignore "Use foldr" #-}
-- Opção para o Linter não sugerir `map`
{-# HLINT ignore "Use map" #-}

module Trabalho where

import Data.Char (toLower, isSpace) -- toUpper removido por implementação própria
import Data.List (sort)

-- 'hiding' esconde implementações
import Prelude hiding (abs, elem, head, init, last, length, negate, otherwise, tail, toUpper)

-- ATENÇÃO: Defina abaixo exatamente a letra do grupo atribuída a vocês:
letra_do_grupo :: Char
letra_do_grupo = 'O'  -- Substitua pela sua letra (Ex: 'A', 'B', 'C'...)

-- ==============================================================================
-- INSTRUÇÕES DE IMPLEMENTAÇÃO:
-- 1. Implemente APENAS as funções do seu kit designado.
-- 2. Declare explicitamente o tipo APENAS da função principal de cada questão.
-- 3. Mantenha os comentários indicando o número da questão conforme o enunciado.
-- ==============================================================================

-- Insira suas funções abaixo:

-- ==============================================================================
-- Auxiliares
-- ==============================================================================

-- TODO: Implementação de `mod`

-- abs:: (Ord i, Num i) => i -> i
abs 0 = 0
abs n = if n > 0 then n else negate n

-- associate :: (Eq t) => t -> [t] -> [t] ->  t
associate n [] _ = n
associate n _ [] = n
associate n (x : xs) (y : ys)
   | x == n    = y
   | otherwise = associate n xs ys

-- length :: (Num i) => [t] -> i
length xs = length_aux xs 0

-- length_aux :: (Num i) => [t] -> i -> i
length_aux [] current_size = current_size
length_aux (x : xs) current_size = length_aux xs (current_size + 1)
associate n (x:xs) (y:ys)
   | x == n    = y
   | otherwise = associate n xs ys

-- elem :: (Eq t) => t -> [t] -> Bool
elem a [] = False
elem a (x:xs)
   | a == x = True
   | otherwise = a `elem` xs

-- head :: [t] -> t
head [] = error "list is empty"
head (x:_) = x

-- init :: [t] -> [t]
init [] = error "list is empty"
init [x] = []
init (x : xs) = x : init xs

-- head :: [t] -> t
head [] = error "list is empty"
head (x : _) = x

-- tail :: [t] -> [t]
tail [] = error "list is empty"
tail (_ : xs) = xs
init (x:xs) = x : init xs

-- last :: [t] -> t
last [] = error "list is empty"
last [x] = x
last (_:xs) = last xs

-- length :: (Num i) => [t] -> i
length [] = 0
length (_:xs) = 1 + length xs

-- negate :: (Eq i, Num i) => i -> i
negate 0 = 0
negate n = -n

-- otherwise :: Bool
otherwise = True

-- tail :: [t] -> [t]
tail [] = error "list is empty"
tail (_:xs) = xs

-- toUpper :: Char -> Char
toUpper c
   | c `elem` ['a'..'z'] = associate c ['a'..'z'] ['A'..'Z']
   | otherwise           = c

-- grau_precedencia :: (Ord t, Num i) => t -> [t] -> i
grau_precedencia a xs = grau_aux a xs 0
-- ^ quantos elementos o elemento a precede na lista l

-- grau_aux :: (Ord t, Num i) => t -> [t] -> i -> i
grau_aux _ [] grau_corrente = grau_corrente
grau_aux a (x : xs) grau_corrente
   | a < x        = grau_aux a xs (grau_corrente + 1)
   | otherwise    = grau_aux a xs grau_corrente

-- mesmos_aux :: (Eq t) => [t] -> [t] -> Bool
mesmos_aux [] _ = True -- chegou ao final da lista l
mesmos_aux (x : xs) ys 
   | x `elem` ys = mesmos_aux xs ys
   | otherwise   = False
-- ^ consome os elementos da lista l na medida em que estão nas lista k
-- l, k inicialmente não-vazias

-- ADICIONADA
-- ordenado :: (Ord t) => t -> Bool
ordenado [] = True
ordenado [a] = True
ordenado (a : b : xs)
   | a <= b    = ordenado (b : xs)
   | otherwise = False

-- ADICIONADA
-- insere_ord_aux :: (Ord t) => t -> [t] -> [t]
insere_ord_aux n [] = [n]
insere_ord_aux n (x : xs)
   | n > x     = x : insere_ord_aux n xs
   | otherwise = [n] ++ (x : xs)

-- ADICIONADA
-- insertion_sort :: (Ord t) => [t] -> [t] -> [t]
insertion_sort [] l_ordenada = l_ordenada
insertion_sort (x : xs) l_ordenada =
   insertion_sort xs (insere_ordenado x l_ordenada)

-- ADICIONADA
-- somatorio :: (Num x) => [x] -> x
somatorio l = soma_aux l 0

-- ADICIONADA
-- soma_aux :: (Num x) => [x] -> x
soma_aux [] soma_corrente = soma_corrente
soma_aux (x : xs) soma_corrente = soma_aux xs (soma_corrente + x)

-- ADICIONADA
-- soma_deltas :: (Num x) => [x] -> x -> x
soma_deltas l x_barra = soma_deltas_aux l x_barra 0

-- ADICIONADA
-- soma_deltas_aux :: (Num x) => [x] -> x -> x -> x
soma_deltas_aux [] _ soma_corrente = soma_corrente
soma_deltas_aux (x_i : x_is) x_barra soma_corrente =
   soma_deltas_aux x_is x_barra (soma_corrente + delta)
      where delta = (x_i - x_barra)^2

-- ==============================================================================
-- Questões
-- ==============================================================================

-- === questão 1 ===
ultimo :: [t] -> t
ultimo [x] = x
ultimo (x : xs) = ultimo xs

-- === questão 2 ===
insere_no_fim :: t -> [t] -> [t]
insere_no_fim p [] = [p]
insere_no_fim p (x : xs) = x : insere_no_fim p xs

-- === questão 4 ===
maiores_que :: (Ord t) => t -> [t] -> [t]
maiores_que _ [] = []
maiores_que p (x : xs)
   | x > p         = x : maiores_que p xs
   | otherwise     = maiores_que p xs

-- === questão 13 ===
-- uma lista l = [m, m + 1, ..., m + (n - 2), m + (n - 1)]
sequencia :: (Ord i, Num i) => i -> i -> [i]
sequencia 0 _ = []
sequencia n m
   | (n < 0) || (m < 0) = error "n ou m negativo"
   | otherwise          = m : sequencia (n - 1) (m + 1)

-- === questão 15 ===
-- todos os top n maiores elementos da lista k,
-- na ordem em que aparecem
maiores :: (Ord t) => Int -> [t] -> [t]
maiores _ [] = []
maiores 0 _ = []
maiores n (x : xs)
   | n < 0                       = error "n é negativo"
   | (grau_precedencia x xs) < n = x : maiores (n - 1) xs
   | otherwise                   = maiores n xs

-- === questão 17 ===
-- dado os conjuntos A e B,
-- A ⋃ B = {x ∈ U | x ∈ A **ou** x ∈ B}
sequencia :: (Ord i, Num i) => i -> i -> [i]
sequencia 0 _ = []
sequencia n m
   | (n < 0) || (m < 0) = []
   | otherwise          = m : sequencia (n - 1) (m + 1)

-- === Questão 17 ===
-- Dado os conjuntos A e B, a união A B é definida como:
-- x tal que x pertence a A **ou** x pertence a B
uniao :: (Eq t) => [t] -> [t] -> [t]
uniao [] [] = []
uniao x [] = x
uniao [] y = y
uniao (x : xs) y
   | x `elem` y = uniao xs y
   | otherwise  = x : uniao xs y

-- === questão 18 ===
-- dado os conjuntos A e B,
-- A ⋂ B = {x ∈ U | x ∈ A **e** x ∈ B}
interseccao :: (Eq t) => [t] -> [t] -> [t]
interseccao [] [] = []
interseccao _ [] = []
interseccao [] _ = []
interseccao (x : xs) y
   | x `elem` y = x : interseccao xs y
   | otherwise  = interseccao xs y

-- === questão 19 ===
-- dadas as listas l, k,
-- os elementos de l estão em k e os elementos de k estão em l
mesmos_elementos :: (Eq t) => [t] -> [t] -> Bool
mesmos_elementos [] [] = True
mesmos_elementos _ [] = False
mesmos_elementos [] _ = False
mesmos_elementos xs ys = (mesmos_aux xs ys) && (mesmos_aux ys xs)

-- ADICIONADA
-- === questão 20 ===
-- lista k (ordenada) com um elemento n inserido na lista l (também ordenada)
insere_ordenado :: (Ord t) => t -> [t] -> [t]
insere_ordenado n l
   | ordenado l = insere_ord_aux n l
   | otherwise  = error "lista não ordenada"

-- ADICIONADA
-- === questão 22 ===
-- lista k que consiste em uma lista l ordenada
ordena :: (Ord t) => [t] -> [t]
ordena l = insertion_sort l []

-- === questão 25 ===
rodar_esquerda :: (Integral n) => n -> [t] -> [t]
rodar_esquerda _ [] = []
rodar_esquerda _ [x] = [x]
rodar_esquerda n xs
   | j <= 0 = xs
   | j > 0  = rodar_esquerda (j - 1) (ultimo xs : init xs)
   where j = n `mod` length xs
uniao (x:xs) y
   | x `elem` y = uniao xs y
   | otherwise  = x : uniao xs y

-- === Questão 18 ===
{- Dado os conjuntos A e B, a interseccao A B é definida como:x tal que x pertence
a A **e** x pertence a B -}
interseccao :: (Eq t) => [t] -> [t] -> [t]
interseccao [] [] = []
interseccao _ [] = []
interseccao [] _ = []
interseccao (x:xs) y
   | x `elem` y = x : interseccao xs y
   | otherwise  = interseccao xs y

-- === Questão 19 ===
mesmos_elementos :: (Eq t) => [t] -> [t] -> Bool
mesmos_elementos [] [] = True
mesmos_elementos x [] = False
mesmos_elementos [] y = False
mesmos_elementos (x:xs) y
   | x `elem` y = mesmos_elementos xs y
   | otherwise  = False

-- === Questão 25 ===
rodar_esquerda :: (Integral n) => n -> [t] -> [t]
rodar_esquerda _ [] = []
rodar_esquerda _ [x] = [x]
rodar_esquerda n list
   | j <= 0 = list
   | j > 0  = rodar_esquerda (j-1) (last list : init list)
   where j = n `mod` length list

-- === questão 26 ===
todas_maiusculas :: [Char] -> [Char]
todas_maiusculas [] = []
todas_maiusculas (c : cs) = toUpper c : todas_maiusculas cs

-- ADICIONADA
-- === questão 28 ===
-- a média aritmética (populacional) de uma lista l de números
media :: (Fractional x) => [x] -> x
media l = somatorio l / length l

-- ADICIONADA
-- == questão 29 ===
-- a variância (populacional) de uma lista l de números
variancia :: (Fractional x) => [x] -> x
variancia l = soma_deltas l (media l) / length l

-- ==============================================================================
-- Implementações alternativas de algumas questões
-- ==============================================================================

-- função auxiliar length, versão não-otimizada
-- length :: (Num i) => [t] -> i
-- length [] = 0
-- length (_ : xs) = 1 + length xs

-- função auxiliar grau_precedencia, versão não-otimizada
-- grau_precedencia :: (Ord t) => t -> [t] -> Int
-- grau_precedencia _ [] = 0
-- grau_precedencia a (x : xs)
--   | a < x         = 1 + grau_precedencia a xs
--   | otherwise     = grau_precedencia a xs

-- função auxiliar das implementações alternativas de rodar_direita e rodar_esquerda
-- ==============================================================================
-- Implementações alternativas questões
-- ==============================================================================

-- Usado para implementação alternativa de rodar_direita e rodar_esquerda
-- rodar :: (Integral n) => n -> [t] -> [t]
-- rodar _ [] = []
-- rodar _ [x] = [x]
-- rodar n list
--    | j < 0     = rodar (j + 1) (ultimo list : init list)
--    | j == 0    = list
--    | otherwise = rodar (j - 1) (tail list ++ [head list])
--    | j < 0 = rodar (j+1) (last list : init list)
--    | j == 0 = list
--    | otherwise = rodar (j-1) (tail list ++ [head list])
--    where j = if n >= 0
--          then n `mod` length list
--          else negate (abs n `mod` length list)

-- rodar_direita alternativa
-- rodar_direita :: (Integral i) => i -> [t] -> [t]
-- rodar_direita i list = if i <= 0 then list else rodar i list

-- rodar_esquerda alternativa
-- rodar_esquerda :: (Integral i) => i -> [t] -> [t]
-- rodar_esquerda i list = if i <= 0 then list else rodar (negate i) list

-- função auxiliar que cria lista de deltas para a variância
-- elem_menos_media :: (Floating a) => [a] -> a -> [a]
-- elem_menos_media [] _ = []
-- elem_menos_media (x : xs) n = (x - n)**2 : elem_menos_media xs n

-- variancia alternativa
-- variancia :: (Floating a) => [a] -> a
-- variancia [] = error "lista vazia!"
-- variancia [x] = 0
-- variancia lista (x : xs) = media (elem_menos_media lista (media lista))
-- -- Alternativa rodar_direita
-- rodar_direita :: (Integral i) => i -> [t] -> [t]
-- rodar_direita i list = if i <= 0 then list else rodar i list

-- -- Alternativa rodar_esquerda
-- rodar_esquerda :: (Integral i) => i -> [t] -> [t]
-- rodar_esquerda i list = if i <= 0 then list else rodar (negate i) list
