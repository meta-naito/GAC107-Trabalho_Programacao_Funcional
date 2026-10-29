{- ==============================================================================
   UNIVERSIDADE FEDERAL DE LAVRAS - UFLA
   Departamento de Computação Aplicada
   Paradigmas de Linguagens de Programação
   
   TRABALHO PRÁTICO - PROGRAMAÇÃO FUNCIONAL
   
   GRUPO: 'O'
   
   INTEGRANTES:
   1. Arthur Fazzio Mendes | Matrícula: 202520154
   2. Livia Rezende Masson | Matrícula: [00000000]
   3. Natan Amaral Uchôa | Matrícula: [00000000]
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
import Prelude hiding (abs, elem, head, init, last, length, negate, otherwise, tail)

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

-- ==============================================================================
-- Implementações alternativas questões
-- ==============================================================================

-- Usado para implementação alternativa de rodar_direita e rodar_esquerda
-- rodar :: (Integral n) => n -> [t] -> [t]
-- rodar _ [] = []
-- rodar _ [x] = [x]
-- rodar n list
--    | j < 0 = rodar (j+1) (last list : init list)
--    | j == 0 = list
--    | otherwise = rodar (j-1) (tail list ++ [head list])
--    where j = if n >= 0
--          then n `mod` length list
--          else negate (abs n `mod` length list)

-- -- Alternativa rodar_direita
-- rodar_direita :: (Integral i) => i -> [t] -> [t]
-- rodar_direita i list = if i <= 0 then list else rodar i list

-- -- Alternativa rodar_esquerda
-- rodar_esquerda :: (Integral i) => i -> [t] -> [t]
-- rodar_esquerda i list = if i <= 0 then list else rodar (negate i) list
