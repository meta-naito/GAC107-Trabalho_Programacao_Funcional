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

-- 'hiding' esconde implementações do Prelude
import Prelude hiding (elem, head, tail, init, last, length, negate, abs, otherwise, map, toUpper)

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

-- otherwise :: Bool
otherwise = True

-- abs:: (Ord i, Num i) => i -> i
abs 0 = 0
abs n = if n > 0 then n else negate n

-- elem :: (Eq a) => a -> [a] -> Bool
elem a [] = False
elem a (x:xs)
   | a == x = True
   | otherwise = a `elem` xs

-- length :: (Num n) => [t] -> n
length [] = 0
length (_:xs) = 1 + length xs

-- head :: [t] -> t
head [] = error "list is empty"
head (x:_) = x

-- tail :: [t] -> [t]
tail [] = error "list is empty"
tail (_:xs) = xs

-- init :: [t] -> [t]
init [] = error "list is empty"
init [x] = []
init (x:xs) = x : init xs

-- last :: [t] -> t
last [] = error "list is empty"
last [x] = x
last (_:xs) = last xs

-- negate :: (Ord i, Num i) => i -> i
negate 0 = 0
negate n = -n

-- toUpper :: Char -> Char
toUpper c
   | c `elem` ['a'..'z'] = toEnum (fromEnum c - dif) :: Char
   | otherwise = c
   where dif = fromEnum 'a' - fromEnum 'A'

-- ==============================================================================
-- Questões
-- ==============================================================================

-- === questão 1 ===
ultimo :: [a] -> a
ultimo [x] = x
ultimo (x : xs) = ultimo xs

-- === questão 2 ===
insere_no_fim :: a -> [a] -> [a]
insere_no_fim p [] = [p]
insere_no_fim p (x : xs) = x : insere_no_fim p xs

-- === questão 4 ===
maiores_que :: Ord a => a -> [a] -> [a]
maiores_que _ [] = []
maiores_que p (x : xs)
   | x > p         = x : maiores_que p xs
   | otherwise     = maiores_que p xs

-- === questão 13 ===
sequencia :: Int -> Int -> [Int]
sequencia 0 _ = []
sequencia n m
   | (n < 0) || (m < 0)    = []
   | otherwise             = m : sequencia (n - 1) (m + 1)

-- === Questão 17 ===
-- Dado os conjuntos A e B, a união A B é definida como:
-- x tal que x pertence a A **ou** x pertence a B
uniao :: (Eq a) => [a] -> [a] -> [a]
uniao [] [] = []
uniao a [] = a
uniao [] b = b
uniao (x:xs) b
   | x `elem` b = uniao xs b
   | otherwise = x : uniao xs b

-- === Questão 18 ===
{- Dado os conjuntos A e B, a interseccao A B é definida como:x tal que x pertence
a A **e** x pertence a B -}
interseccao :: (Eq a) => [a] -> [a] -> [a]
interseccao [] [] = []
interseccao _ [] = []
interseccao [] _ = []
interseccao (x:xs) b
   | x `elem` b = x : interseccao xs b
   | otherwise = interseccao xs b

-- === Questão 19 ===
{- Dado os conjuntos A e B, a interseccao A B é definida como: x tal que x pertence
a A **e** x pertence a B -}
mesmos_elementos :: (Eq a) => [a] -> [a] -> Bool
mesmos_elementos [] [] = True
mesmos_elementos a [] = False
mesmos_elementos [] b = False
mesmos_elementos (x:xs) b
   | x `elem` b = mesmos_elementos xs b
   | otherwise = False

-- === Questão 25 ===
rodar_esquerda :: Int -> [a] -> [a]
rodar_esquerda _ [] = []
rodar_esquerda _ [x] = [x]
rodar_esquerda n list
   | j <= 0 = list
   | j > 0 = rodar_esquerda (j-1) (last list : init list)
   where j = n `mod` length list

-- === questão 26 ===
todas_maiusculas :: [Char] -> [Char]
todas_maiusculas [] = []
todas_maiusculas (c : cs) = toUpper c : todas_maiusculas cs

-- ==============================================================================
-- Implementações alternativas questões
-- ==============================================================================

-- Usado para implementação alternativa de rodar_direita e rodar_esquerda
-- rodar :: Int -> [a] -> [a]
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
-- rodar_direita :: Int  -> [a] -> [a]
-- rodar_direita i list = if i <= 0 then list else rodar i list

-- -- Alternativa rodar_esquerda
-- rodar_esquerda :: Int -> [a] -> [a]
-- rodar_esquerda i list = if i <= 0 then list else rodar (negate i) list
