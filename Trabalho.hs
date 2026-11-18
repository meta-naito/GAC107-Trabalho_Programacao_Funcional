{- ==============================================================================
   UNIVERSIDADE FEDERAL DE LAVRAS - UFLA
   Departamento de Computação Aplicada
   Paradigmas de Linguagens de Programação

   TRABALHO PRÁTICO - PROGRAMAÇÃO FUNCIONAL

   GRUPO: 'O'

   INTEGRANTES:
   1. Arthur Fazzio Mendes | Matrícula: 202520154
   2. Livia Rezende Masson | Matrícula: 202520156
   3. Natan Amaral Uchôa | Matrícula: 202610131
   ============================================================================== -}

{- Opção para o GHC suprimir alertas sobre pragmas não reconhecidos. Necessário
para as diretivas do Linter -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- Opção para o Linter ignorar padrão de camelCase na nomeação de funções
{-# HLINT ignore "Use camelCase" #-}
-- Opção para o Linter não sugerir `foldr`
{-# HLINT ignore "Use foldr" #-}
-- Opção para o Linter não sugerir `foldl`
{-# HLINT ignore "Use foldl" #-}
-- Opção para o Linter não sugerir `map`
{-# HLINT ignore "Use map" #-}

module Trabalho where

-- toUpper foi removido de Data.Char em favor de implementação própria
import Data.Char (toLower, isSpace)
import Data.List (sort)

-- 'hiding' esconde implementações
import Prelude hiding (elem, head, init, last, length, otherwise, sum, tail)

-- ATENÇÃO: Defina abaixo exatamente a letra do grupo atribuída a vocês:
letra_do_grupo :: Char
letra_do_grupo = 'O'

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

-- associar :: (Eq t) => t -> [t] -> [t] ->  t
associar n [] _ = n
associar n _ [] = n
associar n (x:xs) (y:ys)
    | x == n    = y
    | otherwise = associar n xs ys

-- elem :: (Eq t) => t -> [t] -> Bool
elem a [] = False
elem a (x:xs)
    | a == x    = True
    | otherwise = a `elem` xs

-- elem_menos_media :: (Floating a) => [a] -> a -> [a]
elem_menos_media [] _ = []
elem_menos_media (x:xs) n = (x - n)^2 : elem_menos_media xs n

-- grau_precedencia :: (Num a, Ord t) => t -> [t] -> a
grau_precedencia _ [] = 0
grau_precedencia n (x:xs)
    | n < x = 1 + grau_precedencia n xs
    | otherwise = grau_precedencia n xs

-- head :: [t] -> t
head [] = error "list is empty"
head (x:_) = x

-- init :: [t] -> [t]
init [] = error "list is empty"
init [x] = []
init (x:xs) = x : init xs

-- insere_ord_aux :: (Ord t) => t -> [t] -> [t]
insere_ord_aux n [] = [n]
insere_ord_aux n (x : xs)
    | n > x     = x : insere_ord_aux n xs
    | otherwise = n : (x : xs)

-- last :: [t] -> t
last [] = error "list is empty"
last [x] = x
last (_:xs) = last xs

-- length :: (Num a) => [t] -> a
length [] = 0
length (_:xs) = 1 + length xs

-- ordenado :: (Ord t) => t -> Bool
ordenado [] = True
ordenado [a] = True
ordenado (a : b : xs)
    | a <= b    = ordenado (b : xs)
    | otherwise = False

-- otherwise :: Bool
otherwise = True

-- somatorio :: (Num a) => [a] -> a
sum l = sum_aux l 0

-- sum_aux :: (Num x) => [x] -> x
sum_aux [] soma_corrente = soma_corrente
sum_aux (x:xs) soma_corrente = sum_aux xs (soma_corrente + x)

-- tail :: [t] -> [t]
tail [] = error "list is empty"
tail (_:xs) = xs

-- toUpper :: Char -> Char
toUpper c
    | c `elem` ['a'..'z'] = associar c ['a'..'z'] ['A'..'Z']
    | otherwise           = c

-- ==============================================================================
-- Questões
-- ==============================================================================

-- === questão 1 ===
ultimo :: [t] -> t
ultimo [x] = x
ultimo (x:xs) = ultimo xs

-- === questão 2 ===
insere_no_fim :: t -> [t] -> [t]
insere_no_fim p [] = [p]
insere_no_fim p (x:xs) = x : insere_no_fim p xs

-- === questão 4 ===
maiores_que :: (Ord t) => t -> [t] -> [t]
maiores_que _ [] = []
maiores_que p (x:xs)
    | x > p     = x : maiores_que p xs
    | otherwise = maiores_que p xs

-- === questão 13 ===
sequencia :: (Ord a, Num a) => a -> a -> [a]
sequencia 0 _ = []
sequencia n m
    | (n < 0) || (m < 0) = []
    | otherwise          = m : sequencia (n - 1) (m + 1)

-- == questão 15 ==
maiores :: (Ord t) => Int -> [t] -> [t]
maiores _ [] = []
maiores 0 _ = []
maiores n (x:xs)
    | n < 0                     = error "N maiores elementos é negativo"
    | grau_precedencia x xs < n = x : maiores (n - 1) xs
    | otherwise                 = maiores n xs

-- === Questão 17 ===
uniao :: (Eq t) => [t] -> [t] -> [t]
uniao [] [] = []
uniao x [] = x
uniao [] y = y
uniao (x:xs) y
    | x `elem` y = uniao xs y
    | otherwise  = x : uniao xs y

-- === Questão 18 ===
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

-- == questão 20 ==
insere_ordenado :: (Ord a) => a -> [a] -> [a]
insere_ordenado n list
    | ordenado list = insere_ord_aux n list
    | otherwise  = error "lista não está ordenada"

-- === Questão 22 ===
ordena :: (Ord a) => [a] -> [a]
ordena [] = []
ordena (x:xs) =
    let menores = ordena [a | a <- xs, a <= x]
        maiores = ordena [a | a <- xs, a > x]
    in menores ++ [x] ++ maiores

-- === Questão 24 ===
rodar_esquerda :: (Integral a) => a -> [t] -> [t]
rodar_esquerda _ [] = []
rodar_esquerda _ [x] = [x]
rodar_esquerda n list
    | j <= 0 = list
    | j > 0  = rodar_esquerda (j-1) (last list : init list)
    where j = n `mod` length list

-- === Questão 25 ===
rodar_direita :: (Integral a) => a -> [t] -> [t]
rodar_direita _ [] = []
rodar_direita _ [x] = [x]
rodar_direita n list
    | j <= 0 = list
    | j >= 0 = rodar_direita (j-1) (tail list ++ [head list])
    where j = n `mod` length list

-- === questão 26 ===
todas_maiusculas :: [Char] -> [Char]
todas_maiusculas [] = []
todas_maiusculas (c : cs) = toUpper c : todas_maiusculas cs

-- === questão 28 ===
media :: (Fractional a) => [a] -> a
media lista = sum lista / fromIntegral (length lista)

-- == questão 29 ===
variancia :: (Fractional a) => [a] -> a
variancia lista = media (elem_menos_media lista (media lista))
