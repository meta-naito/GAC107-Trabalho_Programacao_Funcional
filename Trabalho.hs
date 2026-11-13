{- ==============================================================================
   UNIVERSIDADE FEDERAL DE LAVRAS - UFLA
   Departamento de Computação Aplicada
   Paradigmas de Linguagens de Programação
   
   TRABALHO PRÁTICO - PROGRAMAÇÃO FUNCIONAL
   
   GRUPO: 'O'

   INTEGRANTES:
   1. Arthur Fazzio Mendes | Matrícula: 202520154
   2. Livia Rezende Masson | Matrícula: 202520156
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

-- soma todos os elementos de uma lista, igual a sum para listas (mudar o nome ou algo assim)
-- somatorio :: (Num a) => [a] -> a
somatorio [] = 0
somatorio [x] = x
somatorio (x:xs) = x + somatorio xs

-- elem_menos_media é a parte do (x_i - media)**2 da variância, coloquei separado por conta da recursão
-- elem_menos_media :: (Floating a) => [a] -> a -> [a]
elem_menos_media [] _ = []
elem_menos_media (x:xs) n = (x - n)**2 : elem_menos_media xs n

-- remove apenas UMA instância de um elemento n da lista
-- remove_um :: (Eq a, Ord a) => a -> [a] -> [a]
remove_um _ [] = []
remove_um n (x:xs)
   | n /= x = x : remove_um n xs
   | otherwise = xs

-- não sei se é igual a concat, mas funciona para o que precisava KKK
-- concatena :: (Ord a) => [a] -> [a] -> [a]
concatena [] [] = []
concatena x [] = x
concatena [] y = y
concatena (x:xs) (y:yt) = x : concatena xs (y:yt)

-- essencialmente pega os maiores para depois arruma_posicao arrumá-los
-- maiores_aux :: (Integral n, Ord a) => n -> [a] -> [a]
maiores_aux _ [] = []
maiores_aux n lista@(x:xs)
   | n > tamanho lista = error "querendo mais elementos que a lista possui?"
   | n == 0 = []
   | otherwise = m : maiores_aux (n - 1) (remove_um m lista)
   where m = maximum lista

-- retorna uma lista nova, com os elementos ordenados baseado na lista original (pois a questão pede isso)
-- (x:xs) = original | lista2 = a dos maiores
-- arruma_posicao :: (Eq a) => [a] -> [a] -> [a]
arruma_posicao [] [] = []
arruma_posicao x [] = []
arruma_posicao [] y = []
arruma_posicao (x:xs) lista2
   | x `elem` lista2 = x : arruma_posicao xs lista2
   | otherwise = arruma_posicao xs lista2

-- ==============================================================================
-- Questões
-- ==============================================================================

-- == questão 15 ==
maiores :: (Integral n, Ord a) => n -> [a] -> [a]
maiores _ [] = []
maiores n lista = arruma_posicao lista (maiores_aux n lista)

-- == questão 20 ==
-- TODO: checar se a lista ta ordenada pra poder fazer isso, senao ou ordena ou manda erro
insere_ordenado :: (Ord a) => a -> [a] -> [a]
insere_ordenado n [] = [n]
insere_ordenado n (x:xs)
   | n > x = x : insere_ordenado n xs
   | otherwise = concatena [n] (x:xs)

-- == questão 22 ==
ordena :: (Ord a, Eq a) => [a] -> [a]
ordena [] = error "lista vazia!"
ordena [x] = [x]
ordena (x:y:[]) = (min x y) : [max x y]
ordena (x:xs)
   | x > m = m : (ordena (x : (remove_um m xs)))
   | otherwise = x : ordena xs
   where m = minimum xs

-- == questão 28 ==
media :: (Num a, Fractional a) => [a] -> a
media [] = 0
media [x] = x
media lista@(x:xs) = somatorio lista / tamanho lista

-- == questão 29 ==
variancia :: (Floating a) => [a] -> a
variancia [] = error "lista vazia!"
variancia [x] = 0
variancia lista@(x:xs) = media (elem_menos_media lista (media lista))
