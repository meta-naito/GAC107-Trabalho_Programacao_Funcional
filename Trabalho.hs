{- ==============================================================================
   UNIVERSIDADE FEDERAL DE LAVRAS - UFLA
   Departamento de Computação Aplicada
   Paradigmas de Linguagens de Programação
   
   TRABALHO PRÁTICO - PROGRAMAÇÃO FUNCIONAL
   
   GRUPO: [Preencher com a letra do seu grupo, ex: 'A']
   
   INTEGRANTES:
   1. [Nome Completo do Aluno 1] | Matrícula: [00000000]
   2. [Nome Completo do Aluno 2] | Matrícula: [00000000]
   3. [Nome Completo do Aluno 3] | Matrícula: [00000000]
   ============================================================================== -}

module Trabalho where

import Data.Char (toUpper, toLower, isSpace)
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

-- questão 1
ultimo :: [a] -> a
ultimo (x : []) = x
ultimo (x : xs) = ultimo xs

-- questão 2
insere_no_fim :: a -> [a] -> [a]
insere_no_fim p [] = p : []
insere_no_fim p (x : xs) = x : (insere_no_fim p xs)

-- questão 4
maiores_que :: Ord a => a -> [a] -> [a]
maiores_que _ [] = []
maiores_que p (x : xs)
   | x > p         = x : maiores_que p xs
   | otherwise     = maiores_que p xs

-- questão 13
sequencia :: Int -> Int -> [Int]
sequencia 0 _ = []
sequencia n m
   | (n < 0) || (m < 0)    = []
   | otherwise             = m : sequencia (n - 1) (m + 1)

-- questão 26
todas_maiusculas :: [Char] -> [Char]
todas_maiusculas [] = []
todas_maiusculas (c : cs) = (maiuscula c) : (todas_maiusculas cs)

maiuscula 'a' = 'A'
maiuscula 'b' = 'B'
maiuscula 'c' = 'C'
maiuscula 'd' = 'D'
maiuscula 'e' = 'E'
maiuscula 'f' = 'F'
maiuscula 'g' = 'G'
maiuscula 'h' = 'H'
maiuscula 'i' = 'I'
maiuscula 'j' = 'J'
maiuscula 'k' = 'K'
maiuscula 'l' = 'L'
maiuscula 'm' = 'M'
maiuscula 'n' = 'N'
maiuscula 'o' = 'O'
maiuscula 'p' = 'P'
maiuscula 'q' = 'Q'
maiuscula 'r' = 'R'
maiuscula 's' = 'S'
maiuscula 't' = 'T'
maiuscula 'u' = 'U'
maiuscula 'v' = 'V'
maiuscula 'w' = 'W'
maiuscula 'x' = 'X'
maiuscula 'y' = 'Y'
maiuscula 'z' = 'Z'
maiuscula c = c