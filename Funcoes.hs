module Funcoes where

import Tipos
import Data.Time.Calendar (Day, diffDays, fromGregorian)

-----------------------------------
-------- FUNÇÕES BÁSICAS ----------
-----------------------------------

-- Insere no início da lista (O(1))
adicionarTarefa :: Tarefa -> [Tarefa] -> [Tarefa]
adicionarTarefa novaTarefa tarefas = novaTarefa : tarefas

-- Remove primeira tarefa com ID correspondente
removerTarefa :: Int -> [Tarefa] -> [Tarefa]
removerTarefa _ [] = []
removerTarefa id (x:xs) | idTarefa x == id = xs
                        | otherwise = x : removerTarefa id xs

-- Altera status de tarefa para Concluída por ID
marcarConcluída :: Int -> [Tarefa] -> [Tarefa]
marcarConcluída id [] = []
marcarConcluída id (x:xs) | idTarefa x == id = x {status = Concluída} : xs
                          | otherwise = x : marcarConcluída id xs

-----------------------------------
-------- FUNÇÕES AVANÇADAS --------
-----------------------------------

-- Filtra tarefas por categoria
listarPorCategoria :: Categoria -> [Tarefa] -> [Tarefa]
listarPorCategoria cat ts = filter condicao ts
 where
  condicao = (\ts -> categoria ts == cat)

-- Filtra tarefas por prioridade
listarPorPrioridade :: Prioridade -> [Tarefa] -> [Tarefa]
listarPorPrioridade prior ts = filter condicao ts
 where
  condicao = (\ts -> prioridade ts == prior)

-- Filtra tarefas por status
filtrarPorStatus :: Status -> [Tarefa] -> [Tarefa]
filtrarPorStatus stat ts = filter condicao ts
 where
  condicao = (\ts -> status ts == stat)

-- Ordena por prioridade (Alta > Média > Baixa)
ordenarPorPrioridade :: [Tarefa] -> [Tarefa]
ordenarPorPrioridade ts = listarPorPrioridade Alta ts  ++
                          listarPorPrioridade Media ts ++
                          listarPorPrioridade Baixa ts

-- Busca tarefas contendo palavra na descrição, usa words para separá-la palavra a palavra
buscarPorPalavraChave :: String -> [Tarefa] -> [Tarefa]
buscarPorPalavraChave palavra ts = filter condicao ts
 where
  condicao = (\ts -> procurarString palavra (words(descricao ts)) == palavra)

-----------------------------------
------- FUNÇÕES ADICIONAIS --------
-----------------------------------

-- Construtor rápido com valores padrão
-- Tarefa: idTarefa, descrição, status, prioridade, caregoria, prazo, tags
criarTarefaPadrão :: Int -> String -> Prioridade -> Categoria -> Tarefa
criarTarefaPadrão id desc prio cat = Tarefa id desc Pendente prio cat Nothing []

-- Auxiliar: verifica se palavra está na lista
procurarString :: String -> [String] -> String
procurarString _ [] = ""
procurarString palavra (x:xs) | palavra == x = palavra
                              | otherwise    = procurarString palavra xs


-- Contar quantidade de tarefas
contarTarefas :: [Tarefa] -> Int
contarTarefas [] = 0
contarTarefas tarefas = length tarefas

-- Achar maior ID
acharMaiorID :: [Tarefa] -> Int
acharMaiorID [] = 0
acharMaiorID [x] = idTarefa x
acharMaiorID (x:y:xs) | idTarefa x > idTarefa y = max (idTarefa x) (acharMaiorID (x:xs))
                      | otherwise = max (idTarefa y) (acharMaiorID (y:xs))

-----------------------------------
-------- GESTÃO DE PRAZOS ---------
-----------------------------------

-- Retorna tarefas com prazos expirados
verificarAtrasos :: [Tarefa] -> Day -> [Tarefa]
verificarAtrasos ts dataAtual = filter condicao ts
 where
  condicao ts = case prazo ts of
   Nothing -> False
   Just dataPrazo -> dataPrazo < dataAtual

-- Calcula quantos dias faltam para o prazo de uma tarefa
calcularDiasRestantes :: Tarefa -> Day -> Maybe Int
calcularDiasRestantes ts dataAtual =
 case prazo ts of
  Just dataPrazo -> Just (fromInteger $ diffDays dataPrazo dataAtual)
  Nothing        -> Nothing

--------------------------------------
-- TESTES RETIRAR ANTES DE COMPILAR --
--------------------------------------

hoje :: Day
hoje = fromGregorian 2025 04 20

t1 = Tarefa 1 "Reuniao importante" Pendente Alta Trabalho (Just $ fromGregorian 2025 04 15) ["Trabalho", "Urgencia"]
t2 = Tarefa 2 "Ir no mercado" Pendente Media Pessoal (Just $ fromGregorian 2025 04 25) ["Casa", "Comida"]
t3 = Tarefa 3 "Estudar Haskell" Concluída Baixa Estudos Nothing ["PF", "Faculdade"]
t4 = Tarefa 4 "Enviar Trabalho PF" Pendente Alta Estudos Nothing ["PF", "Trabalho"]
tarefas = [t1,t2,t3,t4]
t5 = Tarefa 5 "Mandar mensagem professor" Pendente Media Estudos Nothing ["SD", "Faculdade"]

-------------------------------------
--------- SISTEMA DE TAGS -----------
-------------------------------------

-- Retorna tarefas que possuem uma respectiva tag
filtrarPorTag :: String -> [Tarefa] -> [Tarefa]
filtrarPorTag tag ts = filter condicao ts
 where
  condicao = (\ts -> procurarString tag (tags ts) == tag)

-- Gera uma lista de tags e suas frequências de uso (Lista de Tuplas)
nuvemDeTags :: [Tarefa] -> [(String, Int)]
nuvemDeTags tarefas = [(tag, length (filter (== tag) todasTags))
                      |tag <- removerRepetidos (quickSort todasTags)]
                      where
                       todasTags = formarListaDeTags tarefas

-- Forma uma lista com as tags de todas as tarefas
formarListaDeTags :: [Tarefa] -> [String]
formarListaDeTags ts = [ tag | t <- ts, tag <- tags t]

-- Remove elementos repetidos de uma lista
removerRepetidos :: Eq a => [a] -> [a]
removerRepetidos [] = []
removerRepetidos [x] = [x]
removerRepetidos (x:y:xs) | x == y = removerRepetidos (y:xs)
                          | otherwise = x:removerRepetidos(y:xs)

-- Ordena uma lista
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort menores ++ [x] ++ quickSort maiores
 where
  menores = filter (< x) xs
  maiores = filter (>= x) xs
