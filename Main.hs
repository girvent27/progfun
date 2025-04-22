module Main (main) where

import Tipos
import Funcoes
import Persistencia
import System.IO
import Text.Read
import Data.Char
import Data.Time.Calendar
import Control.Monad (unless)

main :: IO()
main = do hSetBuffering stdout NoBuffering
          listaPreExistente <- carregarDeArquivo "tarefas.txt"
          mainLoop listaPreExistente
           
mainLoop :: [Tarefa] -> IO ()
mainLoop lista = do
   mostrarCabecalho
   menuPrincipal
   opcao <- getLine
   unless (opcao == "14") $ do
     case opcao of
       "1" -> adicionarTarefaIO lista >>= mainLoop
       "2" -> listarTarefasIO lista >>= mainLoop
       "3" -> removerTarefaIO lista >>= mainLoop
       "4" -> marcarComoConcluídaIO lista >>= mainLoop 
       "5" -> listarCategoriaIO lista >>= mainLoop
       _   -> do putStrLn "Opção inválida, tente novamente"
                 mainLoop lista
          
mostrarCabecalho :: IO()
mostrarCabecalho = do
          putStrLn "=========================================="
          putStrLn "       SISTEMA DE GESTÃO DE TAREFAS       "
          putStrLn "=========================================="
          
menuPrincipal :: IO()
menuPrincipal = do
          putStrLn "\nQual opção deseja?"
          putStrLn "1 para Adicionar Tarefa"
          putStrLn "2 para Listar Tarefas"
          putStrLn "3 para Remover Tarefa"
          putStrLn "4 para Marcar Conclusão de Tarefa"
          putStrLn "5 para listar tarefas de determinada categoria"
          putStrLn "6 para listar tarefas de determinada prioridade"
          putStrLn "7 para ordenar todas as tarefas por ordem de prioridade"
          putStrLn "8 para filtrar listas por status específico"
          putStrLn "9 para buscar tarefa por palavra chave"
          putStrLn "10 para calcular tarefas atrasadas"
          putStrLn "11 para ver prazo restante para determinada tarefa"
          putStrLn "12 para buscar tarefas por TAG"
          putStrLn "13 para listar frequência de uso de cada TAG"
          putStrLn "14 Sair"
          putStrLn "Opção:"
          
listarCategoriaIO :: [Tarefa] -> IO [Tarefa]
listarCategoriaIO tarefas = do
         putStrLn "Escolha a Categoria (Trabalho/Estudos/Pessoal/Outro):"
         cat <- getLine
         let catStr = case cat of
                    "Trabalho" -> Trabalho
                    "Estudos"  -> Estudos
                    "Pessoal"  -> Pessoal
                    _          -> Outro
         let tarefasFiltradas = listarPorCategoria catStr tarefas
         
         if null tarefasFiltradas
                    then putStrLn "Nenhuma tarefa registrada nessa categoria."
                    else mapM_ mostrarTarefa tarefasFiltradas
         
         return tarefas
            where
               mostrarTarefa tarefa = do
                     putStrLn "\n----------------------------------------------------"
                     putStrLn $ "ID: "            ++ show (idTarefa tarefa) ++
                                " | Descrição:"   ++ descricao tarefa ++
                                " | Status: "     ++ show (status tarefa) ++
                                " | Prioridade: " ++ show (prioridade tarefa)
                     putStrLn "----------------------------------------------------"
                     
marcarComoConcluídaIO :: [Tarefa] -> IO [Tarefa]
marcarComoConcluídaIO l = do
          putStrLn "ID da tarefa concluida:"
          idTarefa <- getLine
          case readMaybe idTarefa of
            Just id1 -> do
              let listaAtual = marcarConcluída id1 l
              print listaAtual
              salvarEmArquivo "tarefas.txt" listaAtual
              return listaAtual
            Nothing -> do
              putStrLn "ID inválido!"
              return l
          
listarTarefasIO :: [Tarefa] -> IO [Tarefa]
listarTarefasIO tarefas = do
         putStrLn "\n----------------------------------------------------"
         mapM_ mostrarTarefa tarefas
         return tarefas
            where
               mostrarTarefa tarefa = do
                     putStrLn "\n----------------------------------------------------"
                     putStrLn $ "ID: " ++ show (idTarefa tarefa) ++
                                " | Descrição:" ++ descricao tarefa ++
                                " | Status: " ++ show (status tarefa)
                     putStrLn "----------------------------------------------------"
                     
removerTarefaIO :: [Tarefa] -> IO [Tarefa]
removerTarefaIO l = do
          putStrLn "ID da tarefa:"
          idTarefa <- getLine
          case readMaybe idTarefa of
            Just id1 -> do
              let listaAtual = removerTarefa id1 l
              print listaAtual
              salvarEmArquivo "tarefas.txt" listaAtual
              return listaAtual
            Nothing -> do
              putStrLn "ID inválido!"
              return l
          
          
adicionarTarefaIO :: [Tarefa] -> IO [Tarefa]
adicionarTarefaIO l = do
          putStrLn "Descrição:"
          descricao <- getLine
          putStrLn "Status (Concluida/Pendente):"
          status <- getLine
          putStrLn "Prioridade (Baixa/Media/Alta):"
          prioridade <- getLine
          putStrLn "Categoria (Trabalho/Estudos/Pessoal/Outro):"
          categoria <- getLine
          putStrLn "Prazo (YYYY-MM-DD ou deixe vazio):"
          prazoStr <- getLine
          putStrLn "Informe as TAGs (separadas por espaço):"
          tags <- getLine
          
          let status1 = if status == "Concluida" then Concluída else Pendente
              prioridade1 = case prioridade of
                           "Baixa" -> Baixa
                           "Media" -> Media
                           "Alta"  -> Alta
                           _       -> Media
              categoria1 = case categoria of
                           "Trabalho" -> Trabalho
                           "Estudos"  -> Estudos
                           "Pessoal"  -> Pessoal
                           "Outro"    -> Outro
                           _          -> Outro
              tags1 = words tags
              prazo1 = parsePrazo prazoStr
              id1 = case l of
                    [] -> 1
                    _  -> acharMaiorID l + 1
              t1 = Tarefa id1 descricao status1 prioridade1 categoria1 prazo1 tags1
              listaAtual = adicionarTarefa t1 l
              
          putStrLn "Tarefa adicionada com sucesso!"   
          print listaAtual
          salvarEmArquivo "tarefas.txt" listaAtual
          return listaAtual
       where
          parsePrazo "" = Nothing
          parsePrazo str = case reads str of
                           [(d, "")] -> Just d
                           _ -> Nothing    
