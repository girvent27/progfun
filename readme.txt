______________________________________________________________
======= Sistema de Gerenciamento de Tarefas em Haskell =======
______________________________________________________________
--Versão do Haskell: v8.8.4 (ou acima)


+ Compilação       
   Pelo terminal, qualquer sistema com Haskell já instalado, 
 vá ao diretório do arquivo do gerenciador  e digite os seguintes
 comandos:

- Se o Main.exe ou Main não estiver compilado:

      ghc --make Main.hs
      
- Depois digite o comando:

      .\Main

+ Uso
   Programa  para inserir e monitorar Tarefas de uso cotidiano
 com menu de opções para: inserção, remoção, analíse e filtro/ordenação.
   As tarefas são organizadas com as seguintes informações:
  
   - Descrição: String para escrever detalhamento da tarefa;
   - Status: Pendente ou Concluída (respeitar o uso de caixa alta e
    acento);
   - Prioridade: Baixa, Media, Alta (respeitar o uso de caixa alta e
     acento);
   - Categoria: Trabalho, Estudos, Pessoal, Outro (respeitar o uso
     de caixa alta e acento);
   - Prazo: Data limite, formatado em AAAA-MM-DD; 
   - Tags: Grupo de tags em string, ilimitadas;
   
   Ao utilizar pela primeira vez o programa, um arquivo frases.txt é criado,
 o qual é utilizado como persistência da memória da lista.

+ Funcionalidades
   - Criação de Novas Tarefas;
   - Organização de Tarefas e filtros para organização pessoal;
   - Atualização do status de Trefas;
   - Remoção de Tarefas;

+ Autor
   Desenvolvido pelo grupo de alunos de Programação Funcional
 diciplina de código: *GBC033* da Universidade Federal de Uberlândia
   Professor da diciplina: Alexsandro Santos Soares
   Autores do Projeto:
   - Ana Luiza Moreira Silva
   - Jean Luc Girvent Deu
   - Matheus Martins de Resende
   - Vinícius Augusto de Souza
   
