# master

## New features

* Inclui diretorios `conf` e `scripts` em `inst`, contendo script para rodada operacional da selecao
  de cenarios e arquivo de configuracao default do mesmo, respectivamente
* `jsonlite` foi adicionado aos Suggests para realizar a leitura dos arquivos de configuracao
* Introduz funcao `as.cenariosena` para conversao de `data.table`s ja lidos em `cenariosena`
* Funcoes de compactacao agora tratam todas as bacias inclusas no dado de forma multivariada:
  * `acumulaena` faz a soma acumulada de cada bacia em cada cenario, retornando uma dimensao 
    compactada igual a `Nbacias * Nquebras`
  * `PCAena` concatena os trajetos de cada bacia em uma observacao `tamanho_cen * Nbacias` e usa
    isso como dado para compactar

# clustena 1.0

Este pacote prove funcoes para selecao de cenarios de Energia Natural Afluente, independentemente da
metodologia utilizada para gera-los. Alem dos metodos para selecao de cenarios sao fornecidas
ferramentas para visualizacao da escolha.
