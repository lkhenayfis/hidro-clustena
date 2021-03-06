#' Dado Exemplo Do Pacote
#' 
#' Dado contendo um numero reduzido de anos de referencia e bacia para exemplos e testes
#' 
#' \code{cenariosdummy} e um objeto da classe \code{cenariosena}, uma lista de um emento chamado 
#' cenarios, sendo este um \code{data.table} em formato regular com as colunas
#' 
#' \describe{
#' \item{\code{anoref}}{ano de referencia para geracao do cenario}
#' \item{\code{bacia}}{bacia a qual a ENA corresponde}
#' \item{\code{cenario}}{indice do cenario com respeito a bacia e ano de referencia}
#' \item{\code{data}}{data a qual o valor de ENA corresponde}
#' \item{\code{ena}}{valor de energia afluente}
#' }
#' 
#' Adicionalmente contem os atributos
#' 
#' \itemize{
#' \item{\code{bacias}: }{vetor de caracters contendo as bacias presentes no dado}
#' \item{\code{datas}: }{vetor de datas contendo as datas para as quais foram gerados cenarios}
#' \item{\code{anos}: }{vetor contendo os anos de referencia -- o tipo vai depender do dado lido}
#' \item{\code{ncen}: }{inteiro indicando numero de cenarios por bacia por ano de referencia}
#' }
"cenariosdummy"