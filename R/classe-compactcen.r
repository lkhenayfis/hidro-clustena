############################ FUNCOES ASSOCIADAS A COMPACTACAO DE CENARIOS ##########################

#' Construtor De \code{compactcen}
#' 
#' Construtor interno de objetos com cenarios compactados
#' 
#' O argumento \code{compact} deve seguir um padrao de formato:
#' 
#' \describe{
#' \item{\code{anoref}}{ano de referencia para geracao do cenario}
#' \item{\code{bacia}}{bacia a qual a ENA corresponde}
#' \item{\code{cenario}}{indice do cenario com respeito a bacia e ano de referencia}
#' \item{\code{ind}}{indice do elemento no vetor de dimensao reduzida}
#' \item{\code{ena}}{valor de energia afluente}
#' }
#' 
#' @param compact data.table contendo a informacao de cenarios compactados. Ver Detalhes
#' @param metodo string indicando o nome da funcao utilizada para compactacao
#' @param infunc caso exista, funcao para transformar vetores no espaco reduzido de volta ao 
#'     original
#' 
#' @return Objeto da classe \code{compactcen}, uma lista de um emento chamado compact contendo o
#'     argumento \code{compact}
#' 
#' Adicionalmente contem os atributos
#' 
#' \itemize{
#' \item{\code{metodo}: }{string do nome da funcao chamada para compactacao}
#' \item{\code{teminv}: }{booleano indicando se a compactacao possui inversa}
#' \item{\code{invfunc}: }{caso tenha inversa, a funcao que recebe vetores no espaco compactado e 
#'     retorna no espaco original}
#' }

new_compactcen <- function(compact, metodo, invfunc) {

    teminv <- !is.null(invfunc)

    out <- list(compact = compact)

    class(out) <- c("compactcen")
    attr(out, "metodo")  <- metodo
    attr(out, "teminv")  <- teminv
    if(teminv) attr(out, "invfunc") <- invfunc

    return(out)
}

# METODOS ------------------------------------------------------------------------------------------

rbind.compactcen <- function(..., deparse.level = 1L) {
    x <- list(...)
    nx <- length(x)

    if(nx == 1) {
        return(x[[1]])
    } else if(nx == 2) {
        x1 <- x[[1]]$compact
        x2 <- x[[2]]$compact

        new_compactcen(rbind(x1, x2), attr(x[[1]], "metodo"), attr(x[[1]], "invfunc"))
    } else if(nx > 2) {
        rbind(x[[1]], do.call(rbind, x[-1]))
    }
}