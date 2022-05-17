############################# FUNCOES PARA REDUCAO DE DIMENSIONALIDADE #############################

#' Compactacao Por PCA
#' 
#' Reduz dimensionalidade dos cenarios via PCA mantendo um minimo de variacao total
#' 
#' A matriz de dados considerada para reducao de dimensionalidade tem em cada linha o cenario e,
#' em cada coluna, os passos a frente de simulacao. Serao selecionadas as n primeiras componentes 
#' principais que representem no minimo \code{vartot} por cento da variacao total.
#' 
#' @param cenarios objeto da classe \code{cenariosena} contendo apenas uma bacia e ano de referencia
#' @param vartot percentual em formato decimal de variacao total mininima
#' 
#' @return objeto da classe \code{compactcen} contendo o dado em dimensao reduzida
#' 
#' @export

PCAena <- function(cenarios, vartot = .8) {

    dat <- copy(cenarios$cenarios)

    pca <- dcast(dat, cenario ~ data, value.var = "ena")[, -1]
    pca <- prcomp(pca, scale = TRUE)

    importance <- which(summary(pca)$importance[3, ] >= vartot)[1]

    compdat <- pca$x[, seq(importance)]
    compdat <- cbind(cenario = seq(nrow(compdat)), as.data.table(compdat))
    compdat <- melt(compdat, id.vars = "cenario", variable.name = "ind", value.name = "ena")
    compdat[, ind := sub("[[:alpha:]]*", "", ind)]

    out <- cbind(anoref = dat$anoref[1], bacia = dat$bacia[1], compdat)
    setorder(out, anoref, bacia, cenario, ind)

    new_compactcen(out, "PCAena", invtransfpca(pca, importance))
}

#' Calcula ENA Acumulada
#' 
#' Reducao de dimensao por ENA acumulada, possivelmente em partes crescentes
#' 
#' Esta funcao reduz a dimensao de um vetor de cenarios de ENA calculando a soma acumulada do vetor,
#' possivelmente em partes. Se \code{quebras = 3L}, por exemplo, a funcao quebra o cenario em tres
#' partes iguais e calcula a soma de cada um. O vetor em dimensao reduzida e, entao, a soma 
#' acumulada destas partes.
#' 
#' Por exemplo, para cenarios de um ano, indo de janeiro a dezembro, o uso de \code{quebras = 3L}
#' representa o calculo da ENA acumulada ate abril, agosto e dezembro (por construcao um vetor de
#' valores crescentes).
#' 
#' @param cenarios objeto da classe \code{cenariosena} contendo apenas uma bacia e ano de referencia
#' @param quebras ou um inteiro indicando em quantas partes iguais separar o dado ou um vetor de 
#'     inteiros indicando as posicoes nas quais separar
#' 
#' @return objeto da classe \code{compactcen} contendo o dado em dimensao reduzida
#' 
#' @export

acumulaena <- function(cenarios, quebras = 3L) {

    dat <- copy(cenarios$cenarios)
    dat[, ena := (ena - min(ena)) / diff(range(ena))]

    out <- dat[, list(ind = seq(quebras), ena = stepcumsum(ena, quebras)),
        by = c("anoref", "bacia", "cenario")]

    new_compactcen(out, "acumulaena", NULL)
}

# HELPERS ------------------------------------------------------------------------------------------

#' Funcao Inversa Do PCA
#' 
#' Retorna uma funcao que recebe vetores no espaco reduzido e projeta de volta no espaco original
#' 
#' @param pca objeto da classe \code{prcomp}
#' @param importance um inteiro indicando a ultima componente principal a manter
#' 
#' @return funcao que recebe um vetor no espaco reduzido e retorna projecao no espaco original

invtransfpca <- function(pca, importance) {
    SIGMA <- pca$rotation[, seq(importance)]
    SIGMA <- t(SIGMA)
    out <- function(newdata) newdata %*% SIGMA
    return(out)
}

#' Soma Acumulada Por Degraus
#' 
#' Calcula a soma acumulada de um vetor em partes
#' 
#' @param x vetor do qual calcular soma
#' @param qbr escalar ou vetor de inteiros indicando quantas partes ou onde separar as partes
#' 
#' @return vetor de ENA acumulada por partes

stepcumsum <- function(x, qbr) {
    if(length(qbr) == 1) qbr <- floor(seq(1, length(x), length.out = qbr + 1)[-1])
    out <- sapply(qbr, function(q) sum(x[seq(q)]))
    out <- unname(out)
    return(out)
}
