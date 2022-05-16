############################# FUNCOES PARA REDUCAO DE DIMENSIONALIDADE #############################

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

invtransfpca <- function(pca, importance) {
    SIGMA <- pca$rotation[, seq(importance)]
    SIGMA <- t(SIGMA)
    out <- function(newdata) newdata %*% SIGMA
    return(out)
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

acumulaena <- function(cenarios, quebras = 3L) {

    dat <- copy(cenarios$cenarios)
    dat[, ena := (ena - min(ena)) / diff(range(ena))]

    out <- dat[, list(ind = seq(quebras), ena = stepcumsum(ena, quebras)),
        by = c("anoref", "bacia", "cenario")]

    new_compactcen(out, "acumulaena", NULL)
}

stepcumsum <- function(x, qbr) {
    if(length(qbr) == 1) qbr <- floor(seq(1, length(x), length.out = qbr + 1)[-1])
    out <- sapply(qbr, function(q) sum(x[seq(q)]))
    out <- unname(out)
    return(out)
}