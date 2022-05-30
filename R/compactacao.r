############################# FUNCOES PARA REDUCAO DE DIMENSIONALIDADE #############################

#' Funcoes de compactacao
#' 
#' Wrappers de metodos de compactacao para uso nos processos desse pacote
#' 
#' Durante a execucao das funcoes de selecao e/ou extracao de cenarios, o objeto \code{cenariosena}
#' passa por uma reducao de dimensionalidade e posterior clusterizacao. As funcoes para compactacao
#' do dado podem implementar qualquer metodo, mas devem respeitar alguns padroes de argumentos de 
#' entrada e caracteristicas na saida.
#' 
#' Quanto as entradas, qualquer funcao implementada com este proposito deve receber na primeira 
#' posicao o argumento \code{cenarios}, um objeto da classe \code{cenariosena}. Outros argumentos
#' especificos a cada metodo de compactacao podem seguir o primeiro, com qualquer nome (estes 
#' serao passados a chamada via o argumento \code{compact_args} das funcoes de selecao/extracao).
#' 
#' Os objetos retornados pelas funcoes de compactacao devem ser da classe \code{compactcen}. A 
#' geracao destes objetos e feita pela funcao \code{\link{new_compactcen}}. Essencialmente deve 
#' receber um datatable com estrutura igual a de objetos \code{cenariosena}, exceto pela coluna 
#' \code{data} que muda para \code{ind}, indicando o indice da variavel compactada para cada 
#' cenario. Consulte a pagina de ajuda do construtor para mais detalhes.
#' 
#' Atualmente o pacote fornece duas opcoes:
#' 
#' \itemize{
#' \item{\code{\link{PCAena}}}
#' \item{\code{\link{acumulaena}}}
#' }
#' 
#' As paginas de help de cada uma das funcoes contem detalhes a respeito dos argumentos de cada uma
#' (que podem ser passados as funcoes de selecao atraves de \code{compact_args} naquelas chamadas).
#' 
#' @seealso Funcoes \code{\link{PCAena}} e \code{\link{acumulaena}} para compactacao de dados
#' 
#' @name compact_funs
NULL

#' Compactacao Por PCA
#' 
#' Reduz dimensionalidade dos cenarios via PCA mantendo um minimo de variacao total
#' 
#' A matriz de dados considerada para reducao de dimensionalidade tem em cada linha o cenario e,
#' em cada coluna, os passos a frente de simulacao por bacia. Desta forma, cem cenarios 15 passos a
#' frente de duas bacias corresponde a uma matriz de dados 100 por 30. Serao selecionadas as n 
#' primeiras componentes principais que representem no minimo \code{vartot} por cento da variacao
#' total. Deve ser observado que o dado e normalizado para media zero e variancia um antes da 
#' compactacao.
#' 
#' @param cenarios objeto da classe \code{cenariosena} contendo apenas um ano de referencia
#' @param vartot percentual em formato decimal de variacao total mininima
#' 
#' @return objeto da classe \code{compactcen} contendo o dado em dimensao reduzida. Este e uma lista
#'     de um elemento nomeado \code{"compact"}, um \code{data.table} com as colunas
#' 
#' \describe{
#' \item{\code{anoref}}{ano de referencia para geracao do cenario}
#' \item{\code{bacia}}{bacia a qual a ENA corresponde}
#' \item{\code{cenario}}{indice do cenario com respeito a bacia e ano de referencia}
#' \item{\code{ind}}{indice do elemento no vetor de dimensao reduzida}
#' \item{\code{ena}}{valor de energia afluente}
#' }
#' 
#' Adicionalmente tem os atributos
#' 
#' \itemize{
#' \item{\code{metodo}: }{string do nome da funcao chamada para compactacao}
#' \item{\code{teminv}: }{booleano indicando se a compactacao possui inversa}
#' \item{\code{invfunc}: }{caso tenha inversa, a funcao que recebe vetores no espaco compactado e 
#'     retorna no espaco original}
#' }
#' 
#' @examples 
#' 
#' # usando o dado exemplo do pacote
#' 
#' # a compactacao deve ser usada para cada ano de referencia individualmente
#' cens <- cenariosdummy["A1"]
#' 
#' # Compactando apenas os cenarios relativos ao SIN completo
#' cens_compact <- PCAena(cens[, "SIN"])
#' \dontrun{
#' plot(cens_compact)
#' }
#' 
#' @seealso \code{\link{plot.compactcen}} para visualizacao das compactacoes
#' 
#' @export

PCAena <- function(cenarios, vartot = .8) {

    ind <- anoref <- bacia <- cenario <- NULL

    if(vartot > 1) vartot <- vartot / 100

    dat <- copy(cenarios$cenarios)

    pca <- dcast(dat, cenario ~ bacia + data, value.var = "ena")[, -1]
    pca <- prcomp(pca, scale = TRUE)

    if(vartot < 0) {
        importance <- 1
    } else {
        importance <- which(summary(pca)$importance[3, ] >= vartot)[1]
    }

    compdat <- pca$x[, seq(importance), drop = FALSE]
    compdat <- cbind(cenario = seq(nrow(compdat)), as.data.table(compdat))
    compdat <- melt(compdat, id.vars = "cenario", variable.name = "ind", value.name = "ena")
    compdat[, ind := as.numeric(sub("[[:alpha:]]*", "", ind))]

    out <- cbind(anoref = dat$anoref[1], bacia = paste0(attr(cenarios, "bacias"), collapse = "."), compdat)
    setorder(out, anoref, bacia, cenario, ind)

    new_compactcen(out, "PCAena", invtransfpca(pca, importance))
}

#' Calcula ENA Acumulada
#' 
#' Reducao de dimensao por ENA acumulada, possivelmente em partes crescentes
#' 
#' Esta funcao reduz a dimensao de um vetor de cenarios de ENA calculando a soma acumulada do vetor,
#' possivelmente em partes. Deve ser notado que, antes da compactacao, os cenarios sao escalonados
#' para o intervalo \eqn{[0, 1]} de modo a obter valores de menor variancia. Esta regularizacao e feita 
#' por bacia individualmente. Isto objetiva determinar um espaco reduzido que capture principalmente
#' o perfil de cada cenario em cada regiao, independentemente da magnitude.
#' 
#' Se \code{quebras = 3L}, por exemplo, a funcao quebra o cenario em tres
#' partes iguais e calcula a soma de cada um. O vetor em dimensao reduzida e, entao, a soma 
#' acumulada destas partes. Para cenarios de um ano, indo de janeiro a dezembro, o uso de 
#' \code{quebras = 3L} representa o calculo da ENA acumulada ate abril, agosto e dezembro (por 
#' construcao um vetor de valores crescentes).
#' 
#' @param cenarios objeto da classe \code{cenariosena} contendo apenas um ano de referencia
#' @param quebras ou um inteiro indicando em quantas partes iguais separar o dado ou um vetor de 
#'     inteiros indicando as posicoes nas quais separar
#' 
#' @return objeto da classe \code{compactcen} contendo o dado em dimensao reduzida. Este e uma lista
#'     de um elemento nomeado \code{"compact"}, um \code{data.table} com as colunas
#' 
#' \describe{
#' \item{\code{anoref}}{ano de referencia para geracao do cenario}
#' \item{\code{bacia}}{bacia a qual a ENA corresponde}
#' \item{\code{cenario}}{indice do cenario com respeito a bacia e ano de referencia}
#' \item{\code{ind}}{indice do elemento no vetor de dimensao reduzida}
#' \item{\code{ena}}{valor de energia afluente}
#' }
#' 
#' Adicionalmente tem os atributos
#' 
#' \itemize{
#' \item{\code{metodo}: }{string do nome da funcao chamada para compactacao}
#' \item{\code{teminv}: }{booleano indicando se a compactacao possui inversa}
#' \item{\code{invfunc}: }{caso tenha inversa, a funcao que recebe vetores no espaco compactado e 
#'     retorna no espaco original}
#' }
#' 
#' @examples 
#' 
#' # usando o dado exemplo do pacote
#' 
#' # a compactacao deve ser usada para cada ano de referencia individualmente
#' cens <- cenariosdummy["A1"]
#' 
#' # Compactando apenas os cenarios relativos ao SIN completo
#' cens_compact <- acumulaena(cens[, "SIN"], quebras = 2L)
#' \dontrun{
#' plot(cens_compact)
#' }
#' 
#' @seealso \code{\link{plot.compactcen}} para visualizacao das compactacoes
#' 
#' @export

acumulaena <- function(cenarios, quebras = 1L) {

    ena <- ena2 <- NULL

    if(quebras < 0) quebras <- length(attr(cenarios, "datas"))

    dat <- copy(cenarios$cenarios)

    # por algum motivo inexplicavel, ena := da erro mas assim funciona
    dat[, ena2 := (ena - min(ena)) / diff(range(ena)), by = "bacia"]
    dat[, ena := ena2]
    dat[, ena2 := NULL]

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
