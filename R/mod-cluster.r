############################### WRAPPERS DE FUNCOES DE CLUSTERIZACAO ###############################

#' Funcoes De Clusterizacao
#' 
#' Wrappers de metodos de clusterizacao para uso nos processos do pacote
#' 
#' Durante a execucao das funcoes de selecao e/ou extracao de cenarios, o objeto \code{cenariosena}
#' passa por uma reducao de dimensionalidade e posterior clusterizacao. As funcoes para clusterizar
#' o dado podem implementar qualquer metodo, mas devem respeitar alguns padroes de argumentos de 
#' entrada e caracteristicas na saida.
#' 
#' Quanto as entradas, todas as funcoes de clusterizacao precisam receber o primeiro argumento 
#' chamado \code{mat}, correspondente a uma matrix ou data.frame-like contendo observacoes a serem
#' clusterizadas nas linhas e variaveis nas colunas (formato padrao). Alem de \code{mat}, o segundo
#' argumento deve ser \code{nc}, recebendo um inteiro indicando o numero de clustes desejados. Os 
#' demais argumentos sao livres, porem independentemente deles deve existir \code{...} para 
#' consistencia com as demais funcoes.
#' 
#' Em seguida, as caracteristicas de saida. Todas as funcoes utilizadas para clusterizacao devem
#' retornar um objeto com classe \code{"clustena"} e uma subclasse especifica do metodo. Por exemplo
#' a funcao \code{\link{clustkmeans}} retorna resultado com classe 
#' \code{c("clustenakmeans", "clustena")}. Alem disso, devem ser definidos metodos das genericas
#' \code{getclustclass} e \code{getclustmeans}, que extraem do objeto de saida as classificacoes de
#' cada obsevacao e centroides dos clusters, respectivamente.
#' 
#' Atualmente o pacote fornece duas opcoes:
#' 
#' \itemize{
#' \item{\code{clustkmeans}}
#' \item{\code{clustEM}}
#' }
#' 
#' As paginas de help de cada uma das funcoes contem detalhes a respeito dos argumentos de cada uma
#' (que podem ser passados as funcoes de selecao atraves de \code{...} naquelas chamadas).
#' 
#' @seealso clustkmeans clustEM
#' 
#' @name clust_funs
NULL

# GENERICAS ----------------------------------------------------------------------------------------

#' Genericas Para Classes Cluster
#' 
#' Funcoes genericas para extracao das classes ajustadas e centroides de cada classe
#' 
#' @param clust objeto com classe \code{clustena} e subclasse especifica do metodo
#' 
#' @return Para \code{getclustmeans}, os centroides; para \code{getclustclass} as classificacoes
#' 
#' @name getclust
NULL

#' @export 
#' 
#' @rdname getclust

getclustmeans <- function(clust) UseMethod("getclustmeans")

#' @rdname getclust

getclustmeans.default <- function(clust) {
    stop(paste0("Metodo 'getclustmeans' nao implementado para a classe ", class(clust)))
}

#' @export 
#' 
#' @rdname getclust

getclustclass <- function(clust) UseMethod("getclustclass")

#' @rdname getclust

getclustclass.default <- function(clust) {
    stop(paste0("Metodo 'getclustclass' nao implementado para a classe ", class(clust)))
}

# KMEANS -------------------------------------------------------------------------------------------

#' Clusteriza Dado Por Kmeans
#' 
#' Wrapper da funcao \code{\link[stats]{kmeans}} para uso neste pacote
#' 
#' @param mat matriz de dados para clusterizacao
#' @param nc numero de clusters
#' @param nstart numero de sementes para testar o kmeans
#' @param ... demais parametros passados a funcao \code{\link[stats]{kmeans}} exceto \code{nstart}
#' 
#' @return objeto da classe \code{clustenakmeans}: uma lista de um elemento contendo a saida da
#'     clusterizacao

clustkmeans <- function(mat, nc, nstart = 30, ...) {
    clusters <- kmeans(mat, nc, nstart = nstart, ...)
    clusters <- list(clusters = clusters)
    class(clusters) <- c("clustenakmeans", "clustena")

    return(clusters)
}

#' @param clust objeto da classe \code{clustenakmeans}
#' 
#' @rdname clustkmeans

getclustclass.clustenakmeans <- function(clust) clust[[1]]$cluster

#' @param clust objeto da classe \code{clustenakmeans}
#' 
#' @rdname clustkmeans

getclustmeans.clustenakmeans <- function(clust) clust[[1]]$centers

# EM MIXTURE GUASS ---------------------------------------------------------------------------------

#' Clusteriza Dado Por Mistura Gaussiana
#' 
#' Wrapper da funcao \code{\link[mclust]{Mclust}} para uso neste pacote
#' 
#' @param mat matriz de dados para clusterizacao
#' @param nc numero de clusters
#' @param ... demais parametros passados a funcao \code{\link[mclust]{Mclust}}
#' 
#' @return objeto da classe \code{clustenaem}: uma lista de um elemento contendo a saida da
#'     clusterizacao

clustEM <- function(mat, nc, ...) {
    if(!requireNamespace("mclust", quietly = TRUE)) {
        stop("Clusterizacao por EM requer o pacote 'mclust'")
    }

    # tem algum problema de implementacao do mclust que a funcao mclustBIC nao e encontrada a nao
    # ser que o pacote seja explicitamente importado no NAMESPACE. Como ele e uma dependencia
    # opcional isso se torna um problema
    # Para contorna-lo, e feito o bind do nome mclustBIC localmente a funcao homonima no namespace
    # do mclust
    # E uma maracturaia mas resolve o problema
    mclustBIC <- mclust::mclustBIC

    clusters <- mclust::Mclust(mat, G = nc, ...)
    clusters <- list(clusters = clusters)
    class(clusters) <- c("clustenaem", "clustena")

    return(clusters)
}

#' @param clust objeto da classe \code{clustenaem}
#' 
#' @rdname clustEM

getclustclass.clustenaem <- function(clust) clust[[1]]$classification

#' @param clust objeto da classe \code{clustenaem}
#' 
#' @rdname clustEM

getclustmeans.clustenaem <- function(clust) t(clust[[1]]$parameters$mean)
