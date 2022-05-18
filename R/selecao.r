######################### FUNCOES PARA CLUSTERIZAR E/OU SELECIONAR CENARIOS ########################

selecporquantil <- function(cenarios, quantis = c(.25, .5, .75), compact_fun = acumulaena,
    compact_args = list()) {

    compact_call <- c(list(compact_fun, cenarios), compact_args)
    compact <- eval(as.call(compact_call))

    dat <- compact$compact
    dat <- dcast(dat, cenario ~ ind, value.var = "ena")[, -1]

    if(ncol(dat) > 1) stop("A compactacao para selecao por quantil deve ser em apenas uma dimensao")

    dat_quant <- quantile(dat[[1]], quantis)
    maisprox  <- unname(sapply(dat_quant, function(dq) which.min((dat[[1]] - dq)^2)))

    return(maisprox)
}

selecporcluster <- function(cenarios, nc, clust_fun = clustkmeans, ..., transforma = FALSE,
    compact_fun = PCAena, compact_args = list()) {

    compact_call <- c(list(compact_fun, cenarios), compact_args)
    compact <- eval(as.call(compact_call))

    dat <- compact$compact
    dat <- dcast(dat, cenario ~ ind, value.var = "ena")[, -1]

    cluster_call <- c(list(clust_fun, dat, nc), list(...))
    cluster <- eval(as.call(cluster_call))

    if(attr(compact, "teminv") & transforma) {
        stop("Transformacao inversa ainda nao implementada")
    } else {

        # calcula elementos mais proximos de cada centroide

        novaordem <- split(seq(nrow(dat)), getclustclass(cluster))

        dat_clust <- split(dat, getclustclass(cluster))
        clstmeans <- getclustmeans(cluster)
        clstmeans <- lapply(seq(dat_clust), function(i) clstmeans[i, ])

        maisprox <- mapply(dat_clust, clstmeans,
            FUN = function(dc, cm) rowSums(mapply(dc, cm, FUN = "-")^2), SIMPLIFY = FALSE)
        maisprox <- lapply(maisprox, which.min)
        maisprox <- mapply(novaordem, maisprox, FUN = function(v, i) v[i])

        return(maisprox)
    }
}