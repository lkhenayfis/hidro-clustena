test_that("Testes de clusterizacao", {

    set.seed(1234)

    cens <- cenariosdummy["A1", "SIN"]
    compac <- PCAena(cens)

    # KMEANS

    clust <- clustkmeans(compac, 5)

    expect_equal(class(clust), "kmeans")
    expect_snapshot_value(getclustclass(clust), style = "deparse")
    expect_snapshot_value(getclustmeans(clust), style = "deparse")

    gg <- plot(compac, clust, print = FALSE)
    expect_equal(class(gg), c("plotly", "htmlwidget"))

    # EM

    clust <- clustEM(compac, 5)

    expect_equal(class(clust), "Mclust")
    expect_snapshot_value(getclustclass(clust), style = "deparse")
    expect_snapshot_value(getclustmeans(clust), style = "deparse")

    gg <- plot(compac, clust, print = FALSE)
    expect_equal(class(gg), c("plotly", "htmlwidget"))
})