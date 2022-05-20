test_that("Testes de selecao de cenarios", {

    set.seed(1234)

    cens <- cenariosdummy["A1", "SIN"]

    sel <- selecporcluster(cens, 5)

    expect_equal(length(sel), 3)

    expect_equal(class(sel[[1]]), "integer")
    expect_equal(length(sel[[1]]), 5)

    expect_equal(class(sel[[2]]), "compactcen")
    expect_equal(class(sel[[3]]), "kmeans")

    sel <- selecporcluster(cens, 5, compact_fun = acumulaena, compact_args = list(quebras = 3),
        clust_fun = clustEM, verbose = FALSE)

    expect_equal(length(sel), 3)

    expect_equal(class(sel[[1]]), "integer")
    expect_equal(length(sel[[1]]), 5)

    expect_equal(class(sel[[2]]), "compactcen")
    expect_equal(class(sel[[3]]), "Mclust")
})