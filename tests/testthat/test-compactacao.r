test_that("Testes das funcoes de compactacao", {

    cens <- cenariosdummy["A1", "SIN"]

    # PCA

    compac <- PCAena(cens, vartot = 1)

    expect_equal(colnames(compac$compact), c("anoref", "bacia", "cenario", "ind", "ena"))
    expect_equal(class(compac), "compactcen")
    expect_equal(attr(compac, "metodo"), "PCAena")
    expect_true(attr(compac, "teminv"))

    reverse <- unname(attr(compac, "invfunc")(compac$compact[cenario == 1, ena]))[1, ]
    expect_snapshot_value(reverse, style = "deparse")

    expect_error(plot(compac, print = FALSE))

    compac <- PCAena(cens)

    gg <- plot(compac, print = FALSE)
    expect_equal(class(gg), c("plotly", "htmlwidget"))

    compac <- PCAena(cens, .75)

    gg <- plot(compac, print = FALSE)
    expect_equal(class(gg), c("gg", "ggplot"))

    # ACUMULADA

    compac <- acumulaena(cens, quebras = 5L)

    expect_equal(colnames(compac$compact), c("anoref", "bacia", "cenario", "ind", "ena"))
    expect_equal(class(compac), "compactcen")
    expect_equal(attr(compac, "metodo"), "acumulaena")
    expect_true(!attr(compac, "teminv"))

    expect_error(plot(compac, print = FALSE))

    compac <- acumulaena(cens, 3L)

    gg <- plot(compac, print = FALSE)
    expect_equal(class(gg), c("plotly", "htmlwidget"))

    compac <- acumulaena(cens, 2L)

    gg <- plot(compac, print = FALSE)
    expect_equal(class(gg), c("gg", "ggplot"))
})