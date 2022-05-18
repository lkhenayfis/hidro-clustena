####################################### VISUALIZACAO DE DADOS ######################################

#' Visualizacao De \code{cenariosena}
#' 
#' Plot em grade de objetos \code{cenariosena}
#' 
#' \code{y} permite que seja passado outro dado de cenarios que sera plotado por cima de \code{x}. O
#' uso deste argumento e normalmente voltado ao highlight de cenarios selecionados e/ou extraidos do
#' original, para fins de comparacao.
#' Os cenarios em \code{y} serao coloridos de acordo com a ENA acumulada de cada um.
#' 
#' @param x objeto da classe \code{cenariosena}
#' @param y objeto da classe \code{cenariosena} a ser plotado por cima. Ver Detalhes
#' @param print booleano indicando se o plot deve ser exibido ou retornado invisivelmente
#' @param ... nao possui funcao -- existe apenas para consistencia com a generica
#' 
#' @importFrom ggplot2 ggplot aes geom_line facet_grid theme theme_bw scale_x_date
#' @importFrom ggplot2 scale_color_discrete element_line element_text labs
#' 
#' @export

plot.cenariosena <- function(x, y, ..., print = TRUE) {

    bacia <- cenario <- data <- ena <- acum <- tipo <- NULL

    nbacias <- length(attr(x, "bacias"))
    nanos   <- length(attr(x, "anos"))
    if(nbacias <= nanos) form <- anoref ~ bacia else form <- bacia ~ anoref

    x <- copy(x$cenarios)

    if(!missing(y)) {
        y <- copy(y$cenarios)
        y[, acum := sum(ena), by = c("anoref", "bacia", "cenario")]
        y[, tipo := factor(acum, labels = paste0("Cen.", seq(unique(cenario)))), by = c("anoref", "bacia")]
        y[, seq_along(unique(cenario)), by = c("anoref", "bacia")]
    } else {
        y <- cbind(x[is.na(bacia)], tipo = numeric(0))
    }

    g <- ggplot() +
        geom_line(data = x, aes(data, ena, group = cenario), color = "grey80", alpha = .4) +
        geom_line(data = y, aes(data, ena, group = cenario, color = tipo)) +
        scale_color_discrete(name = "") +
        scale_x_date(name = "Data", breaks = "1 month", date_labels = "%b/%Y") +
        labs(y = "ENA") +
        facet_grid(form, scales = "free") +
        theme_bw() +
        theme(axis.text.x = element_text(hjust = 1, angle = 50),
            panel.grid.minor = element_line(color = NA))

    if(print) print(g)

    invisible(g)
}