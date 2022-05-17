####################################### VISUALIZACAO DE DADOS ######################################

#' Visualizacao De \code{cenariosena}
#' 
#' Plot em grade de objetos \code{cenariosena}
#' 
#' @param x objeto da classe \code{cenariosena}
#' @param print booleano indicando se o plot deve ser exibido ou retornado invisivelmente
#' @param ... nao possui funcao -- existe apenas para consistencia com a generica
#' 
#' @importFrom ggplot2 ggplot aes geom_line facet_grid theme theme_bw scale_x_date
#' @importFrom ggplot2 element_line element_text labs
#' 
#' @export

plot.cenariosena <- function(x, print = TRUE, ...) {

    data <- ena <- cenario <- NULL

    nbacias <- length(attr(x, "bacias"))
    nanos   <- length(attr(x, "anos"))
    if(nbacias <= nanos) form <- anoref ~ bacia else form <- bacia ~ anoref

    x <- x$cenarios

    g <- ggplot(x, aes(data, ena, group = cenario)) + geom_line(color = "skyblue2", alpha = .4) +
        scale_x_date(name = "Data", breaks = "1 month", date_labels = "%b/%Y") +
        labs(y = "ENA") +
        facet_grid(form, scales = "free") +
        theme_bw() +
        theme(axis.text.x = element_text(hjust = 1, angle = 50),
            panel.grid.minor = element_line(color = NA))

    if(print) print(g)

    invisible(g)
}