library(data.table)

formatadata <- function(data) {
    data <- strsplit(as.character(data), "/")
    data <- lapply(data, function(x) c(formatC(as.numeric(x[1]), digits = 1, width = 2, flag = "0"), x[2]))
    data <- lapply(data, function(x) paste0(c("01", x), collapse = "/"))
    sapply(data, as.Date, format = "%d/%m/%Y")
}

dat <- fread("data-raw/cenarios.csv")
dat[, anoref := paste0("ano", rep(seq(3), each = 100))]

dat <- melt(dat, id.vars = c("cenario", "bacia"), variable.name = "data", value.name = "ena")



data <- dat$data[1:20]
