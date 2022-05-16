
#' Leitura Do Arquivo De Cenarios
#' 
#' Funcao auxiliar que le arquivo texto e formata como objeto da classe \code{cenariosena}
#' 
#' Espera-se um arquivo com formato especifico. As duas primeiras colunas devem conter o indice do 
#' cenario e bacia correspondente, respectivamente. As demais colunas devem corresponder aos passos
#' de tempo para os quais os cenarios foram gerados. Por exemplo, um arquivo contendo cinco cenarios 
#' gerados para 2022-01, 2022-02 e 2022-03 teria a forma
#' 
#' | cenario | bacia | anoref | 1/2022 | 2/2022 | 3/2022 |
#' | ------- | ----- | ------ | ------ | ------ | ------ |
#' | 1       | SIN   | AA     | XXXXXX | XXXXXX | XXXXXX | 
#' | 2       | SIN   | AA     | XXXXXX | XXXXXX | XXXXXX | 
#' | 3       | SIN   | AA     | XXXXXX | XXXXXX | XXXXXX |
#' | 4       | SIN   | AA     | XXXXXX | XXXXXX | XXXXXX |
#' | 5       | SIN   | AA     | XXXXXX | XXXXXX | XXXXXX |
#' 
#' XXXXXX e um placeholder para os valores de ena e AA um placeholder para os valores de ano de 
#' referencia. Nao existe um padrao para os anos, podendo ser qualquer tipo de dado (i.e. "A1", 
#' "ano_1", 2022 e etc).
#' 
#' O nome das colunas de cenarios nao precisa seguir esta formato, sendo possivel informar o padrao
#' de nome pelo argumento \code{pat_data}, que deve corresponder a uma string de formato de data.
#' Neste caso, \code{pat_data = "%m/$Y"}
#' 
#' @param arq caminho do arquivo a ser lido com os cenarios. Formato deve ser compativel com o 
#'     descrito em Detalhes
#' @param pat_data string indicando o formato de data dos nomes das colunas de cenario
#' 
#' @return objeto da classe \code{cenariosena} contendo dados do arquivo lido
#' 
#' @export

learqcenarios <- function(arq, pat_data = "%m/%Y") {

    temdia <- grepl("\\%d", pat_data)
    pat_data_2 <- ifelse(temdia, pat_data, paste0("%d__", pat_data))

    dat <- fread(arq)

    coldata <- as.Date(paste0("01__", colnames(dat)), format = pat_data_2)
    colnames(dat)[!is.na(coldata)] <- as.character(coldata[!is.na(coldata)])

    dat <- melt(dat, id.vars = colnames(dat)[is.na(coldata)], value.name = "ena", variable.name = "data")
    dat[, data := as.Date(as.character(data))]
    setorder(dat, cenario, bacia, data)

    new_cenariosena(dat)
}

new_cenariosena <- function(dat) {

    out <- list(cenarios = dat[, .SD, .SDcols = c("anoref", "bacia", "data", "ena")])

    class(out) <- "cenariosena"

    attr(out, "bacias") <- unique(dat$bacia)
    attr(out, "datas")  <- unique(dat$data)
    attr(out, "anos")   <- unique(dat$anoref)
    attr(out, "ncen")   <- nrow(dat) / with(attributes(out), length(bacias) * length(datas) * length(anos))

    return(out)
}