devtools::load_all()
cenariosdummy <- learqcenarios("data-raw/cenarios.csv")
usethis::use_data(cenariosdummy, overwrite = TRUE)
