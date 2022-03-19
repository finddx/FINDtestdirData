# library(fs)
library(readr)
library(dplyr)

# fs::dir_create("data")

sample <-
  iris |>
  dplyr::sample_n(10)


write_csv(sample, "data/iris.csv")
