library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)

set.seed(5)


test_dirs <- c('covid19_new', 'ntd','outbreaks', 'tb')

data <-
  list.files(path = paste0(here::here(), "/data/", test_dirs),
             pattern = "*testdir.csv",
             full.names = TRUE) |>
  map(
    ~ read_csv(.x) |>
      mutate(name = str_remove(basename(.x), "\\.csv"))
  ) |>
  reduce(bind_rows)



meta_cols <- list.files(path = paste0(here::here(), "/data/", test_dirs),
                        pattern = "*meta_cols.csv",
                        full.names = TRUE) |>
map(
  ~ read_csv(.x) |>
    mutate(name = str_remove(basename(.x), "\\.csv"))
) |>
  reduce(bind_rows)



write_csv(data, paste0(here::here(), "/data/testdir_explorer/data_all_testdir.csv"))
write_csv(meta_cols, paste0(here::here(), "/data/testdir_explorer/all_meta_cols.csv"))


