library(readr)
library(tidyr)
library(stringr)
library(purrr)
library(dplyr)

set.seed(5)


test_dirs <- c('amr', 'covid19', 'ntd','outbreaks', 'tb')

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
  reduce(bind_rows) |>
  filter(!name == "selftests_meta_cols")



write_csv(data, paste0(here::here(), "/data/testdir_explorer/data_all_testdir.csv"))
write_csv(meta_cols, paste0(here::here(), "/data/testdir_explorer/all_meta_cols.csv"))


