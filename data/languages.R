library(tidyverse)

path_data_raw <- "data/languages_raw.csv"
path_out <- "data/languages.csv"

data <- 
  read_csv(path_data_raw) |> 
  rename(total = Total, state = name) |>
  select(-`Speak only English`) |>
  pivot_longer(
    cols = -c(geoid, state, total),
    names_to = "language",
    values_to = "speakers"
  ) |>
  mutate(percent = speakers / total * 100) |> 
  select(-total) |> 
  write_csv(path_out)s
