#
#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(DBI)
library(duckdb)
library(dbplyr)
#
#
#
con <- dbConnect(duckdb(), "data/seda_2025.duckdb", read_only = TRUE)
dbplyr_dist <- tbl(con, "district_scores")
#
#
#
#| cache: true
#| cache.extra: !expr file.mtime("data/seda_2025.duckdb")
score_change <- dbplyr_dist |>
  filter(year %in% c(2015, 2023), !is.na(rla_score)) |>
  select(district_id, year, rla_score) |>
  collect() |>
  pivot_wider(names_from = year, values_from = rla_score) |>
  filter(!is.na(`2015`), !is.na(`2023`)) |>
  transmute(district_id, rla_change = `2023` - `2015`)
dbplyr_state <- tbl(con, "state_scores")
state_change <- dbplyr_state |>
  filter(year %in% c(2015, 2023), !is.na(rla_score)) |>
  select(stateabb, year, rla_score) |>
  collect() |>
  pivot_wider(names_from = year, values_from = rla_score) |>
  filter(!is.na(`2015`), !is.na(`2023`)) |>
  transmute(stateabb, rla_change = `2023` - `2015`)
state_change |> arrange(rla_change)
#
#
#
score_change |>
  mutate(
    change_direction = case_when(
      rla_change > 0 ~ "Positive",
      rla_change < 0 ~ "Negative",
      TRUE ~ "No change"
    )
  ) |>
  ggplot(aes(x = rla_change, fill = change_direction)) +
  geom_histogram(bins = 30, color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_manual(
    values = c("Positive" = "#2E8B57", "Negative" = "#C44E52", "No change" = "#7A7A7A")
  ) +
  labs(
    title = "Distribution of District Reading Score Changes",
    subtitle = paste("Number of districts:", nrow(score_change)),
    caption = "Change is calculated as the 2023 score minus the 2015 score.",
    x = "Reading score change",
    y = "Number of districts"
  )
#
#
#
#
#
