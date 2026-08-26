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
state_scores_plot <- tbl(con, "state_scores") |>
  filter(year %in% c(2015, 2023), !is.na(rla_score)) |>
  select(stateabb, state_name, year, rla_score) |>
  collect() |>
  pivot_wider(names_from = year, values_from = rla_score) |>
  filter(!is.na(`2015`), !is.na(`2023`)) |>
  mutate(
    rla_change = `2023` - `2015`,
    change_direction = case_when(
      rla_change > 0 ~ "Positive",
      rla_change < 0 ~ "Negative",
      TRUE ~ "No change"
    ),
    state_name = fct_reorder(state_name, `2023`)
  )

ggplot(state_scores_plot) +
  geom_segment(
    aes(
      x = `2015`,
      xend = `2023`,
      y = state_name,
      yend = state_name,
      color = change_direction
    ),
    arrow = grid::arrow(length = grid::unit(0.12, "inches"), type = "closed")
  ) +
  geom_point(aes(x = `2015`, y = state_name), color = "gray50", size = 2) +
  scale_color_manual(
    values = c("Positive" = "#2E8B57", "Negative" = "#C44E52", "No change" = "#7A7A7A")
  ) +
  labs(
    title = "State Reading Scores: 2015 to 2023",
    subtitle = "States are ordered by their 2023 reading score",
    caption = "Gray dots mark 2015 scores; arrows point to 2023 scores.",
    x = "Reading score",
    y = NULL,
    color = "Change direction"
  )
#
#
#
atus_con <- dbConnect(duckdb(), "data/atus.duckdb", read_only = TRUE)
dbplyr_act <- tbl(atus_con, "activities")
dbplyr_resp <- tbl(atus_con, "respondents")
dbplyr_codes <- tbl(atus_con, "activity_codes")
table_counts <- tibble(
  table = c("activities", "respondents", "activity_codes"),
  rows = c(
    dbplyr_act |> summarise(rows = n()) |> collect() |> pull(rows),
    dbplyr_resp |> summarise(rows = n()) |> collect() |> pull(rows),
    dbplyr_codes |> summarise(rows = n()) |> collect() |> pull(rows)
  )
)
table_counts
dbplyr_act |>
  left_join(dbplyr_codes, by = "activity_code") |>
  count(major_name, sort = TRUE) |>
  collect()
dbplyr_act |>
  left_join(dbplyr_codes, by = "activity_code") |>
  group_by(major_name) |>
  summarise(average_duration_min = mean(duration_min, na.rm = TRUE)) |>
  arrange(desc(average_duration_min)) |>
  collect()
dbplyr_resp |>
  count(sex, employment_status) |>
  collect()
dbplyr_resp |>
  count(year) |>
  arrange(year) |>
  collect()
activity_totals <- dbplyr_act |>
  inner_join(
    dbplyr_resp |>
      filter(day_type == "Weekday", employment_status == "Not in labor force") |>
      select(tucaseid, sex),
    by = "tucaseid"
  ) |>
  inner_join(
    dbplyr_codes |> select(activity_code, major_name),
    by = "activity_code"
  ) |>
  group_by(tucaseid, sex, major_name) |>
  summarise(minutes = sum(duration_min, na.rm = TRUE)) |>
  collect()

activity_avg <- activity_totals |>
  group_by(sex, major_name) |>
  summarise(mean_minutes = mean(minutes), .groups = "drop") |>
  arrange(sex, desc(mean_minutes))
activity_avg
#
#
#
keep_cats <- c(
  "Personal Care Activities",
  "Household Activities",
  "Caring For & Helping Household (HH) Members",
  "Socializing, Relaxing, and Leisure"
)

hourly_avg <- dbplyr_act |>
  mutate(hour = start_hhmm %/% 100L) |>
  filter(hour >= 6L, hour <= 23L) |>
  left_join(dbplyr_codes |> select(activity_code, major_name), by = "activity_code") |>
  filter(major_name %in% keep_cats) |>
  inner_join(
    dbplyr_resp |>
      filter(employment_status == "Not in labor force", day_type == "Weekday") |>
      select(tucaseid, sex),
    by = "tucaseid"
  ) |>
  group_by(sex, hour, major_name) |>
  summarise(avg_min = mean(duration_min, na.rm = TRUE), .groups = "drop") |>
  collect()
hourly_avg
#
#
#
activity_avg |>
  filter(major_name != "Data Codes") |>
  mutate(major_name = fct_reorder(major_name, mean_minutes)) |>
  ggplot(aes(x = major_name, y = mean_minutes, fill = sex)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Average Daily Minutes by Major Activity Category",
    subtitle = "Weekday respondents not in the labor force",
    x = "Major activity category",
    y = "Average daily minutes",
    fill = "Sex",
    caption = "Source: American Time Use Survey (ATUS) 2003–2024."
  )
#
#
#
#
#
