library(tidyverse)
library(gt)
library(networkD3)
library(pins)
library(plotly)
library(gtExtras)

board <- pins::board_folder('board')

data <- pins::pin_read(board, 'processed-data')

data <- data |> drop_na()

participants <- unique(data$participants_giver)

tbl_data <- data |> 
  select(year, participants_giver, participants_receiver) |> 
  mutate(matched = 1) |> 
  complete(nesting(year, participants_giver), participants_receiver, fill = list(matched = 0)) |> 
  filter(participants_giver != participants_receiver) |> 
  select(-year) |> 
  distinct() |>
  group_by(participants_giver) |> 
  summarise(prob_repeat = (sum(matched == 1) / n()) * 100,
            prob_non_repeat = ( sum(matched == 0) / n()) * 100)

tbl <- tbl_data |>
  gt() |>
  gt_plt_bar_pct(
    column = prob_repeat,
    scaled = TRUE,
    fill = "red",
    labels = TRUE
  ) |>
  gt_plt_bar_pct(
    column = prob_non_repeat,
    scaled = TRUE,
    fill = "forestgreen",
    labels = TRUE
  ) |>
  cols_label(
    participants_giver = "Participant",
    prob_repeat = "Probability of repeat",
    prob_non_repeat = "Probability of non-repeat"
  ) |>
  cols_width(prob_repeat ~ px(200),
             prob_non_repeat ~ px(200))

pins::pin_write(board, tbl, "tbl-prob-repeat-non-repeat", type = 'rds')
