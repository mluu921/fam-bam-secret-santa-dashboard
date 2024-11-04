library(tidyverse)
library(gt)
library(networkD3)
library(pins)
library(plotly)

board <- pins::board_folder('board')

data <- pins::pin_read(board, 'processed-data')

gender_data <- data |> 
  select(participants_giver, gender_giver) |> 
  distinct()

data <- data |> drop_na()

participants <- unique(data$participants_giver)

# Function to perform the Secret Santa draw
secret_santa <- function(participants) {
  repeat {
    # Randomly shuffle participants
    matches <- sample(participants)
    
    # Check if any participant is matched to themselves
    if (!any(participants == matches)) {
      return(tibble(participant = participants, match = matches))
    }
  }
}

# -------------------------------------------------------------------------

sims <- tibble(i = 1:100000)

set.seed(1)
sims$draws <- map(sims$i, \(x) secret_santa(participants), .progress = TRUE)

sims <- sims |> 
  unnest(draws)

sims <- sims |> 
  left_join(gender_data, by = c('participant' = 'participants_giver')) |> 
  left_join(gender_data, by = c('match' = 'participants_giver')) |> 
  rename('gender_participant' = gender_giver.x) |> 
  rename('gender_match' = gender_giver.y) |> 
  separate_wider_delim(participant, names = c('participant_first', 'participant_last'), delim = ' ') |> 
  separate_wider_delim(match, names = c('match_first', 'match_last'), delim = ' ')

sims <- sims |> 
  mutate(
    concordant_gender = ifelse(gender_participant == gender_match, 1, 0)
  )


# -------------------------------------------------------------------------


plot_data <- sims  |> 
  select(i, participant_last, match_last) |> 
  count(participant_last, match_last) |> 
  group_by(participant_last) |>
  mutate(prob = n / sum(n)) |> 
  ungroup() |> 
  select(-n)

prob_range <- c(min(plot_data$prob), max(plot_data$prob))

plot_data <- plot_data |> 
  ungroup() |> 
  pivot_wider(
    names_from = match_last,
    values_from = prob
  )

tbl <- plot_data |> 
  gt() |> 
  fmt_percent(decimals = 0) |> 
  fmt_missing(missing_text = '') |> 
  cols_label(participant_last = '') |> 
  data_color(
    columns = 2:ncol(plot_data),
    palette = 'viridis',
    domain = prob_range
  )

tbl
  
pins::pin_write(board, tbl, 'sims-tbl-prob-last-name', type = 'rds')


# -------------------------------------------------------------------------

plot_data <- sims |> 
  select(gender_participant, gender_match) |> 
  count(gender_participant, gender_match) |> 
  mutate(prob = n / sum(n)) |> 
  select(
    -n
  ) 

prob_range <- c(min(plot_data$prob), max(plot_data$prob))

plot_data <- plot_data |> 
  pivot_wider(
    names_from = gender_match,
    values_from = prob
  )

tbl <- plot_data |> 
  gt() |> 
  fmt_percent(decimals = 0) |> 
  fmt_missing(missing_text = '') |> 
  cols_label(gender_participant = '') |> 
  data_color(
    columns = 2:ncol(plot_data),
    palette = 'viridis',
    domain = prob_range
  )

tbl

pins::pin_write(board, tbl, 'sims-tbl-prob-gender', type = 'rds')
