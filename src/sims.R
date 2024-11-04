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

sims <- tibble(i = 1:10000)

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

plot_data <- sims  |> 
  select(i, participant_last, match_last) |> 
  group_by(i, participant_last, match_last) |> 
  count() |> 
  group_by(i) |> 
  mutate(prob = n / sum(n)) |> 
  ungroup() |> 
  group_by(participant_last, match_last) |> 
  summarise(prob = mean(prob))

p <- ggplot(plot_data, aes(x = participant_last, y = match_last, fill = prob)) +
  geom_tile() +
  geom_text(aes(label = scales::percent(prob, .1)), vjust = 1) +
  scale_fill_viridis_c(labels = scales::label_percent(.1)) +
  theme_minimal(base_size = 15) +
  theme(legend.position = 'bottom', panel.grid = element_blank()) +
  guides(fill = guide_colorbar(
    barwidth = 20,
    barheight = 1,
    title.position = 'top',
  )) +
  labs(fill = 'Probability', x = NULL, y = NULL)

p <- plotly::ggplotly(p)

pins::pin_write(board, p, 'sims-plot-prob-last-name', type = 'rds')
