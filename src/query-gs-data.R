library(tidyverse)
library(googlesheets4)
library(pins)

board <- pins::board_folder('board')

url <- 'https://docs.google.com/spreadsheets/d/1Lp6XU1Uk4lnROuFmwFCqwziw1u1f5MWKE1MSZ0Ddjfc/edit?gid=1789879022#gid=1789879022'


# -------------------------------------------------------------------------

query_gs <- \(url) {
  
  md <- gs4_get(url)
  
  sheets <- md$sheets |> pull(name)
  
  sheets <- sheets[sheets %in% 2014:year(Sys.Date())]
  
  datas <- map(sheets, \(x) {
    read_sheet(url, sheet = x) |> 
      mutate(across(everything(), \(x) as.character(x)))
  }) |> 
    set_names(sheets)
  
  data <- bind_rows(datas, .id = 'year')
  
}

data <- query_gs(url)

# -------------------------------------------------------------------------

demos <- read_sheet(url, sheet = 'participants')

demos <- demos |> 
  geocode(location)

data <- data |>
  group_by(year) |> 
  mutate(i = row_number()) |> 
  ungroup() |> 
  pivot_longer(-c(year, i), names_to = 'role', values_to = 'participants') |> 
  left_join(demos, by = join_by(participants)) |> 
  pivot_wider(names_from = role, values_from = c(participants, gender, location, lat, long))

pins::pin_write(board, data, 'processed-data', type = 'rds')
