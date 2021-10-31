library(fastRhockey)
library(tidyverse)

h <- phf_schedule(season = 2016)

lst <- list()

for (k in 2017:2021) {
  
  current <- phf_schedule(season = k) %>%
    dplyr::filter(has_play_by_play == TRUE) %>%
    dplyr::select(game_id)
  
  lst[[k]] <- current
  
  print(k)
  
}

games <- dplyr::bind_rows(lst) %>%
  dplyr::mutate(order = row_number())

lst2 <- list()
mx <- max(games$order, na.rm = TRUE)

for (z in 1:mx) {
  
  gm <- games %>%
    dplyr::filter(order == z) %>%
    dplyr::select(game_id) %>%
    dplyr::pull(game_id)
  
  df <- load_pbp(game_id = gm)
  
  lst2[[z]] <- df
  
  print(z)
  
}

fast_pbp <- dplyr::bind_rows(lst2)

  
  