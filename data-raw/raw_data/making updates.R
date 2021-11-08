source('R/pbp_functions.R')
source('R/phf_schedule.R')
source('R/utils.R')

library(tidyverse)

games <- c(419904, 419905, 419902, 419903, 419901)

g <- fastRhockey::load_phf_boxscore(game_id = 419903)

pbp <- fastRhockey::load_phf_pbp(game_id = 419903)

ss <- list()

for (y in 2016:2022) {

  season <- phf_schedule(season = y)

  ss[[y]] <- season

}

s <- dplyr::bind_rows(ss)
