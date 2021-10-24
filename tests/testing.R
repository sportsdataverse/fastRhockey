require(tidyverse)
require(rvest)
require(strex)
require(tokenizers)

source('R/pbp_functions.R')
source('R/phf_schedule.R')
source('R/utils.R')

a <- Sys.time()

df <- load_pbp(game_id = 268078)

Sys.time() - a

df1 <- load_boxscore(game_id = 268078)
