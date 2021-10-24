require(tidyverse)

source('R/pbp_functions.R')
source('R/phf_schedule.R')
source('R/utils.R')

lst <- list()

for (a in 2016:2021) {

  b <- phf_schedule(season = a)

  lst[[a]] <- b

}

gms <- bind_rows(lst)
