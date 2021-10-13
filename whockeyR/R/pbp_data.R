# source('../pbp_functions.R')

pkgload::load_all()

a <- Sys.time()

rg <- load_pbp(game_id = 268078)
ot <- load_pbp(game_id = 268116)
so <- load_pbp(game_id = 268123)

Sys.time() - a
