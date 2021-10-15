source('R/pbp_functions.R')

# pkgload::load_all()

a <- Sys.time()

rg <- load_pbp(game_id = 268078)
ot <- load_pbp(game_id = 268116)
so <- load_pbp(game_id = 268123)

Sys.time() - a

# g <- wehoop::espn_wnba_game_all(game_id = 401322870)
#
# g$Team

raw <- load_raw_data(game_id = 268078)

h <- c("away", "home")

ah <- data.frame(h)

tm <- raw[[max(length(raw)) - 1]] %>%
  clean_names() %>%
  mutate(
    order = row_number(),
    meta = ifelse(
      order == 1, "away_team", ifelse(
        order == 2, "home_team", NA
      )
    )
  ) %>%
  dplyr::select(shots, meta) %>%
  pivot_wider(values_from = shots,
              names_from = meta)

rg <- rg %>%
  right_join(tm, by = character())

rdw <- load_raw_data(game_id = 268116)

bdc <- read.csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_nwhl.csv")
