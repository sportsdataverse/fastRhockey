# 

# fastRhockey

[`fastRhockey`](https://github.com/sportsdataverse/fastRhockey) is an R
package for accessing hockey data from the **NHL** (National Hockey
League) and **PWHL** (Professional Women’s Hockey League) via public web
APIs. It provides structured data frames of play-by-play, schedule,
standings, roster, draft, player stats, and team data. The package also
includes integrated **expected goals (xG)** models trained via XGBoost.

Part of the [SportsDataverse](https://sportsdataverse.org/) family of
R/Python packages for sports analytics.

### Key Features

- **NHL Data** – Game feeds, boxscores, play-by-play with shifts,
  schedules, standings, rosters, draft picks, player/team stats,
  playoffs, and broadcast information via `api-web.nhle.com` and
  `api.nhle.com/stats`
- **NHL Edge Analytics** (33 functions) – Skater, goalie, and team
  advanced metrics from `api-web.nhle.com/v1/edge/...` covering shot
  location, shot speed, skating speed, skating distance, zone time,
  comparisons, landing pages, and top-10 leaderboards. Each wrapper
  accepts an optional `season` argument and falls back to the current
  season via the `/now` form
- **NHL Records API** (25 functions) – First-time integration with
  `records.nhl.com/site/api/` for franchise totals, player/skater/goalie
  career and real-time stats, draft lottery, hall of fame, trophies,
  awards, attendance, venues, officials, and combine data
- **NHL Stats REST** (19 dedicated wrappers) –
  [`nhl_stats_franchise()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_franchise.md),
  [`nhl_stats_players()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_players.md),
  [`nhl_stats_glossary()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_glossary.md),
  [`nhl_stats_country()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_country.md),
  [`nhl_stats_skater_leaders()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_skater_leaders.md),
  [`nhl_stats_goalie_leaders()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_goalie_leaders.md),
  [`nhl_stats_skater_milestones()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_skater_milestones.md),
  [`nhl_stats_goalie_milestones()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_goalie_milestones.md),
  plus the original
  [`nhl_stats_skaters()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_skaters.md)
  /
  [`nhl_stats_goalies()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_goalies.md)
  /
  [`nhl_stats_teams()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_teams.md)
  /
  [`nhl_stats_draft()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_draft.md)
  /
  [`nhl_stats_seasons()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_seasons.md)
  /
  [`nhl_stats_misc()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_misc.md)
  family
- **PWHL Data** – Schedules, standings, play-by-play, player box scores,
  team rosters, stat leaders, player profiles, game logs, career stats,
  player search, league leaders, transactions, streaks, playoff
  brackets, game summaries, and scorebar via the HockeyTech API
- **xG Models** – Expected goals predictions for NHL play-by-play data
  using XGBoost models (5v5, special teams, penalty shots)
- **Helper Aggregators** – Convenience functions inspired by
  `nhl-api-py` that combine multiple endpoint calls into a single tidy
  frame:
  [`nhl_game_ids_by_season()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_ids_by_season.md),
  [`nhl_all_players_by_season()`](https://fastRhockey.sportsdataverse.org/reference/nhl_all_players_by_season.md),
  [`nhl_player_career_stats()`](https://fastRhockey.sportsdataverse.org/reference/nhl_player_career_stats.md),
  [`nhl_team_summary_range()`](https://fastRhockey.sportsdataverse.org/reference/nhl_team_summary_range.md),
  [`nhl_skater_summary_range()`](https://fastRhockey.sportsdataverse.org/reference/nhl_skater_summary_range.md),
  [`nhl_goalie_summary_range()`](https://fastRhockey.sportsdataverse.org/reference/nhl_goalie_summary_range.md)
- **Full Season Loaders** – Nineteen NHL loaders covering play-by-play
  (full + lite), schedules, season + per-game rosters,
  player/skater/goalie/team boxscores, game info, scoring & penalty
  summaries, three stars, scratches, linescores, shifts, officials,
  shots-by-period, and shootouts
  ([`load_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp.md),
  [`load_nhl_pbp_lite()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp_lite.md),
  [`load_nhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_schedule.md),
  [`load_nhl_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_rosters.md),
  [`load_nhl_game_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_game_rosters.md),
  [`load_nhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_team_box.md),
  [`load_nhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_player_box.md),
  [`load_nhl_skater_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_skater_box.md),
  [`load_nhl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_goalie_box.md),
  [`load_nhl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_game_info.md),
  [`load_nhl_scoring()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_scoring.md),
  [`load_nhl_penalties()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_penalties.md),
  [`load_nhl_three_stars()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_three_stars.md),
  [`load_nhl_scratches()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_scratches.md),
  [`load_nhl_linescore()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_linescore.md),
  [`load_nhl_shifts()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_shifts.md),
  [`load_nhl_officials()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_officials.md),
  [`load_nhl_shots_by_period()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_shots_by_period.md),
  [`load_nhl_shootout()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_shootout.md));
  for PWHL, fifteen loaders covering play-by-play, schedules, season +
  per-game rosters, player/skater/goalie/team boxscores, game info,
  scoring & penalty summaries, three stars, officials, shots-by-period,
  and shootouts
  ([`load_pwhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_pbp.md),
  [`load_pwhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_schedule.md),
  [`load_pwhl_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_rosters.md),
  [`load_pwhl_game_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_game_rosters.md),
  [`load_pwhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_player_box.md),
  [`load_pwhl_skater_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_skater_box.md),
  [`load_pwhl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_goalie_box.md),
  [`load_pwhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_team_box.md),
  [`load_pwhl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_game_info.md),
  [`load_pwhl_scoring_summary()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_scoring_summary.md),
  [`load_pwhl_penalty_summary()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_penalty_summary.md),
  [`load_pwhl_three_stars()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_three_stars.md),
  [`load_pwhl_officials()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_officials.md),
  [`load_pwhl_shots_by_period()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_shots_by_period.md),
  [`load_pwhl_shootout()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_shootout.md))

> **Note:** PHF (Premier Hockey Federation) functions are deprecated as
> of v1.0.0. The league ceased operations; use PWHL functions instead.

![](https://raw.githubusercontent.com/sportsdataverse/fastRhockey/main/man/figures/fastRhockey_full_holographic_graphic.png)

------------------------------------------------------------------------

## Installation

You can install the CRAN version of
[**`fastRhockey`**](https://CRAN.R-project.org/package=fastRhockey)
with:

``` r
install.packages("fastRhockey")
```

You can install the development version from
[GitHub](https://github.com/sportsdataverse/fastRhockey) with:

``` r
# You can install using the pak package using the following code:
if (!requireNamespace('pak', quietly = TRUE)){
  install.packages('pak')
}
pak::pak("sportsdataverse/fastRhockey")
```

If you would prefer the `remotes` installation:

``` r
if (!requireNamespace('remotes', quietly = TRUE)){
  install.packages('remotes')
}
remotes::install_github(repo = "sportsdataverse/fastRhockey")
```

------------------------------------------------------------------------

## Quick Start

``` r
library(fastRhockey)

# NHL
nhl_schedule(season = "20242025")
nhl_standings(season = "20242025")
nhl_game_feed(game_id = 2024020001)

# NHL Edge advanced metrics
nhl_edge_skater_detail(player_id = 8478402)               # Connor McDavid, current season
nhl_edge_goalie_save_percentage_detail(player_id = 8475883)
nhl_edge_team_zone_time_details(team_id = 10)             # Toronto Maple Leafs

# NHL Records API
nhl_records_franchise()
nhl_records_draft_lottery_odds()
nhl_records_skater_real_time_stats_career(limit = 50)

# Helper aggregators
nhl_game_ids_by_season(season = 2025, team_abbr = "TOR")
nhl_player_career_stats(player_id = 8478402)
nhl_team_summary_range(start_season = 2023, end_season = 2024)

# PWHL
pwhl_schedule(season = 2025)
pwhl_standings(season = 2025)
pwhl_pbp(game_id = 27)
pwhl_player_info(player_id = 28)
pwhl_leaders(position = "skaters", season = 2025)
pwhl_game_summary(game_id = 27)

# Full season loaders
load_nhl_pbp(seasons = 2024)
load_nhl_schedule(seasons = 2024)
load_pwhl_pbp(seasons = 2024)
load_pwhl_schedule(seasons = 2024)
load_pwhl_team_box(seasons = 2024)
load_pwhl_skater_box(seasons = 2024)
load_pwhl_scoring_summary(seasons = 2024)
load_pwhl_penalty_summary(seasons = 2024)
load_pwhl_game_info(seasons = 2024)
```

------------------------------------------------------------------------

## Documentation

You can find the
[documentation](https://fastRhockey.sportsdataverse.org/) for
[**`fastRhockey`**](https://github.com/sportsdataverse/fastRhockey) on
[GitHub pages](https://fastRhockey.sportsdataverse.org/).

You can view CSVs of historical boxscore and play-by-play on the
[**`fastRhockey`**](https://github.com/sportsdataverse/fastRhockey/)
[data repo](https://github.com/sportsdataverse/fastRhockey-data), as
well as the process for scraping that historical data.

------------------------------------------------------------------------

## Changelog

[**Full News on
Releases**](https://fastRhockey.sportsdataverse.org/news/index.html)

------------------------------------------------------------------------

## Follow the [SportsDataverse](https://twitter.com/sportsdataverse) on Twitter and star this repo

[![Twitter
Follow](https://img.shields.io/twitter/follow/sportsdataverse?color=blue&label=%40sportsdataverse&logo=twitter&style=for-the-badge)](https://twitter.com/sportsdataverse)

[![GitHub
stars](https://img.shields.io/github/stars/sportsdataverse/fastRhockey.svg?color=eee&logo=github&style=for-the-badge&label=Star%20fastRhockey&maxAge=2592000)](https://github.com/sportsdataverse/fastRhockey/stargazers/)

## **Our Authors**

- [Ben Howell](https://twitter.com/BenHowell71)
  [![@BenHowell71](https://img.shields.io/twitter/follow/BenHowell71?color=blue&label=%40BenHowell71&logo=twitter&style=for-the-badge)](https://twitter.com/BenHowell71)
  [![@BenHowell71](https://img.shields.io/github/followers/BenHowell71?color=eee&logo=Github&style=for-the-badge)](https://github.com/BenHowell71)

- [Saiem Gilani](https://twitter.com/saiemgilani)
  [![@saiemgilani](https://img.shields.io/twitter/follow/saiemgilani?color=blue&label=%40saiemgilani&logo=twitter&style=for-the-badge)](https://twitter.com/saiemgilani)
  [![@saiemgilani](https://img.shields.io/github/followers/saiemgilani?color=eee&logo=Github&style=for-the-badge)](https://github.com/saiemgilani)

## **Our Contributors (they’re awesome)**

- [Alyssa Longmuir](https://twitter.com/alyssastweeting)
  [![@alyssastweeting](https://img.shields.io/twitter/follow/alyssastweeting?color=blue&label=%40alyssastweeting&logo=twitter&style=for-the-badge)](https://twitter.com/alyssastweeting)
  [![@Aklongmuir](https://img.shields.io/github/followers/Aklongmuir?color=eee&logo=Github&style=for-the-badge)](https://github.com/Aklongmuir)
- [Tan Ho](https://twitter.com/_TanHo) [](https://twitter.com/_TanHo)
  [![@tanho63](https://img.shields.io/github/followers/tanho63?color=eee&logo=Github&style=for-the-badge)](https://github.com/tanho63)

## **Citations**

To cite the
[**`fastRhockey`**](https://fastRhockey.sportsdataverse.org/) R package
in publications, use:

BibTex Citation

``` bibtex
@misc{howell_gilani_fastRhockey,
  author = {Ben Howell and Saiem Gilani},
  title = {fastRhockey: Functions to Access Professional Women's Hockey League and National Hockey League Play by Play Data.},
  url = {https://fastRhockey.sportsdataverse.org/},
  doi = {10.32614/CRAN.package.fastRhockey},
  year = {2025}
}
```
