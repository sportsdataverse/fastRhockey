
# 

# fastRhockey <a href='https://fastRhockey.sportsdataverse.org/'><img src='https://raw.githubusercontent.com/sportsdataverse/fastRhockey/main/logo.png' align="right" width="20%" min-width="100" /></a>

<!-- badges: start -->

[![CRAN
version](https://img.shields.io/badge/dynamic/json?style=for-the-badge&color=success&label=CRAN%20version&prefix=v&query=%24.Version&url=https%3A%2F%2Fcrandb.r-pkg.org%2FfastRhockey)](https://CRAN.R-project.org/package=fastRhockey)
[![CRAN
downloads](https://img.shields.io/badge/dynamic/json?style=for-the-badge&color=success&label=Downloads&query=%24%5B0%5D.downloads&url=https%3A%2F%2Fcranlogs.r-pkg.org%2Fdownloads%2Ftotal%2F2021-10-26%3Alast-day%2FfastRhockey)](https://CRAN.R-project.org/package=fastRhockey)
[![Version-Number](https://img.shields.io/github/r-package/v/sportsdataverse/fastRhockey?label=fastRhockey&logo=R&style=for-the-badge)](https://github.com/sportsdataverse/fastRhockey/)
[![R-CMD-check](https://img.shields.io/github/actions/workflow/status/sportsdataverse/fastRhockey/R-CMD-check.yaml?branch=main&label=R-CMD-Check&logo=R&logoColor=white&style=for-the-badge)](https://github.com/sportsdataverse/fastRhockey/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg?style=for-the-badge&logo=github)](https://github.com/sportsdataverse/fastRhockey/)
[![Contributors](https://img.shields.io/github/contributors/sportsdataverse/fastRhockey?style=for-the-badge)](https://github.com/sportsdataverse/fastRhockey/graphs/contributors)
<!-- badges: end -->

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
    `records.nhl.com/site/api/` for franchise totals,
    player/skater/goalie career and real-time stats, draft lottery, hall
    of fame, trophies, awards, attendance, venues, officials, and
    combine data
  - **NHL Stats REST** (19 dedicated wrappers) –
    `nhl_stats_franchise()`, `nhl_stats_players()`,
    `nhl_stats_glossary()`, `nhl_stats_country()`,
    `nhl_stats_skater_leaders()`, `nhl_stats_goalie_leaders()`,
    `nhl_stats_skater_milestones()`, `nhl_stats_goalie_milestones()`,
    plus the original `nhl_stats_skaters()` / `nhl_stats_goalies()` /
    `nhl_stats_teams()` / `nhl_stats_draft()` / `nhl_stats_seasons()` /
    `nhl_stats_misc()` family
  - **PWHL Data** – Schedules, standings, play-by-play, player box
    scores, team rosters, stat leaders, player profiles, game logs,
    career stats, player search, league leaders, transactions, streaks,
    playoff brackets, game summaries, and scorebar via the HockeyTech
    API
  - **xG Models** – Expected goals predictions for NHL play-by-play data
    using XGBoost models (5v5, special teams, penalty shots)
  - **Helper Aggregators** – Convenience functions inspired by
    `nhl-api-py` that combine multiple endpoint calls into a single tidy
    frame: `nhl_game_ids_by_season()`, `nhl_all_players_by_season()`,
    `nhl_player_career_stats()`, `nhl_team_summary_range()`,
    `nhl_skater_summary_range()`, `nhl_goalie_summary_range()`
  - **Full Season Loaders** – Sixteen NHL loaders covering play-by-play
    (full + lite), schedules, season + per-game rosters,
    player/skater/goalie/team boxscores, game info, scoring & penalty
    summaries, three stars, scratches, linescores, and shifts
    (`load_nhl_pbp()`, `load_nhl_pbp_lite()`, `load_nhl_schedule()`,
    `load_nhl_rosters()`, `load_nhl_game_rosters()`,
    `load_nhl_team_box()`, `load_nhl_player_box()`,
    `load_nhl_skater_box()`, `load_nhl_goalie_box()`,
    `load_nhl_game_info()`, `load_nhl_scoring()`,
    `load_nhl_penalties()`, `load_nhl_three_stars()`,
    `load_nhl_scratches()`, `load_nhl_linescore()`,
    `load_nhl_shifts()`); for PWHL, fifteen loaders covering
    play-by-play, schedules, season + per-game rosters,
    player/skater/goalie/team boxscores, game info, scoring & penalty
    summaries, three stars, officials, shots-by-period, and shootouts
    (`load_pwhl_pbp()`, `load_pwhl_schedule()`, `load_pwhl_rosters()`,
    `load_pwhl_game_rosters()`, `load_pwhl_player_box()`,
    `load_pwhl_skater_box()`, `load_pwhl_goalie_box()`,
    `load_pwhl_team_box()`, `load_pwhl_game_info()`,
    `load_pwhl_scoring_summary()`, `load_pwhl_penalty_summary()`,
    `load_pwhl_three_stars()`, `load_pwhl_officials()`,
    `load_pwhl_shots_by_period()`, `load_pwhl_shootout()`)

> **Note:** PHF (Premier Hockey Federation) functions are deprecated as
> of v1.0.0. The league ceased operations; use PWHL functions instead.

<center>

<img src='https://raw.githubusercontent.com/sportsdataverse/fastRhockey/main/man/figures/fastRhockey_full_holographic_graphic.png' width="70%" />

</center>

-----

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

-----

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

-----

## Documentation

You can find the
[documentation](https://fastRhockey.sportsdataverse.org/) for
[**`fastRhockey`**](https://github.com/sportsdataverse/fastRhockey) on
[GitHub pages](https://fastRhockey.sportsdataverse.org/).

You can view CSVs of historical boxscore and play-by-play on the
[**`fastRhockey`**](https://github.com/sportsdataverse/fastRhockey/)
[data repo](https://github.com/sportsdataverse/fastRhockey-data), as
well as the process for scraping that historical data.

-----

## Changelog

[**Full News on
Releases**](https://fastRhockey.sportsdataverse.org/news/index.html)

-----

## Follow the [SportsDataverse](https://twitter.com/sportsdataverse) on Twitter and star this repo

[![Twitter
Follow](https://img.shields.io/twitter/follow/sportsdataverse?color=blue&label=%40sportsdataverse&logo=twitter&style=for-the-badge)](https://twitter.com/sportsdataverse)

[![GitHub
stars](https://img.shields.io/github/stars/sportsdataverse/fastRhockey.svg?color=eee&logo=github&style=for-the-badge&label=Star%20fastRhockey&maxAge=2592000)](https://github.com/sportsdataverse/fastRhockey/stargazers/)

## **Our Authors**

  - [Ben Howell](https://twitter.com/BenHowell71)
    <a href="https://twitter.com/BenHowell71" target="blank"><img src="https://img.shields.io/twitter/follow/BenHowell71?color=blue&label=%40BenHowell71&logo=twitter&style=for-the-badge" alt="@BenHowell71" /></a>
    <a href="https://github.com/BenHowell71" target="blank"><img src="https://img.shields.io/github/followers/BenHowell71?color=eee&logo=Github&style=for-the-badge" alt="@BenHowell71" /></a>

  - [Saiem Gilani](https://twitter.com/saiemgilani)
    <a href="https://twitter.com/saiemgilani" target="blank"><img src="https://img.shields.io/twitter/follow/saiemgilani?color=blue&label=%40saiemgilani&logo=twitter&style=for-the-badge" alt="@saiemgilani" /></a>
    <a href="https://github.com/saiemgilani" target="blank"><img src="https://img.shields.io/github/followers/saiemgilani?color=eee&logo=Github&style=for-the-badge" alt="@saiemgilani" /></a>

## **Our Contributors (they’re awesome)**

  - [Alyssa Longmuir](https://twitter.com/alyssastweeting)
    <a href="https://twitter.com/alyssastweeting" target="blank"><img src="https://img.shields.io/twitter/follow/alyssastweeting?color=blue&label=%40alyssastweeting&logo=twitter&style=for-the-badge" alt="@alyssastweeting" /></a>
    <a href="https://github.com/Aklongmuir" target="blank"><img src="https://img.shields.io/github/followers/Aklongmuir?color=eee&logo=Github&style=for-the-badge" alt="@Aklongmuir" /></a>
  - [Tan Ho](https://twitter.com/_TanHo)
    <a href="https://twitter.com/_TanHo" target="blank"></a>
    <a href="https://github.com/tanho63" target="blank"><img src="https://img.shields.io/github/followers/tanho63?color=eee&logo=Github&style=for-the-badge" alt="@tanho63" /></a>

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
