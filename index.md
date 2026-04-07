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
- **PWHL Data** – Schedules, standings, play-by-play, player box scores,
  team rosters, and stat leaders via the HockeyTech API
- **xG Models** – Expected goals predictions for NHL play-by-play data
  using XGBoost models (5v5, special teams, penalty shots)
- **Full Season Loaders** –
  [`load_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp.md),
  [`load_nhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_schedule.md),
  [`load_nhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_team_box.md),
  [`load_nhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_player_box.md),
  [`load_nhl_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_rosters.md)
  for bulk historical data

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
if (!requireNamespace('pak', quietly = TRUE)){
  install.packages('pak')
}
pak::pak("sportsdataverse/fastRhockey")
```

------------------------------------------------------------------------

## Quick Start

``` r
library(fastRhockey)

# NHL
nhl_schedule(season = "20242025")
nhl_standings(season = "20242025")
nhl_game_feed(game_id = 2024020001)

# PWHL
pwhl_schedule(season = 2024)
pwhl_standings(season = 2024)
pwhl_pbp(game_id = 27)

# Full season loaders
load_nhl_pbp(seasons = 2024)
load_nhl_schedule(seasons = 2024)
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
