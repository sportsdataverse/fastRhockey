
# **fastRhockey** <a href='https://fastRhockey.sportsdataverse.org/'><img src='https://raw.githubusercontent.com/sportsdataverse/fastRhockey/main/logo.png' align="right" width="20%" min-width="100" alt='fastRhockey hex logo' /></a>

<!-- badges: start -->
[![CRAN version](https://img.shields.io/badge/dynamic/json?style=for-the-badge&color=success&label=CRAN%20version&prefix=v&query=%24.Version&url=https%3A%2F%2Fcrandb.r-pkg.org%2FfastRhockey)](https://CRAN.R-project.org/package=fastRhockey)
[![CRAN downloads](https://img.shields.io/badge/dynamic/json?style=for-the-badge&color=success&label=Downloads&query=%24%5B0%5D.downloads&url=https%3A%2F%2Fcranlogs.r-pkg.org%2Fdownloads%2Ftotal%2F2021-10-26%3Alast-day%2FfastRhockey)](https://CRAN.R-project.org/package=fastRhockey)
[![R-universe](https://img.shields.io/badge/dynamic/json?style=for-the-badge&color=success&label=r-universe&prefix=v&query=%24.Version&url=https%3A%2F%2Fsportsdataverse.r-universe.dev%2Fapi%2Fpackages%2FfastRhockey)](https://sportsdataverse.r-universe.dev/fastRhockey)
[![Version-Number](https://img.shields.io/github/r-package/v/sportsdataverse/fastRhockey?label=fastRhockey&logo=R&style=for-the-badge)](https://github.com/sportsdataverse/fastRhockey/)
[![R-CMD-check](https://img.shields.io/github/actions/workflow/status/sportsdataverse/fastRhockey/R-CMD-check.yaml?branch=main&label=R-CMD-Check&logo=R&logoColor=white&style=for-the-badge)](https://github.com/sportsdataverse/fastRhockey/actions/workflows/R-CMD-check.yaml)
[![Codecov](https://img.shields.io/codecov/c/github/sportsdataverse/fastRhockey?logo=codecov&style=for-the-badge)](https://app.codecov.io/gh/sportsdataverse/fastRhockey)
[![pkgdown](https://img.shields.io/github/actions/workflow/status/sportsdataverse/fastRhockey/pkgdown.yaml?branch=main&label=pkgdown&logo=github&style=for-the-badge)](https://github.com/sportsdataverse/fastRhockey/actions/workflows/pkgdown.yaml)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg?style=for-the-badge&logo=github)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg?style=for-the-badge)](https://github.com/sportsdataverse/fastRhockey/blob/main/LICENSE.md)
[![Contributors](https://img.shields.io/github/contributors/sportsdataverse/fastRhockey?style=for-the-badge)](https://github.com/sportsdataverse/fastRhockey/graphs/contributors)
[![GitHub stars](https://img.shields.io/github/stars/sportsdataverse/fastRhockey?style=for-the-badge&logo=github)](https://github.com/sportsdataverse/fastRhockey)
[![X Follow](https://img.shields.io/twitter/follow/SportsDataverse?color=blue&label=%40SportsDataverse&logo=x&style=for-the-badge)](https://x.com/SportsDataverse)
<!-- badges: end -->

[`fastRhockey`](https://github.com/sportsdataverse/fastRhockey) is an R
package for accessing hockey data via public web APIs: the **NHL**
(National Hockey League), the **PWHL** (Professional Women’s Hockey
League), the AHL and the three Canadian major-junior leagues (OHL, WHL,
QMJHL), thirteen European leagues, and NCAA men’s and women’s college
hockey through ESPN. It exports 880 functions that return structured
data frames of play-by-play, schedule, standings, roster, draft, player
stats, and team data. The package also includes integrated **expected
goals (xG)** models trained via XGBoost.

Part of the [SportsDataverse](https://sportsdataverse.org/) family of
R/Python packages for sports analytics.

### Key Features

  - **NHL Data** – Game feeds, boxscores, play-by-play with shifts,
    schedules, standings, rosters, draft picks, player/team stats,
    playoffs, and broadcast information via `api-web.nhle.com` and
    `api.nhle.com/stats`
  - **NHL Edge Analytics** (35 functions, including the two `/cat/edge`
    detail wrappers `nhl_cat_edge_skater_detail()` and
    `nhl_cat_edge_goalie_detail()`) – Skater, goalie, and team advanced
    metrics from `api-web.nhle.com/v1/edge/...` covering shot location,
    shot speed, skating speed, skating distance, zone time, comparisons,
    landing pages, and top-10 leaderboards. Each wrapper accepts an
    optional `season` argument and falls back to the current season via
    the `/now` form
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
  - **[ESPN
    NHL](https://fastRhockey.sportsdataverse.org/reference/index.html#espn-nhl)**
    (126 functions, `espn_nhl_*`) – Play-by-play, team and player box
    scores, scoreboards, schedules, standings, teams, rosters, athletes,
    odds, win probability, injuries, transactions and the draft from
    ESPN’s site, web and core APIs
  - **[Fox Sports
    NHL](https://fastRhockey.sportsdataverse.org/reference/index.html#fox-sports-nhl-data)**
    (8 functions, `fox_nhl_*`) – Play-by-play, box scores, odds, team
    rosters, team stats, game logs, standings and league leaders from
    Fox Sports’ Bifrost API
  - **[AHL](https://fastRhockey.sportsdataverse.org/reference/index.html#ahl-data),
    [OHL](https://fastRhockey.sportsdataverse.org/reference/index.html#ohl-data),
    [WHL](https://fastRhockey.sportsdataverse.org/reference/index.html#whl-data)
    and
    [QMJHL](https://fastRhockey.sportsdataverse.org/reference/index.html#qmjhl-data)**
    (48 functions, `ahl_*`, `ohl_*`, `whl_*`, `qmjhl_*`) – Schedules,
    play-by-play, standings, teams, rosters, player stats, leaders, game
    summaries and on-ice shift / TOI / Corsi analytics through the same
    HockeyTech API as the PWHL
  - **[European
    leagues](https://fastRhockey.sportsdataverse.org/reference/index.html#european-hockey)**
    (211 functions) – Thirteen leagues: DEL (`del_*`), ICE Hockey League
    (`icehl_*`), EIHL (`eihl_*`), Metal Ligaen (`metal_*`), KHL
    (`khl_*`), VHL (`vhl_*`), SHL (`shl_*`), HockeyAllsvenskan (`ha_*`),
    Liiga (`liiga_*`), the Swiss federation (`sihf_*`) and National
    League (`nl_*`), the Czech Extraliga (`elh_*`) and Ligue Magnus
    (`magnus_*`). Seasons are end years (2027 = 2026-27), requests to
    one host are spaced at least a second apart, and failures raise
    classed conditions (`fastRhockey_no_data`,
    `fastRhockey_asset_fetch`, `fastRhockey_bad_request`)
  - **[NCAA college
    hockey](https://fastRhockey.sportsdataverse.org/reference/index.html#espn-college-hockey)**
    (242 functions) – ESPN men’s (`espn_mch_*`) and women’s
    (`espn_wch_*`) college hockey: play-by-play, box scores,
    scoreboards, schedules, standings, teams, rosters, rankings and
    leaders
  - **xG Models** – Expected goals predictions for NHL play-by-play data
    using XGBoost models (5v5, special teams, penalty shots)
  - **Helper Aggregators** – Convenience functions inspired by
    `nhl-api-py` that combine multiple endpoint calls into a single tidy
    frame: `nhl_game_ids_by_season()`, `nhl_all_players_by_season()`,
    `nhl_player_career_stats()`, `nhl_team_summary_range()`,
    `nhl_skater_summary_range()`, `nhl_goalie_summary_range()`
  - **[Full Season
    Loaders](https://fastRhockey.sportsdataverse.org/reference/index.html#nhl-data-loaders)**
    – Twenty-three NHL loaders covering play-by-play (full + lite),
    schedules, season + per-game rosters, player/skater/goalie/team
    boxscores, game info, scoring & penalty summaries, three stars,
    scratches, linescores, shifts, officials, shots-by-period,
    shootouts, and conference / division reference tables
    (`load_nhl_pbp()`, `load_nhl_pbp_lite()`, `load_nhl_schedule()`,
    `load_nhl_rosters()`, `load_nhl_game_rosters()`,
    `load_nhl_team_box()`, `load_nhl_player_box()`,
    `load_nhl_skater_box()`, `load_nhl_goalie_box()`,
    `load_nhl_game_info()`, `load_nhl_scoring()`,
    `load_nhl_penalties()`, `load_nhl_three_stars()`,
    `load_nhl_scratches()`, `load_nhl_linescore()`, `load_nhl_shifts()`,
    `load_nhl_officials()`, `load_nhl_shots_by_period()`,
    `load_nhl_shootout()`, `load_nhl_groups()`,
    `load_nhl_group_seasons()`, `load_nhl_group_aliases()`,
    `load_nhl_team_group_seasons()`); for the
    [PWHL](https://fastRhockey.sportsdataverse.org/reference/index.html#pwhl-data-loaders),
    seventeen loaders covering play-by-play (plain and xG-enriched),
    schedules, season + per-game rosters, player/skater/goalie/team
    boxscores, game info, scoring & penalty summaries, three stars,
    officials, shots-by-period, shootouts, and shifts
    (`load_pwhl_pbp()`, `load_pwhl_xg_pbp()`, `load_pwhl_schedule()`,
    `load_pwhl_rosters()`, `load_pwhl_game_rosters()`,
    `load_pwhl_player_box()`, `load_pwhl_skater_box()`,
    `load_pwhl_goalie_box()`, `load_pwhl_team_box()`,
    `load_pwhl_game_info()`, `load_pwhl_scoring_summary()`,
    `load_pwhl_penalty_summary()`, `load_pwhl_three_stars()`,
    `load_pwhl_officials()`, `load_pwhl_shots_by_period()`,
    `load_pwhl_shootout()`, `load_pwhl_shifts()`). Both families also
    export aliases named to match
    [sportsdataverse-py](https://py.sportsdataverse.org/)
    (`load_nhl_schedules()`, `load_pwhl_team_boxscores()`, …).

> **Note:** PHF (Premier Hockey Federation) functions have been defunct
> since v1.0.0: they stop with an error. The league ceased operations;
> use PWHL functions instead.

<center>

<img src='https://raw.githubusercontent.com/sportsdataverse/fastRhockey/main/man/figures/fastRhockey_full_holographic_graphic.png' width="70%" alt='fastRhockey holographic logo banner' />

</center>

-----

## **Data status**

The `load_nhl_*()` and `load_pwhl_*()` functions read release assets on
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data/releases).
The badges below are rebuilt nightly from each producer’s latest
workflow run and newest release files; idle means the sport is out of
season. Full detail:
[sportsdataverse.org/status](https://sportsdataverse.org/status).

| Dataset                                                                                              | Data updated                                                                                                                                                                                                                                                                       | Through                                                                                                                                                                                                                                                                              | Pipeline                                                                                                                                                                                                                                                                    | Update workflows                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| :--------------------------------------------------------------------------------------------------- | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [NHL](https://github.com/sportsdataverse/fastRhockey-nhl-data)                                       | [![fastRhockey-nhl-data data updated](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fsportsdataverse%2F.github%2Fmain%2Fstatus%2Fbadges%2FfastRhockey-nhl-data%2Fupdated.json)](https://github.com/sportsdataverse/sportsdataverse-data/releases)   | [![fastRhockey-nhl-data through season](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fsportsdataverse%2F.github%2Fmain%2Fstatus%2Fbadges%2FfastRhockey-nhl-data%2Fthrough.json)](https://github.com/sportsdataverse/sportsdataverse-data/releases)   | [![fastRhockey-nhl-data pipeline state](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fsportsdataverse%2F.github%2Fmain%2Fstatus%2Fbadges%2FfastRhockey-nhl-data%2Fstatus.json)](https://sportsdataverse.org/status#fastRhockey-nhl-data)    | [![fastRhockey-nhl-data daily\_nhl\_python](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fsportsdataverse%2F.github%2Fmain%2Fstatus%2Fbadges%2FfastRhockey-nhl-data%2Fwf-daily_nhl_python.json)](https://github.com/sportsdataverse/fastRhockey-nhl-data/actions/workflows/daily_nhl_python.yml) [![fastRhockey-nhl-data nhl\_model\_pipeline](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fsportsdataverse%2F.github%2Fmain%2Fstatus%2Fbadges%2FfastRhockey-nhl-data%2Fwf-nhl_model_pipeline.json)](https://github.com/sportsdataverse/fastRhockey-nhl-data/actions/workflows/nhl_model_pipeline.yml) |
| [PWHL](https://github.com/sportsdataverse/fastRhockey-pwhl-data)                                     | [![fastRhockey-pwhl-data data updated](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fsportsdataverse%2F.github%2Fmain%2Fstatus%2Fbadges%2FfastRhockey-pwhl-data%2Fupdated.json)](https://github.com/sportsdataverse/sportsdataverse-data/releases) | [![fastRhockey-pwhl-data through season](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fsportsdataverse%2F.github%2Fmain%2Fstatus%2Fbadges%2FfastRhockey-pwhl-data%2Fthrough.json)](https://github.com/sportsdataverse/sportsdataverse-data/releases) | [![fastRhockey-pwhl-data pipeline state](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fsportsdataverse%2F.github%2Fmain%2Fstatus%2Fbadges%2FfastRhockey-pwhl-data%2Fstatus.json)](https://sportsdataverse.org/status#fastRhockey-pwhl-data) | [![fastRhockey-pwhl-data daily\_pwhl\_python](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fsportsdataverse%2F.github%2Fmain%2Fstatus%2Fbadges%2FfastRhockey-pwhl-data%2Fwf-daily_pwhl_python.json)](https://github.com/sportsdataverse/fastRhockey-pwhl-data/actions/workflows/daily_pwhl_python.yml) [![fastRhockey-pwhl-data pwhl\_xg\_cron](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fsportsdataverse%2F.github%2Fmain%2Fstatus%2Fbadges%2FfastRhockey-pwhl-data%2Fwf-pwhl_xg_cron.json)](https://github.com/sportsdataverse/fastRhockey-pwhl-data/actions/workflows/pwhl_xg_cron.yml)          |
| [Conference, division and ballpark reference](https://github.com/sportsdataverse/sdv-reference-data) | [![sdv-reference-data data updated](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fsportsdataverse%2F.github%2Fmain%2Fstatus%2Fbadges%2Fsdv-reference-data%2Fupdated.json)](https://github.com/sportsdataverse/sportsdataverse-data/releases)       | [![sdv-reference-data through season](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fsportsdataverse%2F.github%2Fmain%2Fstatus%2Fbadges%2Fsdv-reference-data%2Fthrough.json)](https://github.com/sportsdataverse/sportsdataverse-data/releases)       | [![sdv-reference-data pipeline state](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fsportsdataverse%2F.github%2Fmain%2Fstatus%2Fbadges%2Fsdv-reference-data%2Fstatus.json)](https://sportsdataverse.org/status#sdv-reference-data)          | —                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |

## **Installation**

You can install the CRAN version of [**`fastRhockey`**](https://CRAN.R-project.org/package=fastRhockey) with:

```r
install.packages("fastRhockey")
```

You can install the latest build from [R-universe](https://sportsdataverse.r-universe.dev/fastRhockey) with:

```r
install.packages("fastRhockey", repos = c("https://sportsdataverse.r-universe.dev", getOption("repos")))
```

You can install the development version of [**`fastRhockey`**](https://github.com/sportsdataverse/fastRhockey/) from GitHub with:

```r
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
pak::pak("sportsdataverse/fastRhockey")
# or, with remotes:
# if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
# remotes::install_github("sportsdataverse/fastRhockey")
```

-----

## **Quick start**

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

# ESPN and Fox Sports NHL
espn_nhl_scoreboard(dates = 20250301)
fox_nhl_standings(team_id = "1")

# AHL, OHL, WHL and QMJHL (HockeyTech)
ahl_schedule(season = 2026)

# European leagues (seasons are end years: 2026 = 2025-26)
liiga_schedule(season = 2026)
khl_standings(season = 2026)

# NCAA men's and women's college hockey (ESPN)
espn_mch_scoreboard(dates = 20250301)
espn_wch_standings()

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

## **Documentation**

You can find the
[documentation](https://fastRhockey.sportsdataverse.org/) for
[**`fastRhockey`**](https://github.com/sportsdataverse/fastRhockey) on
[GitHub pages](https://fastRhockey.sportsdataverse.org/).

- [Function reference](https://fastRhockey.sportsdataverse.org/reference/index.html)
- Articles:
  - [Getting Started with fastRhockey: NHL, PWHL and Hockey Data in R](https://fastRhockey.sportsdataverse.org/articles/introduction.html)
  - [Corsi, Fenwick and PDO in R: Hockey Shot-Share Metrics with fastRhockey](https://fastRhockey.sportsdataverse.org/articles/hockey-advanced-metrics-corsi-fenwick.html)
  - [Hockey Shot Maps in R: NHL and PWHL Rink Plots with fastRhockey](https://fastRhockey.sportsdataverse.org/articles/nhl-pwhl-rink-plots-shot-maps.html)
  - [PWHL and College Hockey Data in R: Women's Pro and NCAA Hockey with fastRhockey](https://fastRhockey.sportsdataverse.org/articles/pwhl-and-college-hockey.html)
  - [European Hockey Data in R: SHL, Liiga, DEL, KHL, National League and More](https://fastRhockey.sportsdataverse.org/articles/european-hockey-leagues.html)
  - [NHL Expected Goals (xG) in fastRhockey: How the Model Works](https://fastRhockey.sportsdataverse.org/articles/nhl-expected-goals-xg-model.html)
  - [PWHL Expected Goals (xG): How the Coordinate Model Works](https://fastRhockey.sportsdataverse.org/articles/pwhl-expected-goals-xg-model.html)
- [**Full News on Releases**](https://fastRhockey.sportsdataverse.org/news/index.html)

There is also a printable [**`fastRhockey` cheat sheet
(PDF)**](https://sportsdataverse.org/cheatsheets/fastRhockey.pdf), one
of [a set covering every SportsDataverse
package](https://sportsdataverse.org/cheatsheets).

You can view CSVs of historical boxscore and play-by-play on the
[**`fastRhockey`**](https://github.com/sportsdataverse/fastRhockey/)
[data repo](https://github.com/sportsdataverse/fastRhockey-data), as
well as the process for scraping that historical data.

-----

## **Companion packages**

  - [**`sdvplotR`**](https://sdvplotR.sportsdataverse.org/)
    (`pak::pak("sportsdataverse/sdvplotR")`) adds NHL team logos and
    colours to `ggplot2` charts and draws shot maps from the
    `fastRhockey` loaders; see its [NHL
    visualization](https://sdvplotR.sportsdataverse.org/articles/nhl-viz.html)
    and [NHL shot
    map](https://sdvplotR.sportsdataverse.org/articles/recipe-nhl-shot-map.html)
    articles.
  - [**`sportyR`**](https://sportyR.sportsdataverse.org/)
    (`install.packages("sportyR")`) draws regulation rinks
    (`sportyR::geom_hockey("nhl")`) to plot the `x_coord_fixed` /
    `y_coord_fixed` play-by-play coordinates on.
  - [**`hockeyR`**](https://github.com/danmorse314/hockeyR) (Daniel
    Morse) is the other NHL play-by-play package for R. The
    `fastRhockey` NHL xG feature pipeline is adapted from hockeyR’s.
  - [**`sportsdataverse-py`**](https://py.sportsdataverse.org/) is the
    Python package; most `fastRhockey` functions have a Python twin of
    the same name.

-----

## **The SportsDataverse**

[**`fastRhockey`**](https://fastRhockey.sportsdataverse.org/) is part of the [SportsDataverse](https://sportsdataverse.org/), a set of
open-source packages for sports data.

| Package | Sport / Scope |
|---|---|
| [**cfbfastR**](https://cfbfastR.sportsdataverse.org/) | College football |
| [**hoopR**](https://hoopR.sportsdataverse.org/) | Men's basketball (NBA and NCAA) |
| [**wehoop**](https://wehoop.sportsdataverse.org/) | Women's basketball (WNBA and NCAA) |
| [**fastRhockey**](https://fastRhockey.sportsdataverse.org/) | Hockey (NHL and PWHL) |
| [**baseballr**](https://billpetti.github.io/baseballr/) | Baseball (MLB, MiLB, NCAA) |
| [**softballR**](https://github.com/sportsdataverse/softballR) | College softball |
| [**oddsapiR**](https://oddsapiR.sportsdataverse.org/) | Sports betting odds |
| [**recruitR**](https://recruitR.sportsdataverse.org/) | College football recruiting |
| [**usfootballR**](https://usfootballR.sportsdataverse.org/) | US soccer |
| [**cfbseedR**](https://cfbseedR.sportsdataverse.org/) · [**cfb4th**](https://cfb4th.sportsdataverse.org/) | CFB playoff simulation · fourth-down decisions |
| [**cfbplotR**](https://cfbplotR.sportsdataverse.org/) · [**sdvplotR**](https://sdvplotR.sportsdataverse.org/) · [**sportyR**](https://sportyR.sportsdataverse.org/) | Logos, colors, tables · playing surfaces |
| [**sportsdataverse-R**](https://r.sportsdataverse.org/) | Umbrella R metapackage |
| [**sportsdataverse-py**](https://py.sportsdataverse.org/) · [**sportsdataverse.js**](https://js.sportsdataverse.org/) | Python and Node.js |

See the full ecosystem at [sportsdataverse.org](https://sportsdataverse.org/).

## **Follow the [SportsDataverse](https://x.com/SportsDataverse) on X and star this repo**

[![X
Follow](https://img.shields.io/twitter/follow/SportsDataverse?color=blue&label=%40SportsDataverse&logo=x&style=for-the-badge)](https://x.com/SportsDataverse)

[![GitHub
stars](https://img.shields.io/github/stars/sportsdataverse/fastRhockey.svg?color=eee&logo=github&style=for-the-badge&label=Star%20fastRhockey&maxAge=2592000)](https://github.com/sportsdataverse/fastRhockey)

<a href="https://github.com/sportsdataverse/fastRhockey"> <picture>
<source media="(prefers-color-scheme: dark)" srcset="https://raw.githubusercontent.com/sportsdataverse/fastRhockey/star-history/star-history-dark.svg">
<img alt="Star history of sportsdataverse/fastRhockey" src="https://raw.githubusercontent.com/sportsdataverse/fastRhockey/star-history/star-history.png" width="600">
</picture> </a>

## **Our Authors**

  - [Ben Howell](https://x.com/BenHowell71)
    <a href="https://x.com/BenHowell71" target="blank"><img src="https://img.shields.io/twitter/follow/BenHowell71?color=blue&label=%40BenHowell71&logo=x&style=for-the-badge" alt="@BenHowell71" /></a>
    <a href="https://github.com/BenHowell71" target="blank"><img src="https://img.shields.io/github/followers/BenHowell71?color=eee&logo=Github&style=for-the-badge" alt="@BenHowell71" /></a>

  - [Saiem Gilani](https://x.com/saiemgilani)
    <a href="https://x.com/saiemgilani" target="blank"><img src="https://img.shields.io/twitter/follow/saiemgilani?color=blue&label=%40saiemgilani&logo=x&style=for-the-badge" alt="@saiemgilani" /></a>
    <a href="https://github.com/saiemgilani" target="blank"><img src="https://img.shields.io/github/followers/saiemgilani?color=eee&logo=Github&style=for-the-badge" alt="@saiemgilani" /></a>

## **Our Contributors (they’re awesome)**

  - [Alyssa Longmuir](https://x.com/alyssastweeting)
    <a href="https://x.com/alyssastweeting" target="blank"><img src="https://img.shields.io/twitter/follow/alyssastweeting?color=blue&label=%40alyssastweeting&logo=x&style=for-the-badge" alt="@alyssastweeting" /></a>
    <a href="https://github.com/Aklongmuir" target="blank"><img src="https://img.shields.io/github/followers/Aklongmuir?color=eee&logo=Github&style=for-the-badge" alt="@Aklongmuir" /></a>
  - [Tan Ho](https://x.com/_TanHo)
    <a href="https://x.com/_TanHo" target="blank"></a>
    <a href="https://github.com/tanho63" target="blank"><img src="https://img.shields.io/github/followers/tanho63?color=eee&logo=Github&style=for-the-badge" alt="@tanho63" /></a>

<!-- cheatsheet-section -->

## **Cheat sheet**

A printable one-page reference for **`fastRhockey`** — the function
families, the loaders, and what each one returns.

📄 **[Download the fastRhockey cheat sheet
(PDF)](https://sportsdataverse.org/cheatsheets/fastRhockey.pdf)**

Free to download, print and hand out; light and dark, US Letter
landscape. Every SportsDataverse package has one — browse them all at
**[sportsdataverse.org/cheatsheets](https://sportsdataverse.org/cheatsheets)**.

## **Code of Conduct**

Please note that the fastRhockey project is released with a [Contributor Code of Conduct](https://fastRhockey.sportsdataverse.org/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## **Citations**

To cite the
[**`fastRhockey`**](https://fastRhockey.sportsdataverse.org/) R package
in publications, use:

BibTeX Citation

``` bibtex
@Manual{howell_gilani_fastRhockey,
  author = {Ben Howell and Saiem Gilani},
  title = {fastRhockey: Functions to Access Hockey Play by Play Data and Statistics},
  note = {R package version 1.1.0},
  url = {https://fastRhockey.sportsdataverse.org/},
  doi = {10.32614/CRAN.package.fastRhockey},
  year = {2026}
}
```

## **License**

[**`fastRhockey`**](https://github.com/sportsdataverse/fastRhockey) is released under the [MIT License](https://github.com/sportsdataverse/fastRhockey/blob/main/LICENSE.md) (`MIT + file LICENSE`).
