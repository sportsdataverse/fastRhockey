# Getting Started with fastRhockey

[`fastRhockey`](https://github.com/sportsdataverse/fastRhockey) is an R
package for accessing hockey data from the **PWHL** (Professional
Women’s Hockey League) and **NHL** (National Hockey League) via public
web APIs. It provides structured data frames of play-by-play, schedule,
standings, roster, draft, player stats, and team data. The package also
includes integrated **expected goals (xG)** models for NHL data.

Part of the [SportsDataverse](https://sportsdataverse.org/) family of
R/Python packages for sports analytics.

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

## PWHL Data

The PWHL (Professional Women’s Hockey League) launched in January 2024
with six teams: Boston, Minnesota, Montreal, New York, Ottawa, and
Toronto. `fastRhockey` provides full access to PWHL schedules,
standings, play-by-play, player box scores, team rosters, and stat
leaders.

### Teams

``` r
library(dplyr)

pwhl_teams()
```

### Schedule

Let’s pull the 2024 PWHL season schedule:

``` r
schedule <- pwhl_schedule(season = 2024)

schedule %>%
  dplyr::filter(game_status == "Final") %>%
  dplyr::select(game_id, game_date, home_team, away_team,
                home_score, away_score, winner, venue) %>%
  head(10)
```

The schedule returns game IDs, dates, teams, scores, and venue
information for every game in the season.

### Standings

``` r
pwhl_standings(season = 2024) %>%
  dplyr::select(team_rank, team, games_played, points,
                wins, losses, goals_for, goals_against)
```

### Player Box Scores

Pick a game from the schedule and pull individual player stats.
[`pwhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_box.md)
returns a list with two data frames: skaters and goalies.

``` r
box <- pwhl_player_box(game_id = 27)

# Skater stats
box[[1]] %>%
  dplyr::select(first_name, last_name, position, team_id,
                goals, assists, points, shots, toi) %>%
  dplyr::arrange(dplyr::desc(points)) %>%
  head(10)

# Goalie stats
box[[2]] %>%
  dplyr::select(first_name, last_name, team_id,
                saves, goals_against, shots_against, toi)
```

### Play-by-Play

The real power of `fastRhockey` is in the play-by-play data.
[`pwhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_pbp.md)
returns detailed event-level data including shot locations, faceoff
results, penalties, goals with assist information, and plus/minus player
tracking.

``` r
pbp <- pwhl_pbp(game_id = 27)

pbp %>%
  dplyr::select(event, team_id, period_of_game, time_of_period,
                player_name_first, player_name_last,
                x_coord, y_coord) %>%
  head(15)
```

The play-by-play data includes 90+ columns with event details, player
information, coordinates (raw and transformed), and game context.

### Stat Leaders

Pull league-wide goalie or skater stats:

``` r
# Goalie stats
pwhl_stats(position = "goalie", season = 2024) %>%
  dplyr::select(player_name, team, games_played,
                goals_against_avg, save_percentage, shutouts) %>%
  head(10)
```

``` r
# Skater stats (all teams)
pwhl_stats(position = "skater", season = 2024) %>%
  dplyr::select(player_name, team, games_played,
                goals, assists, points) %>%
  head(10)
```

------------------------------------------------------------------------

## NHL Data

`fastRhockey` also provides extensive access to NHL data via the
`api-web.nhle.com` and `api.nhle.com/stats` APIs.

### Schedule

``` r
nhl_schedule(season = "20242025") %>%
  head(10)
```

### Standings

``` r
nhl_standings(season = "20242025") %>%
  head(10)
```

### Game Feed and Play-by-Play

The
[`nhl_game_feed()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_feed.md)
function returns detailed game data including play-by-play, rosters, and
game information.
[`nhl_game_pbp()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_pbp.md)
provides play-by-play with shift data integrated.

``` r
feed <- nhl_game_feed(game_id = 2024020001)

dplyr::glimpse(feed)
```

### Player Stats

``` r
# Skater season stats
nhl_stats_skaters(season = "20242025") %>%
  head(10)
```

### Draft Data

``` r
# Get 2024 draft picks (round 1)
nhl_draft_year(year = 2024, round = 1)
```

### Full Season Loaders

For bulk historical data, use the loader functions that pull from the
[fastRhockey-data](https://github.com/sportsdataverse/fastRhockey-data)
repository:

``` r
# Full season of play-by-play
pbp <- load_nhl_pbp(seasons = 2024)

# Full season schedule
sched <- load_nhl_schedule(seasons = 2024)

# Full season rosters
rosters <- load_nhl_rosters(seasons = 2024)
```

------------------------------------------------------------------------

## Deprecated PHF Functions

The Premier Hockey Federation (PHF), formerly the National Women’s
Hockey League (NWHL), ceased operations prior to the formation of the
PWHL. All `phf_*` functions are deprecated as of v1.0.0 and will raise
errors when called. Use the `pwhl_*` functions instead for women’s
professional hockey data.

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
