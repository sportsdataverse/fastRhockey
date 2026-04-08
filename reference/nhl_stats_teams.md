# **NHL Stats API — Team Stats**

Queries the NHL Stats REST API for team-level statistics.

## Usage

``` r
nhl_stats_teams(
  report_type = "summary",
  season = NULL,
  game_type = 2,
  limit = 50,
  start = 0,
  sort = "points",
  direction = "DESC",
  lang = "en"
)
```

## Arguments

- report_type:

  Character report type. Default "summary". Common types: "summary",
  "penalties", "penaltykill", "penaltykilltime", "powerplay",
  "powerplaytime", "realtime", "faceoffpercentages", "faceoffwins",
  "goalsForAgainst", "goalsBy Period", "daysrest", "outshootoutshotby",
  "percentages", "scoretrailfirst", "shootout", "shottype"

- season:

  Character season in "YYYYYYYY" format (e.g., "20242025"). If NULL,
  uses current season.

- game_type:

  Integer game type: 2 = regular season (default), 3 = playoffs

- limit:

  Integer maximum number of results. Default 50.

- start:

  Integer start index for pagination. Default 0.

- sort:

  Character sort column. Default "points".

- direction:

  Character sort direction: "DESC" or "ASC". Default "DESC".

- lang:

  Character language code. Default "en".

## Value

Returns a data frame with team statistics.

## Examples

``` r
# \donttest{
  try(nhl_stats_teams())
#> 2026-04-08 03:01:25.686779: No team stats data
#> NULL
# }
```
