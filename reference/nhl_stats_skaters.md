# **NHL Stats API — Skater Stats**

Queries the NHL Stats REST API for skater statistics. Supports various
report types and filtering.

## Usage

``` r
nhl_stats_skaters(
  report_type = "summary",
  season = NULL,
  game_type = 2,
  limit = 50,
  start = 0,
  sort = NULL,
  direction = "DESC",
  lang = "en"
)
```

## Arguments

- report_type:

  Character report type. Default "summary". Common types: "summary",
  "bios", "faceoffpercentages", "faceoffwins", "goalsForAgainst",
  "realtime", "penalties", "penaltykill", "powerplay",
  "puckPossessions", "summaryshooting", "percentages", "scoringRates",
  "scoringpergame", "shootout", "shottype", "timeonice"

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

  Character sort column. Default varies by report.

- direction:

  Character sort direction: "DESC" or "ASC". Default "DESC".

- lang:

  Character language code. Default "en".

## Value

Returns a data frame with skater statistics.

## Examples

``` r
# \donttest{
  try(nhl_stats_skaters())
#> 2026-04-07 13:55:45.968054: No skater stats data
#> NULL
# }
```
