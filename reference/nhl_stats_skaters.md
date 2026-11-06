# **NHL Stats API - Skater Stats**

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
#> ── NHL Stats Skaters ────────────────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-06 07:49:58 UTC
#> # A tibble: 50 × 26
#>    assists ev_goals ev_points faceoff_win_pct game_winning_goals games_played
#>      <int>    <int>     <int>           <dbl>              <int>        <int>
#>  1      34        8        24           0.489                  0           17
#>  2      17        7        19           0.591                  1           17
#>  3      20        7        20          NA                      2           17
#>  4      18        7        21           0.492                  1           17
#>  5      11       11        19           0.2                    3           17
#>  6      15        8        17           0.548                  2           17
#>  7      19        3         7           0.435                  1           16
#>  8      14        4        13           0.558                  1           18
#>  9      16        6        13           0.460                  1           18
#> 10      18        3        10           0                      1           16
#> # ℹ 40 more rows
#> # ℹ 20 more variables: goals <int>, last_name <chr>, ot_goals <int>,
#> #   penalty_minutes <int>, player_id <int>, plus_minus <int>, points <int>,
#> #   points_per_game <dbl>, position_code <chr>, pp_goals <int>,
#> #   pp_points <int>, season_id <int>, sh_goals <int>, sh_points <int>,
#> #   shooting_pct <dbl>, shoots_catches <chr>, shots <int>,
#> #   skater_full_name <chr>, team_abbrevs <chr>, time_on_ice_per_game <dbl>
# }
```
