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
#> ℹ Data updated: 2026-11-26 22:31:33 UTC
#> # A tibble: 50 × 26
#>    assists ev_goals ev_points faceoff_win_pct game_winning_goals games_played
#>      <int>    <int>     <int>           <dbl>              <int>        <int>
#>  1      45       11        32           0.491                  0           27
#>  2      30       11        34           0.2                    4           27
#>  3      24       12        29           0.568                  3           27
#>  4      29        9        26          NA                      4           27
#>  5      15       13        26          NA                      2           26
#>  6      26        6        21          NA                      3           24
#>  7      21        6        20           0.555                  3           25
#>  8      20        9        22           0.547                  2           26
#>  9      23        6        22           1                      1           24
#> 10      26        5        17           0                      1           25
#> # ℹ 40 more rows
#> # ℹ 20 more variables: goals <int>, last_name <chr>, ot_goals <int>,
#> #   penalty_minutes <int>, player_id <int>, plus_minus <int>, points <int>,
#> #   points_per_game <dbl>, position_code <chr>, pp_goals <int>,
#> #   pp_points <int>, season_id <int>, sh_goals <int>, sh_points <int>,
#> #   shooting_pct <dbl>, shoots_catches <chr>, shots <int>,
#> #   skater_full_name <chr>, team_abbrevs <chr>, time_on_ice_per_game <dbl>
# }
```
