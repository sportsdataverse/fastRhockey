# **NHL Game IDs by Season**

Aggregator helper that iterates every NHL team's season schedule (via
[`nhl_club_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nhl_club_schedule.md))
and returns a deduplicated tidy data frame of game IDs for the requested
season and game types. Mirrors the `Helpers.game_ids_by_season`
convenience helper from the `nhl-api-py` Python client.

## Usage

``` r
nhl_game_ids_by_season(
  season,
  game_types = c(2L),
  team_abbr = NULL,
  sleep_rate = 0
)
```

## Arguments

- season:

  Integer four-digit *end year* of the season (e.g. `2025` for the
  2024-25 season). Passed through to
  [`nhl_club_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nhl_club_schedule.md)
  which uses the same end-year convention.

- game_types:

  Integer vector of NHL game types to keep. `1` = preseason, `2` =
  regular season (default), `3` = playoffs.

- team_abbr:

  Optional character three-letter team abbreviation (e.g. `"TOR"`). If
  supplied, only that team's schedule is fetched instead of iterating
  all 32 NHL clubs.

- sleep_rate:

  Numeric seconds to
  [`Sys.sleep()`](https://rdrr.io/r/base/Sys.sleep.html) between
  per-team API calls when iterating all teams. Defaults to `0` (no
  delay).

## Value

A `fastRhockey_data` / `data.frame` with one row per unique game,
containing `game_id`, `season`, `game_type`, `game_date`, `home_team`,
and `away_team`. Returns `NULL` on outer failure.

## Examples

``` r
# \donttest{
  try(nhl_game_ids_by_season(season = 2025, team_abbr = "TOR"))
#> ── NHL Game IDs by Season ───────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:16 UTC
#> # A tibble: 82 × 6
#>       game_id   season game_type game_date  home_team away_team
#>         <int>    <int>     <int> <chr>      <chr>     <chr>    
#>  1 2024020006 20242025         2 2024-10-09 MTL       TOR      
#>  2 2024020015 20242025         2 2024-10-10 NJD       TOR      
#>  3 2024020026 20242025         2 2024-10-12 TOR       PIT      
#>  4 2024020058 20242025         2 2024-10-16 TOR       LAK      
#>  5 2024020079 20242025         2 2024-10-19 TOR       NYR      
#>  6 2024020091 20242025         2 2024-10-21 TOR       TBL      
#>  7 2024020097 20242025         2 2024-10-22 CBJ       TOR      
#>  8 2024020110 20242025         2 2024-10-24 TOR       STL      
#>  9 2024020125 20242025         2 2024-10-26 BOS       TOR      
#> 10 2024020143 20242025         2 2024-10-28 WPG       TOR      
#> # ℹ 72 more rows
# }
```
