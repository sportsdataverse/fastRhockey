# **NHL Edge Team Zone Time Top 10**

Returns the NHL Edge top-10 team zone-time leaderboard. Wraps
`https://api-web.nhle.com/v1/edge/team-zone-time-top-10/{strength}/{sortBy}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_team_zone_time_top_10(strength, sort_by, season = NULL, game_type = 2)
```

## Arguments

- strength:

  Character strength state (e.g., `"all"`, `"ev"`, `"pp"`, `"pk"`).

- sort_by:

  Character metric to sort the leaderboard by (e.g., `"offensive"`,
  `"defensive"`, `"neutral"`).

- season:

  Optional 4-digit end-year (e.g., `2025` for the 2024-25 season), an
  8-character API season (e.g., `"20242025"`), or `NULL` (default) for
  the current season via the `/now` endpoint.

- game_type:

  Integer game type. 1 = preseason, 2 = regular season (default), 3 =
  playoffs.

## Value

A `fastRhockey_data` tibble with the top-10 team leaderboard, or `NULL`
on failure / empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_team_zone_time_top_10(
    strength = "all",
    sort_by = "offensive"
  ))
#> ── NHL Edge Team Zone Time Top 10 ───────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-11 16:20:23 UTC
#> # A tibble: 10 × 10
#>    offensive_zone_time neutral_zone_time defensive_zone_time team_abbrev
#>                  <dbl>             <dbl>               <dbl> <chr>      
#>  1               0.453             0.182               0.365 CAR        
#>  2               0.432             0.177               0.391 TBL        
#>  3               0.431             0.166               0.402 DAL        
#>  4               0.428             0.177               0.395 PIT        
#>  5               0.426             0.172               0.403 BUF        
#>  6               0.424             0.186               0.390 VGK        
#>  7               0.417             0.182               0.401 ANA        
#>  8               0.411             0.182               0.408 COL        
#>  9               0.405             0.169               0.426 MIN        
#> 10               0.405             0.186               0.409 LAK        
#> # ℹ 6 more variables: team_slug <chr>, team_common_name_default <chr>,
#> #   team_place_name_with_preposition_default <chr>,
#> #   team_place_name_with_preposition_fr <chr>, team_team_logo_light <chr>,
#> #   team_team_logo_dark <chr>
# }
```
