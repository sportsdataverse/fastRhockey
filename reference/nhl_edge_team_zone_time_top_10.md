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
#> ℹ Data updated: 2026-04-13 17:05:14 UTC
#> # A tibble: 10 × 11
#>    offensive_zone_time neutral_zone_time defensive_zone_time team_abbrev
#>                  <dbl>             <dbl>               <dbl> <chr>      
#>  1               0.457             0.183               0.360 CAR        
#>  2               0.434             0.175               0.391 OTT        
#>  3               0.427             0.182               0.390 COL        
#>  4               0.422             0.178               0.400 FLA        
#>  5               0.421             0.180               0.399 NSH        
#>  6               0.421             0.184               0.395 VGK        
#>  7               0.418             0.177               0.406 EDM        
#>  8               0.416             0.173               0.411 NYR        
#>  9               0.415             0.184               0.401 ANA        
#> 10               0.414             0.177               0.408 PIT        
#> # ℹ 7 more variables: team_slug <chr>, team_common_name_default <chr>,
#> #   team_common_name_fr <chr>, team_place_name_with_preposition_default <chr>,
#> #   team_place_name_with_preposition_fr <chr>, team_team_logo_light <chr>,
#> #   team_team_logo_dark <chr>
# }
```
