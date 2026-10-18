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

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| offensive_zone_time | numeric | Percentage of time spent in the offensive zone. |
| neutral_zone_time | numeric | Percentage of time spent in the neutral zone. |
| defensive_zone_time | numeric | Percentage of time spent in the defensive zone. |
| team_abbrev | character | Team abbreviation. |
| team_slug | character | Team URL slug. |
| team_common_name_default | character | Team common name. |
| team_place_name_with_preposition_default | character | Team place name with preposition. |
| team_place_name_with_preposition_fr | character | Team place name with preposition (French). |
| team_team_logo_light | character | Team light logo URL. |
| team_team_logo_dark | character | Team dark logo URL. |

## Examples

``` r
# \donttest{
  try(nhl_edge_team_zone_time_top_10(
    strength = "all",
    sort_by = "offensive"
  ))
#> ── NHL Edge Team Zone Time Top 10 ──────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:09:20 UTC
#> # A tibble: 10 × 11
#>    offensive_zone_time neutral_zone_time defensive_zone_time team_abbrev
#>                  <dbl>             <dbl>               <dbl> <chr>      
#>  1               0.464             0.176               0.360 CAR        
#>  2               0.442             0.178               0.380 FLA        
#>  3               0.435             0.179               0.386 UTA        
#>  4               0.433             0.182               0.384 NJD        
#>  5               0.432             0.182               0.386 VGK        
#>  6               0.428             0.182               0.391 TBL        
#>  7               0.426             0.176               0.398 MTL        
#>  8               0.421             0.187               0.392 LAK        
#>  9               0.419             0.177               0.404 OTT        
#> 10               0.418             0.179               0.403 PIT        
#> # ℹ 7 more variables: team_slug <chr>, team_common_name_default <chr>,
#> #   team_common_name_fr <chr>, team_place_name_with_preposition_default <chr>,
#> #   team_place_name_with_preposition_fr <chr>, team_team_logo_light <chr>,
#> #   team_team_logo_dark <chr>
# }
```
