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
#> ── NHL Edge Team Zone Time Top 10 ───────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-10-08 12:59:10 UTC
#> # A tibble: 10 × 10
#>    offensive_zone_time neutral_zone_time defensive_zone_time team_abbrev
#>                  <dbl>             <dbl>               <dbl> <chr>      
#>  1               0.470             0.179               0.351 CAR        
#>  2               0.451             0.179               0.370 TBL        
#>  3               0.441             0.184               0.375 FLA        
#>  4               0.440             0.181               0.378 VGK        
#>  5               0.440             0.188               0.372 BUF        
#>  6               0.436             0.177               0.386 EDM        
#>  7               0.436             0.176               0.387 UTA        
#>  8               0.432             0.176               0.392 MIN        
#>  9               0.432             0.183               0.386 CGY        
#> 10               0.430             0.192               0.378 NJD        
#> # ℹ 6 more variables: team_slug <chr>, team_common_name_default <chr>,
#> #   team_place_name_with_preposition_default <chr>,
#> #   team_place_name_with_preposition_fr <chr>, team_team_logo_light <chr>,
#> #   team_team_logo_dark <chr>
# }
```
