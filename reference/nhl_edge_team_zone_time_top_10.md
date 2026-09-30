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
#> ℹ Data updated: 2026-09-30 14:44:42 UTC
#> # A tibble: 10 × 10
#>    offensive_zone_time neutral_zone_time defensive_zone_time team_abbrev
#>                  <dbl>             <dbl>               <dbl> <chr>      
#>  1               0.472             0.182               0.346 EDM        
#>  2               0.414             0.177               0.409 TOR        
#>  3               0.414             0.182               0.404 CHI        
#>  4               0.413             0.180               0.407 FLA        
#>  5               0.409             0.177               0.414 MTL        
#>  6               0.407             0.197               0.396 NYR        
#>  7               0.407             0.180               0.413 CAR        
#>  8               0.404             0.182               0.414 VGK        
#>  9               0.396             0.197               0.407 BOS        
#> 10               0.346             0.182               0.472 VAN        
#> # ℹ 6 more variables: team_slug <chr>, team_common_name_default <chr>,
#> #   team_place_name_with_preposition_default <chr>,
#> #   team_place_name_with_preposition_fr <chr>, team_team_logo_light <chr>,
#> #   team_team_logo_dark <chr>
# }
```
