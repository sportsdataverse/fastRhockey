# **NHL Edge Skater Shot Location Detail**

Returns the NHL Edge shot-location detail payload for a single skater
(heatmap-style data). Wraps
`https://api-web.nhle.com/v1/edge/skater-shot-location-detail/{playerId}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_skater_shot_location_detail(player_id, season = NULL, game_type = 2)
```

## Arguments

- player_id:

  Integer NHL player ID (e.g., `8478402` for Connor McDavid).

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
| area | character | Shot location area on the ice. |
| sog | integer | Shots on goal from the area. |
| goals | integer | Goals scored from the area. |
| shooting_pctg | numeric | Shooting percentage from the area. |
| sog_percentile | numeric | League percentile rank for shots on goal. |
| goals_percentile | numeric | League percentile rank for goals. |
| shooting_pctg_percentile | numeric | League percentile rank for shooting percentage. |

## Examples

``` r
# \donttest{
  try(nhl_edge_skater_shot_location_detail(player_id = 8478402))
#> ── NHL Edge Skater Shot Location Detail ─────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-09-30 14:44:34 UTC
#> # A tibble: 17 × 7
#>    area                  sog goals shooting_pctg sog_percentile goals_percentile
#>    <chr>               <int> <int> <lgl>                  <dbl>            <dbl>
#>  1 Behind the Net          0     0 NA                     0.975            1    
#>  2 Beyond Red Line         0     0 NA                     0.967            1    
#>  3 Center Point            0     0 NA                     0.967            1    
#>  4 Crease                  0     0 NA                     0.942            0.983
#>  5 High Slot               0     0 NA                     0.867            0.983
#>  6 L Circle                0     0 NA                     0.858            0.975
#>  7 L Corner                0     0 NA                     0.983            1    
#>  8 L Net Side              0     0 NA                     0.983            1    
#>  9 L Point                 0     0 NA                     0.95             1    
#> 10 Low Slot                0     0 NA                     0.592            0.908
#> 11 Offensive Neutral …     0     0 NA                     0.958            1    
#> 12 Outside L               0     0 NA                     0.875            1    
#> 13 Outside R               0     0 NA                     0.95             1    
#> 14 R Circle                0     0 NA                     0.858            0.975
#> 15 R Corner                0     0 NA                     1                1    
#> 16 R Net Side              0     0 NA                     0.95             1    
#> 17 R Point                 0     0 NA                     0.967            1    
#> # ℹ 1 more variable: shooting_pctg_percentile <lgl>
# }
```
