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
#> ── NHL Edge Skater Shot Location Detail ─────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-25 20:23:10 UTC
#> # A tibble: 17 × 7
#>    area                  sog goals shooting_pctg sog_percentile goals_percentile
#>    <chr>               <int> <int>         <dbl>          <dbl>            <dbl>
#>  1 Behind the Net          3     0        0               0.998            0.986
#>  2 Beyond Red Line         0     0       NA               0.636            0.954
#>  3 Center Point            2     0        0               0.882            0.954
#>  4 Crease                  4     2        0.5             0.954            0.979
#>  5 High Slot               7     1        0.143           0.919            0.888
#>  6 L Circle                9     1        0.111           0.961            0.922
#>  7 L Corner                0     0       NA               0.934            0.994
#>  8 L Net Side              1     1        1               0.777            0.986
#>  9 L Point                 1     0        0               0.789            0.959
#> 10 Low Slot               19     5        0.263           0.907            0.954
#> 11 Offensive Neutral …     3     2        0.667           0.979            1    
#> 12 Outside L               4     0        0               0.922            0.920
#> 13 Outside R               4     0        0               0.928            0.932
#> 14 R Circle               12     1        0.0833          0.975            0.932
#> 15 R Corner                0     0       NA               0.954            0.998
#> 16 R Net Side              3     1        0.333           0.948            0.994
#> 17 R Point                 0     0       NA               0.525            0.971
#> # ℹ 1 more variable: shooting_pctg_percentile <dbl>
# }
```
