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
#> ── NHL Edge Skater Shot Location Detail ────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:17:49 UTC
#> # A tibble: 17 × 7
#>    area                  sog goals shooting_pctg sog_percentile goals_percentile
#>    <chr>               <int> <int>         <dbl>          <dbl>            <dbl>
#>  1 Behind the Net          2     0         0              0.994            0.990
#>  2 Beyond Red Line         0     0        NA              0.732            0.973
#>  3 Center Point            1     0         0              0.864            0.971
#>  4 Crease                  3     2         0.667          0.964            0.994
#>  5 High Slot               4     1         0.25           0.910            0.950
#>  6 L Circle                4     1         0.25           0.922            0.983
#>  7 L Corner                0     0        NA              0.958            0.996
#>  8 L Net Side              1     1         1              0.883            0.992
#>  9 L Point                 0     0        NA              0.683            0.985
#> 10 Low Slot                7     1         0.143          0.725            0.746
#> 11 Offensive Neutral …     1     1         1              0.929            1    
#> 12 Outside L               1     0         0              0.753            0.950
#> 13 Outside R               3     0         0              0.962            0.958
#> 14 R Circle                6     1         0.167          0.966            0.977
#> 15 R Corner                0     0        NA              0.975            0.998
#> 16 R Net Side              0     0        NA              0.652            0.948
#> 17 R Point                 0     0        NA              0.667            0.973
#> # ℹ 1 more variable: shooting_pctg_percentile <dbl>
# }
```
