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
#> ℹ Data updated: 2026-11-06 07:49:15 UTC
#> # A tibble: 17 × 7
#>    area                  sog goals shooting_pctg sog_percentile goals_percentile
#>    <chr>               <int> <int>         <dbl>          <dbl>            <dbl>
#>  1 Behind the Net          2     0         0              0.990            0.990
#>  2 Beyond Red Line         0     0        NA              0.695            0.970
#>  3 Center Point            1     0         0              0.844            0.968
#>  4 Crease                  4     2         0.5            0.976            0.984
#>  5 High Slot               5     1         0.2            0.908            0.935
#>  6 L Circle                6     1         0.167          0.941            0.961
#>  7 L Corner                0     0        NA              0.955            0.996
#>  8 L Net Side              1     1         1              0.837            0.990
#>  9 L Point                 0     0        NA              0.626            0.982
#> 10 Low Slot               13     3         0.231          0.894            0.927
#> 11 Offensive Neutral …     2     1         0.5            0.965            0.998
#> 12 Outside L               1     0         0              0.691            0.943
#> 13 Outside R               3     0         0              0.939            0.951
#> 14 R Circle                7     1         0.143          0.959            0.955
#> 15 R Corner                0     0        NA              0.968            0.998
#> 16 R Net Side              1     0         0              0.850            0.937
#> 17 R Point                 0     0        NA              0.620            0.970
#> # ℹ 1 more variable: shooting_pctg_percentile <dbl>
# }
```
