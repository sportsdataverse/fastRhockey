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
#> ℹ Data updated: 2026-11-03 10:07:34 UTC
#> # A tibble: 17 × 7
#>    area                  sog goals shooting_pctg sog_percentile goals_percentile
#>    <chr>               <int> <int>         <dbl>          <dbl>            <dbl>
#>  1 Behind the Net          2     0         0              0.992            0.990
#>  2 Beyond Red Line         0     0        NA              0.702            0.969
#>  3 Center Point            1     0         0              0.858            0.967
#>  4 Crease                  4     2         0.5            0.982            0.992
#>  5 High Slot               5     1         0.2            0.920            0.942
#>  6 L Circle                6     1         0.167          0.957            0.973
#>  7 L Corner                0     0        NA              0.955            0.996
#>  8 L Net Side              1     1         1              0.856            0.990
#>  9 L Point                 0     0        NA              0.663            0.986
#> 10 Low Slot               10     2         0.2            0.819            0.866
#> 11 Offensive Neutral …     2     1         0.5            0.969            1    
#> 12 Outside L               1     0         0              0.721            0.945
#> 13 Outside R               3     0         0              0.947            0.953
#> 14 R Circle                6     1         0.167          0.951            0.969
#> 15 R Corner                0     0        NA              0.969            0.998
#> 16 R Net Side              1     0         0              0.858            0.942
#> 17 R Point                 0     0        NA              0.639            0.971
#> # ℹ 1 more variable: shooting_pctg_percentile <dbl>
# }
```
