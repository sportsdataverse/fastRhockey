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
#> ℹ Data updated: 2026-11-09 10:28:42 UTC
#> # A tibble: 17 × 7
#>    area                  sog goals shooting_pctg sog_percentile goals_percentile
#>    <chr>               <int> <int>         <dbl>          <dbl>            <dbl>
#>  1 Behind the Net          2     0         0              0.988            0.990
#>  2 Beyond Red Line         0     0        NA              0.684            0.968
#>  3 Center Point            1     0         0              0.833            0.966
#>  4 Crease                  4     2         0.5            0.976            0.984
#>  5 High Slot               5     1         0.2            0.895            0.932
#>  6 L Circle                7     1         0.143          0.956            0.954
#>  7 L Corner                0     0        NA              0.950            0.996
#>  8 L Net Side              1     1         1              0.827            0.990
#>  9 L Point                 0     0        NA              0.597            0.982
#> 10 Low Slot               14     3         0.214          0.899            0.909
#> 11 Offensive Neutral …     2     1         0.5            0.956            0.998
#> 12 Outside L               2     0         0              0.829            0.938
#> 13 Outside R               3     0         0              0.921            0.946
#> 14 R Circle                7     1         0.143          0.942            0.952
#> 15 R Corner                0     0        NA              0.968            0.998
#> 16 R Net Side              1     0         0              0.845            0.936
#> 17 R Point                 0     0        NA              0.599            0.970
#> # ℹ 1 more variable: shooting_pctg_percentile <dbl>
# }
```
