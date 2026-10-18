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
#> ℹ Data updated: 2026-10-18 05:49:29 UTC
#> # A tibble: 17 × 7
#>    area                  sog goals shooting_pctg sog_percentile goals_percentile
#>    <chr>               <int> <int>         <dbl>          <dbl>            <dbl>
#>  1 Behind the Net          1     0         0              0.989            0.996
#>  2 Beyond Red Line         0     0        NA              0.806            0.986
#>  3 Center Point            1     0         0              0.919            0.982
#>  4 Crease                  2     1         0.5            0.964            0.984
#>  5 High Slot               3     1         0.333          0.944            0.975
#>  6 L Circle                0     0        NA              0.409            0.874
#>  7 L Corner                0     0        NA              0.964            0.996
#>  8 L Net Side              1     1         1              0.946            1    
#>  9 L Point                 0     0        NA              0.779            0.993
#> 10 Low Slot                4     0         0              0.688            0.571
#> 11 Offensive Neutral …     1     1         1              0.968            1    
#> 12 Outside L               0     0        NA              0.587            0.964
#> 13 Outside R               1     0         0              0.878            0.980
#> 14 R Circle                2     0         0              0.844            0.885
#> 15 R Corner                0     0        NA              0.984            1    
#> 16 R Net Side              0     0        NA              0.743            0.966
#> 17 R Point                 0     0        NA              0.772            0.982
#> # ℹ 1 more variable: shooting_pctg_percentile <dbl>
# }
```
