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
#> ℹ Data updated: 2026-07-18 17:03:55 UTC
#> # A tibble: 17 × 7
#>    area                  sog goals shooting_pctg sog_percentile goals_percentile
#>    <chr>               <int> <int>         <dbl>          <dbl>            <dbl>
#>  1 Behind the Net         11     0         0              1                0    
#>  2 Beyond Red Line         1     0         0              0.397            0    
#>  3 Center Point            4     0         0              0.697            0    
#>  4 Crease                 23     9         0.391          0.997            0.997
#>  5 High Slot              27     1         0.037          0.945            0.432
#>  6 L Circle               36     6         0.167          0.969            0.976
#>  7 L Corner                1     0         0              0.842            0    
#>  8 L Net Side             34     5         0.147          1                0.998
#>  9 L Point                 3     1         0.333          0.608            0.915
#> 10 Low Slot               97    17         0.175          0.994            0.976
#> 11 Offensive Neutral …     3     2         0.667          0.664            0.985
#> 12 Outside L              15     2         0.133          0.946            0.969
#> 13 Outside R               9     0         0              0.826            0    
#> 14 R Circle               29     2         0.069          0.941            0.769
#> 15 R Corner                0     0         0              0                0    
#> 16 R Net Side             12     3         0.25           0.982            0.990
#> 17 R Point                 1     0         0              0.332            0    
#> # ℹ 1 more variable: shooting_pctg_percentile <dbl>
# }
```
