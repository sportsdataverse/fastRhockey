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

A `fastRhockey_data` tibble of shot locations, or `NULL` on failure /
empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_skater_shot_location_detail(player_id = 8478402))
#> ── NHL Edge Skater Shot Location Detail ─────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:04:47 UTC
#> # A tibble: 17 × 7
#>    area                  sog goals shooting_pctg sog_percentile goals_percentile
#>    <chr>               <int> <int>         <dbl>          <dbl>            <dbl>
#>  1 Behind the Net         11     0        0               1                0    
#>  2 Beyond Red Line         1     0        0               0.391            0    
#>  3 Center Point            4     0        0               0.695            0    
#>  4 Crease                 23     9        0.391           0.997            0.997
#>  5 High Slot              25     1        0.04            0.932            0.437
#>  6 L Circle               36     6        0.167           0.970            0.975
#>  7 L Corner                1     0        0               0.842            0    
#>  8 L Net Side             34     5        0.147           1                0.998
#>  9 L Point                 3     1        0.333           0.607            0.914
#> 10 Low Slot               94    17        0.181           0.993            0.977
#> 11 Offensive Neutral …     3     2        0.667           0.665            0.985
#> 12 Outside L              15     2        0.133           0.946            0.969
#> 13 Outside R               8     0        0               0.799            0    
#> 14 R Circle               28     2        0.0714          0.939            0.766
#> 15 R Corner                0     0        0               0                0    
#> 16 R Net Side             11     2        0.182           0.970            0.959
#> 17 R Point                 1     0        0               0.337            0    
#> # ℹ 1 more variable: shooting_pctg_percentile <dbl>
# }
```
