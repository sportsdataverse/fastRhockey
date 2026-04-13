# **NHL Edge Goalie Shot Location Detail**

Returns the NHL Edge shot-location detail payload for a single goalie.
Wraps
`https://api-web.nhle.com/v1/edge/goalie-shot-location-detail/{playerId}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_goalie_shot_location_detail(player_id, season = NULL, game_type = 2)
```

## Arguments

- player_id:

  Integer NHL player ID (e.g., `8475883` for Andrei Vasilevskiy).

- season:

  Optional 4-digit end-year (e.g., `2025` for the 2024-25 season), an
  8-character API season (e.g., `"20242025"`), or `NULL` (default) for
  the current season via the `/now` endpoint.

- game_type:

  Integer game type. 1 = preseason, 2 = regular season (default), 3 =
  playoffs.

## Value

A `fastRhockey_data` tibble of shot-location details, or `NULL` on
failure / empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_goalie_shot_location_detail(player_id = 8475883))
#> ── NHL Edge Goalie Shot Location Detail ─────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:04:40 UTC
#> # A tibble: 17 × 9
#>    area       shots_against saves goals_against save_pctg shots_against_percen…¹
#>    <chr>              <int> <int>         <int>     <dbl>                  <dbl>
#>  1 Behind th…            11    11             0     1                      0.821
#>  2 Beyond Re…            37    37             0     1                      0.874
#>  3 Center Po…            44    41             3     0.932                  0.379
#>  4 Crease                40    28            12     0.7                    0.779
#>  5 High Slot             86    70            16     0.814                  0.632
#>  6 L Circle              78    71             7     0.910                  0.526
#>  7 L Corner               1     1             0     1                      0.379
#>  8 L Net Side            17    14             3     0.824                  0.484
#>  9 L Point               45    44             1     0.978                  0.495
#> 10 Low Slot             255   207            48     0.812                  0.684
#> 11 Offensive…            18    18             0     1                      0.568
#> 12 Outside L             54    53             1     0.981                  0.642
#> 13 Outside R             31    31             0     1                      0.379
#> 14 R Circle              62    51            11     0.823                  0.410
#> 15 R Corner               1     1             0     1                      0.453
#> 16 R Net Side            22    17             5     0.773                  0.642
#> 17 R Point               47    47             0     1                      0.495
#> # ℹ abbreviated name: ¹​shots_against_percentile
#> # ℹ 3 more variables: saves_percentile <dbl>, goals_against_percentile <dbl>,
#> #   save_pctg_percentile <dbl>
# }
```
