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
#> ℹ Data updated: 2026-05-11 18:38:20 UTC
#> # A tibble: 17 × 9
#>    area       shots_against saves goals_against save_pctg shots_against_percen…¹
#>    <chr>              <int> <int>         <int>     <dbl>                  <dbl>
#>  1 Behind th…             2     2             0     1                      0.696
#>  2 Beyond Re…            17    17             0     1                      1    
#>  3 Center Po…             7     7             0     1                      0.391
#>  4 Crease                12    11             1     0.917                  1    
#>  5 High Slot             16    13             3     0.812                  0.652
#>  6 L Circle              19    19             0     1                      0.739
#>  7 L Corner               0     0             0     0                      0.783
#>  8 L Net Side            10     9             1     0.9                    0.956
#>  9 L Point               13    13             0     1                      0.826
#> 10 Low Slot              41    38             3     0.927                  0.609
#> 11 Offensive…             7     7             0     1                      0.783
#> 12 Outside L             13    13             0     1                      0.696
#> 13 Outside R             16    15             1     0.938                  0.913
#> 14 R Circle              14    14             0     1                      0.565
#> 15 R Corner               0     0             0     0                      0.826
#> 16 R Net Side             6     5             1     0.833                  0.826
#> 17 R Point                8     8             0     1                      0.435
#> # ℹ abbreviated name: ¹​shots_against_percentile
#> # ℹ 3 more variables: saves_percentile <dbl>, goals_against_percentile <dbl>,
#> #   save_pctg_percentile <dbl>
# }
```
