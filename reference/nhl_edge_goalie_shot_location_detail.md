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

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| area | character | Net/ice zone the shots were taken from. |
| shots_against | integer | Shots faced from the area. |
| saves | integer | Saves made from the area. |
| goals_against | integer | Goals against from the area. |
| save_pctg | numeric | Save percentage for the area. |
| shots_against_percentile | numeric | League percentile rank for shots against. |
| saves_percentile | numeric | League percentile rank for saves. |
| goals_against_percentile | numeric | League percentile rank for goals against. |
| save_pctg_percentile | numeric | League percentile rank for save percentage. |

Returns `NULL` on failure / empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_goalie_shot_location_detail(player_id = 8475883))
#> ── NHL Edge Goalie Shot Location Detail ─────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-12 13:21:56 UTC
#> # A tibble: 17 × 9
#>    area       shots_against saves goals_against save_pctg shots_against_percen…¹
#>    <chr>              <int> <int>         <int>     <dbl>                  <dbl>
#>  1 Behind th…             3     3             0     1                       0.84
#>  2 Beyond Re…            21    21             0     1                       1   
#>  3 Center Po…            17    17             0     1                       0.8 
#>  4 Crease                19    15             4     0.789                   0.92
#>  5 High Slot             34    26             8     0.765                   0.92
#>  6 L Circle              33    32             1     0.970                   0.92
#>  7 L Corner               0     0             0     0                       0.76
#>  8 L Net Side            17    16             1     0.941                   1   
#>  9 L Point               24    24             0     1                       0.92
#> 10 Low Slot              76    63            13     0.829                   0.88
#> 11 Offensive…            13    13             0     1                       0.88
#> 12 Outside L             17    17             0     1                       0.8 
#> 13 Outside R             29    27             2     0.931                   0.96
#> 14 R Circle              23    22             1     0.957                   0.84
#> 15 R Corner               1     1             0     1                       0.92
#> 16 R Net Side             8     7             1     0.875                   0.92
#> 17 R Point               19    18             1     0.947                   0.76
#> # ℹ abbreviated name: ¹​shots_against_percentile
#> # ℹ 3 more variables: saves_percentile <dbl>, goals_against_percentile <dbl>,
#> #   save_pctg_percentile <dbl>
# }
```
