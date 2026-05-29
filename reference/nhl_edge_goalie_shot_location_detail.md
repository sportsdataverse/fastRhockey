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
#> ℹ Data updated: 2026-05-29 16:26:04 UTC
#> # A tibble: 17 × 9
#>    area       shots_against saves goals_against save_pctg shots_against_percen…¹
#>    <chr>              <int> <int>         <int>     <dbl>                  <dbl>
#>  1 Behind th…             3     3             0     1                      0.833
#>  2 Beyond Re…            20    20             0     1                      1    
#>  3 Center Po…            13    13             0     1                      0.667
#>  4 Crease                14    12             2     0.857                  0.958
#>  5 High Slot             26    20             6     0.769                  0.833
#>  6 L Circle              25    25             0     1                      0.75 
#>  7 L Corner               0     0             0     0                      0.75 
#>  8 L Net Side            13    12             1     0.923                  0.917
#>  9 L Point               18    18             0     1                      0.875
#> 10 Low Slot              57    49             8     0.860                  0.75 
#> 11 Offensive…             9     9             0     1                      0.833
#> 12 Outside L             14    14             0     1                      0.75 
#> 13 Outside R             19    18             1     0.947                  0.875
#> 14 R Circle              17    17             0     1                      0.583
#> 15 R Corner               0     0             0     0                      0.792
#> 16 R Net Side             6     5             1     0.833                  0.792
#> 17 R Point               11    11             0     1                      0.667
#> # ℹ abbreviated name: ¹​shots_against_percentile
#> # ℹ 3 more variables: saves_percentile <dbl>, goals_against_percentile <dbl>,
#> #   save_pctg_percentile <dbl>
# }
```
