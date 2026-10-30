# **NHL Edge Team Shot Location Detail**

Returns the NHL Edge shot-location detail payload for a single team.
Wraps
`https://api-web.nhle.com/v1/edge/team-shot-location-detail/{teamId}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_team_shot_location_detail(team_id, season = NULL, game_type = 2)
```

## Arguments

- team_id:

  Integer NHL team ID (e.g., `10` for Toronto Maple Leafs).

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
| area | character | Shot-location area on the ice. |
| sog | integer | Shots on goal from the area. |
| sog_rank | integer | League rank for shots on goal from the area. |
| goals | integer | Goals scored from the area. |
| goals_rank | integer | League rank for goals scored from the area. |
| shooting_pctg | numeric | Shooting percentage from the area. |
| shooting_pctg_rank | integer | League rank for shooting percentage from the area. |

## Examples

``` r
# \donttest{
  try(nhl_edge_team_shot_location_detail(team_id = 10))
#> ── NHL Edge Team Shot Location Detail ──────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:17:55 UTC
#> # A tibble: 17 × 7
#>    area           sog sog_rank goals goals_rank shooting_pctg shooting_pctg_rank
#>    <chr>        <int>    <int> <int>      <int>         <dbl>              <int>
#>  1 Behind the …     3       13     0          6       NA                      NA
#>  2 Beyond Red …    16        2     1          5        0.0625                 12
#>  3 Center Point    25       15     0         21       NA                      NA
#>  4 Crease          14       11     1         26        0.0714                 29
#>  5 High Slot       38        9     6          9        0.158                  18
#>  6 L Circle        56        1     6          3        0.107                  16
#>  7 L Corner         0       20     0          3       NA                      NA
#>  8 L Net Side       6       23     0         22       NA                      NA
#>  9 L Point         17       19     0         18       NA                      NA
#> 10 Low Slot       106        5    30          1        0.283                   1
#> 11 Offensive N…     6       18     1          3        0.167                   4
#> 12 Outside L       16       24     2          2        0.125                   2
#> 13 Outside R       27        2     1          3        0.037                  20
#> 14 R Circle        47        1     5          4        0.106                  14
#> 15 R Corner         0       14     0          2       NA                      NA
#> 16 R Net Side      17        3     1          7        0.0588                 17
#> 17 R Point         27        9     2          4        0.0741                  7
# }
```
