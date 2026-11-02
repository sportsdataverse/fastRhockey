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
#> ── NHL Edge Team Shot Location Detail ───────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-02 13:00:01 UTC
#> # A tibble: 17 × 7
#>    area           sog sog_rank goals goals_rank shooting_pctg shooting_pctg_rank
#>    <chr>        <int>    <int> <int>      <int>         <dbl>              <int>
#>  1 Behind the …     4       13     0          6       NA                      NA
#>  2 Beyond Red …    16        5     1          6        0.0625                 13
#>  3 Center Point    26       20     1         15        0.0385                 17
#>  4 Crease          14       14     1         28        0.0714                 31
#>  5 High Slot       42       10     7          9        0.167                  14
#>  6 L Circle        57        2     6          5        0.105                  16
#>  7 L Corner         0       22     0          3       NA                      NA
#>  8 L Net Side       6       24     0         23       NA                      NA
#>  9 L Point         17       29     0         18       NA                      NA
#> 10 Low Slot       116        5    31          1        0.267                   1
#> 11 Offensive N…     6       21     1          3        0.167                   4
#> 12 Outside L       19       22     2          3        0.105                   4
#> 13 Outside R       31        1     2          2        0.0645                  8
#> 14 R Circle        49        4     5          5        0.102                  15
#> 15 R Corner         0       15     0          2       NA                      NA
#> 16 R Net Side      19        2     1          8        0.0526                 18
#> 17 R Point         28       12     2          5        0.0714                  7
# }
```
