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
#> ℹ Data updated: 2026-10-18 05:49:33 UTC
#> # A tibble: 17 × 7
#>    area           sog sog_rank goals goals_rank shooting_pctg shooting_pctg_rank
#>    <chr>        <int>    <int> <int>      <int>         <dbl>              <int>
#>  1 Behind the …     2       10     0          3       NA                      NA
#>  2 Beyond Red …     6       14     1          2        0.167                   3
#>  3 Center Point    12       24     0         18       NA                      NA
#>  4 Crease           8       10     0         28       NA                      NA
#>  5 High Slot       14       29     2         22        0.143                  20
#>  6 L Circle        31        1     3          7        0.0968                 19
#>  7 L Corner         0       17     0          3       NA                      NA
#>  8 L Net Side       4       20     0         19       NA                      NA
#>  9 L Point          5       31     0         11       NA                      NA
#> 10 Low Slot        64        5    18          1        0.281                   1
#> 11 Offensive N…     4       13     0          6       NA                      NA
#> 12 Outside L       12       15     1          4        0.0833                  8
#> 13 Outside R       17        1     1          1        0.0588                 12
#> 14 R Circle        28        5     2          9        0.0714                 18
#> 15 R Corner         0        8     0          1       NA                      NA
#> 16 R Net Side       9        2     0         13       NA                      NA
#> 17 R Point          7       28     0         12       NA                      NA
# }
```
