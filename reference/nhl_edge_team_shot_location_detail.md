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
#> ℹ Data updated: 2026-11-04 09:34:52 UTC
#> # A tibble: 17 × 7
#>    area           sog sog_rank goals goals_rank shooting_pctg shooting_pctg_rank
#>    <chr>        <int>    <int> <int>      <int>         <dbl>              <int>
#>  1 Behind the …     4       13     0          7       NA                      NA
#>  2 Beyond Red …    16        5     1          6        0.0625                 15
#>  3 Center Point    28       21     1         16        0.0357                 20
#>  4 Crease          15       15     1         28        0.0667                 32
#>  5 High Slot       45       10     8         10        0.178                  13
#>  6 L Circle        61        2     6          6        0.0984                 18
#>  7 L Corner         0       22     0          3       NA                      NA
#>  8 L Net Side       6       25     0         24       NA                      NA
#>  9 L Point         19       26     0         19       NA                      NA
#> 10 Low Slot       120        7    32          1        0.267                   1
#> 11 Offensive N…     8       15     1          5        0.125                   6
#> 12 Outside L       21       24     2          3        0.0952                  3
#> 13 Outside R       33        1     2          3        0.0606                  8
#> 14 R Circle        51        4     5          8        0.098                  17
#> 15 R Corner         0       15     0          2       NA                      NA
#> 16 R Net Side      19        2     1          9        0.0526                 18
#> 17 R Point         33        8     2          6        0.0606                  8
# }
```
