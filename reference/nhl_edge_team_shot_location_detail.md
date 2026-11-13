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
#> ℹ Data updated: 2026-11-13 21:32:20 UTC
#> # A tibble: 17 × 7
#>    area           sog sog_rank goals goals_rank shooting_pctg shooting_pctg_rank
#>    <chr>        <int>    <int> <int>      <int>         <dbl>              <int>
#>  1 Behind the …     6       12     0          8       NA                      NA
#>  2 Beyond Red …    20        3     1          8        0.05                   18
#>  3 Center Point    40       12     1         20        0.025                  25
#>  4 Crease          16       20     1         32        0.0625                 32
#>  5 High Slot       59        6     8         14        0.136                  18
#>  6 L Circle        70        3     8          4        0.114                  14
#>  7 L Corner         0       25     0          3       NA                      NA
#>  8 L Net Side       8       26     0         25       NA                      NA
#>  9 L Point         25       29     1         10        0.04                    9
#> 10 Low Slot       152        6    35          1        0.230                   4
#> 11 Offensive N…    12       12     1          5        0.0833                 10
#> 12 Outside L       26       22     2          4        0.0769                  6
#> 13 Outside R       39        2     3          3        0.0769                  6
#> 14 R Circle        58        6     5         16        0.0862                 21
#> 15 R Corner         0       16     0          2       NA                      NA
#> 16 R Net Side      19        3     1         12        0.0526                 21
#> 17 R Point         39        8     2          6        0.0513                  8
# }
```
