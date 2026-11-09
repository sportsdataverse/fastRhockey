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
#> ℹ Data updated: 2026-11-09 19:39:39 UTC
#> # A tibble: 17 × 7
#>    area           sog sog_rank goals goals_rank shooting_pctg shooting_pctg_rank
#>    <chr>        <int>    <int> <int>      <int>         <dbl>              <int>
#>  1 Behind the …     6       10     0          7       NA                      NA
#>  2 Beyond Red …    18        3     1          6        0.0556                 17
#>  3 Center Point    34       17     1         17        0.0294                 22
#>  4 Crease          16       17     1         31        0.0625                 32
#>  5 High Slot       51        8     8         13        0.157                  15
#>  6 L Circle        66        2     7          6        0.106                  16
#>  7 L Corner         0       23     0          3       NA                      NA
#>  8 L Net Side       7       26     0         25       NA                      NA
#>  9 L Point         20       30     0         20       NA                      NA
#> 10 Low Slot       137        7    33          1        0.241                   4
#> 11 Offensive N…     9       17     1          5        0.111                   6
#> 12 Outside L       22       25     2          4        0.0909                  3
#> 13 Outside R       35        2     2          5        0.0571                 10
#> 14 R Circle        53        8     5         13        0.0943                 20
#> 15 R Corner         0       16     0          2       NA                      NA
#> 16 R Net Side      19        3     1         10        0.0526                 20
#> 17 R Point         37        8     2          6        0.0541                  8
# }
```
