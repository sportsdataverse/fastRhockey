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
#> ── NHL Edge Team Shot Location Detail ───────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-10-12 12:08:18 UTC
#> # A tibble: 17 × 7
#>    area           sog sog_rank goals goals_rank shooting_pctg shooting_pctg_rank
#>    <chr>        <int>    <int> <int>      <int>         <dbl>              <int>
#>  1 Behind the …     1       14     0          3       NA                      NA
#>  2 Beyond Red …     5        8     1          2        0.2                     3
#>  3 Center Point    10       16     0         15       NA                      NA
#>  4 Crease           6        8     0         25       NA                      NA
#>  5 High Slot       12       20     2         11        0.167                  14
#>  6 L Circle        22        4     0         25       NA                      NA
#>  7 L Corner         0       14     0          3       NA                      NA
#>  8 L Net Side       2       23     0         16       NA                      NA
#>  9 L Point          1       32     0          7       NA                      NA
#> 10 Low Slot        51        4    13          1        0.255                   8
#> 11 Offensive N…     4        8     0          5       NA                      NA
#> 12 Outside L        9       12     1          3        0.111                   5
#> 13 Outside R       10        3     1          1        0.1                     8
#> 14 R Circle        21        2     2          5        0.0952                 14
#> 15 R Corner         0        5     0          1       NA                      NA
#> 16 R Net Side       8        2     0         11       NA                      NA
#> 17 R Point          5       30     0          8       NA                      NA
# }
```
