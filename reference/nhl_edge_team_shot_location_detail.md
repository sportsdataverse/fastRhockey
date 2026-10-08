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
#> ℹ Data updated: 2026-10-08 12:59:07 UTC
#> # A tibble: 17 × 7
#>    area           sog sog_rank goals goals_rank shooting_pctg shooting_pctg_rank
#>    <chr>        <int>    <int> <int>      <int>         <dbl>              <int>
#>  1 Behind the …     1        9     0          3        NA                     NA
#>  2 Beyond Red …     2       19     0          3        NA                     NA
#>  3 Center Point     7       13     0         10        NA                     NA
#>  4 Crease           2       23     0         18        NA                     NA
#>  5 High Slot        8       20     1         14         0.125                 16
#>  6 L Circle        16        1     0         21        NA                     NA
#>  7 L Corner         0       12     0          3        NA                     NA
#>  8 L Net Side       2       17     0         12        NA                     NA
#>  9 L Point          1       31     0          4        NA                     NA
#> 10 Low Slot        39        2     7          5         0.180                 15
#> 11 Offensive N…     4        7     0          5        NA                     NA
#> 12 Outside L        9        1     1          2         0.111                  5
#> 13 Outside R        8        2     0          9        NA                     NA
#> 14 R Circle        12        7     2          2         0.167                  5
#> 15 R Corner         0        4     0          1        NA                     NA
#> 16 R Net Side       5        2     0          8        NA                     NA
#> 17 R Point          4       21     0          7        NA                     NA
# }
```
