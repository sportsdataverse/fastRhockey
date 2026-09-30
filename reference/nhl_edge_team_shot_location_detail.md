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
#> ℹ Data updated: 2026-09-30 14:44:39 UTC
#> # A tibble: 17 × 7
#>    area           sog sog_rank goals goals_rank shooting_pctg shooting_pctg_rank
#>    <chr>        <int>    <int> <int>      <int>         <dbl>              <int>
#>  1 Behind the …     0        4     0          1        NA                     NA
#>  2 Beyond Red …     1        3     0          1        NA                     NA
#>  3 Center Point     2        3     0          3        NA                     NA
#>  4 Crease           0        7     0          3        NA                     NA
#>  5 High Slot        1        7     0          4        NA                     NA
#>  6 L Circle         6        1     0          3        NA                     NA
#>  7 L Corner         0        2     0          1        NA                     NA
#>  8 L Net Side       1        1     0          1        NA                     NA
#>  9 L Point          0        9     0          2        NA                     NA
#> 10 Low Slot         9        2     1          5         0.111                  6
#> 11 Offensive N…     0        6     0          1        NA                     NA
#> 12 Outside L        3        2     0          1        NA                     NA
#> 13 Outside R        1        3     0          1        NA                     NA
#> 14 R Circle         3        2     1          1         0.333                  2
#> 15 R Corner         0        1     0          1        NA                     NA
#> 16 R Net Side       1        3     0          1        NA                     NA
#> 17 R Point          0        9     0          2        NA                     NA
# }
```
