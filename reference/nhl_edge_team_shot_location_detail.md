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
#> ℹ Data updated: 2026-11-26 22:30:55 UTC
#> # A tibble: 17 × 7
#>    area           sog sog_rank goals goals_rank shooting_pctg shooting_pctg_rank
#>    <chr>        <int>    <int> <int>      <int>         <dbl>              <int>
#>  1 Behind the …     6       15     0          9       NA                      NA
#>  2 Beyond Red …    28        2     2          4        0.0714                 13
#>  3 Center Point    54       12     1         22        0.0185                 27
#>  4 Crease          19       26     2         29        0.105                  32
#>  5 High Slot       73       10    10         15        0.137                  18
#>  6 L Circle        82        3     8         10        0.0976                 18
#>  7 L Corner         1       12     1          1        1                       1
#>  8 L Net Side      10       29     0         27       NA                      NA
#>  9 L Point         37       22     1         18        0.027                  23
#> 10 Low Slot       191        7    42          2        0.220                   6
#> 11 Offensive N…    12       21     1          6        0.0833                  8
#> 12 Outside L       29       29     2          7        0.069                   6
#> 13 Outside R       44        3     3          5        0.0682                  7
#> 14 R Circle        70        9     6         18        0.0857                 22
#> 15 R Corner         1       10     0          2       NA                      NA
#> 16 R Net Side      21        7     1         14        0.0476                 24
#> 17 R Point         49       11     2          7        0.0408                 10
# }
```
