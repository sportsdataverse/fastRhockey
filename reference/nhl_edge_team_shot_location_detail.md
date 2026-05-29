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
#> ℹ Data updated: 2026-05-29 17:06:33 UTC
#> # A tibble: 17 × 7
#>    area           sog sog_rank goals goals_rank shooting_pctg shooting_pctg_rank
#>    <chr>        <int>    <int> <int>      <int>         <dbl>              <int>
#>  1 Behind the …    10       28     0         20        0                      20
#>  2 Beyond Red …    82        3     4         19        0.0488                 25
#>  3 Center Point   146       30     5         29        0.0342                 26
#>  4 Crease          76       19    18         20        0.237                  20
#>  5 High Slot      224       12    39          8        0.174                   5
#>  6 L Circle       228       12    28          5        0.123                   4
#>  7 L Corner         1       30     0          6        0                       6
#>  8 L Net Side      56       13     2         29        0.0357                 31
#>  9 L Point        149       12     7          4        0.047                   4
#> 10 Low Slot       559       19   112         11        0.200                   8
#> 11 Offensive N…    64        7     3          6        0.0469                 10
#> 12 Outside L      131       14     8          4        0.0611                  7
#> 13 Outside R       88       29     4         19        0.0455                 16
#> 14 R Circle       170       32    18         27        0.106                  15
#> 15 R Corner         2       20     0          3        0                       3
#> 16 R Net Side      50       16     4         16        0.08                   21
#> 17 R Point        123       27     0         31        0                      31
# }
```
