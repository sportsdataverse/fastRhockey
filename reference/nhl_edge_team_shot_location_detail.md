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

A `fastRhockey_data` tibble of shot-location metrics, or `NULL` on
failure / empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_team_shot_location_detail(team_id = 10))
#> ── NHL Edge Team Shot Location Detail ───────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:05 UTC
#> # A tibble: 17 × 7
#>    area           sog sog_rank goals goals_rank shooting_pctg shooting_pctg_rank
#>    <chr>        <int>    <int> <int>      <int>         <dbl>              <int>
#>  1 Behind the …     8       31     0         20        0                      20
#>  2 Beyond Red …    81        3     4         17        0.0494                 25
#>  3 Center Point   142       30     5         28        0.0352                 25
#>  4 Crease          76       17    18         20        0.237                  20
#>  5 High Slot      219       11    39          6        0.178                   5
#>  6 L Circle       220       17    27          6        0.123                   3
#>  7 L Corner         1       30     0          6        0                       6
#>  8 L Net Side      55       15     2         28        0.0364                 31
#>  9 L Point        146       12     7          4        0.0479                  4
#> 10 Low Slot       549       19   107         13        0.195                   8
#> 11 Offensive N…    63        7     3          6        0.0476                 10
#> 12 Outside L      130       13     8          4        0.0615                  7
#> 13 Outside R       85       30     4         19        0.0471                 17
#> 14 R Circle       167       32    18         26        0.108                  14
#> 15 R Corner         2       20     0          3        0                       3
#> 16 R Net Side      50       15     4         15        0.08                   19
#> 17 R Point        118       28     0         31        0                      31
# }
```
