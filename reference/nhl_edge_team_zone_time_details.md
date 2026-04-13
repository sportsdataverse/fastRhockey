# **NHL Edge Team Zone Time Details**

Returns the NHL Edge zone-time details payload for a single team. Wraps
`https://api-web.nhle.com/v1/edge/team-zone-time-details/{teamId}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_team_zone_time_details(team_id, season = NULL, game_type = 2)
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

A `fastRhockey_data` tibble of zone-time metrics, or `NULL` on failure /
empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_team_zone_time_details(team_id = 10))
#> ── NHL Edge Team Zone Time Details ──────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:13 UTC
#> # A tibble: 4 × 10
#>   strength_code offensive_zone_pctg offensive_zone_rank offensive_zone_league_…¹
#>   <chr>                       <dbl>               <int>                    <dbl>
#> 1 all                         0.390                  32                    0.411
#> 2 es                          0.388                  32                    0.407
#> 3 pp                          0.570                  24                    0.589
#> 4 pk                          0.258                  24                    0.268
#> # ℹ abbreviated name: ¹​offensive_zone_league_avg
#> # ℹ 6 more variables: neutral_zone_pctg <dbl>, neutral_zone_rank <int>,
#> #   neutral_zone_league_avg <dbl>, defensive_zone_pctg <dbl>,
#> #   defensive_zone_rank <int>, defensive_zone_league_avg <dbl>
# }
```
