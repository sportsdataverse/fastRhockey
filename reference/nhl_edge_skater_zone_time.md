# **NHL Edge Skater Zone Time**

Returns the NHL Edge zone-time payload (offensive, defensive, neutral)
for a single skater. Wraps
`https://api-web.nhle.com/v1/edge/skater-zone-time/{playerId}/...`. When
`season` is `NULL` (default) the `/now` endpoint is used to fetch the
current season.

## Usage

``` r
nhl_edge_skater_zone_time(player_id, season = NULL, game_type = 2)
```

## Arguments

- player_id:

  Integer NHL player ID (e.g., `8478402` for Connor McDavid).

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
  try(nhl_edge_skater_zone_time(player_id = 8478402))
#> ── NHL Edge Skater Zone Time ────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 16:14:38 UTC
#> # A tibble: 4 × 10
#>   strength_code offensive_zone_pctg offensive_zone_percentile
#>   <chr>                       <dbl>                     <dbl>
#> 1 all                         0.456                     0.797
#> 2 es                          0.434                     0.75 
#> 3 pp                          0.644                     0.836
#> 4 pk                          0.280                     0.484
#> # ℹ 7 more variables: offensive_zone_league_avg <dbl>, neutral_zone_pctg <dbl>,
#> #   neutral_zone_percentile <dbl>, neutral_zone_league_avg <dbl>,
#> #   defensive_zone_pctg <dbl>, defensive_zone_percentile <dbl>,
#> #   defensive_zone_league_avg <dbl>
# }
```
