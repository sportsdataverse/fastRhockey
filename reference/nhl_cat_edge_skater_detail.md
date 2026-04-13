# **NHL CAT Edge Skater Detail**

Returns the categorical (CAT) variant of the NHL Edge advanced-metrics
detail payload for a single skater. Wraps
`https://api-web.nhle.com/v1/cat/edge/skater-detail/{playerId}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_cat_edge_skater_detail(player_id, season = NULL, game_type = 2)
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

A `fastRhockey_data` tibble of categorical advanced metrics, or `NULL`
on failure / empty response.

## Examples

``` r
# \donttest{
  try(nhl_cat_edge_skater_detail(player_id = 8478402))
#> ── NHL CAT Edge Skater Detail ───────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:04:24 UTC
#> # A tibble: 5 × 2
#>         id game_types
#>      <int> <list>    
#> 1 20212022 <int [2]> 
#> 2 20222023 <int [2]> 
#> 3 20232024 <int [2]> 
#> 4 20242025 <int [2]> 
#> 5 20252026 <int [1]> 
# }
```
