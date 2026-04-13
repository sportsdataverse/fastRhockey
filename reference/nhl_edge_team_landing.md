# **NHL Edge Team Landing**

Returns the NHL Edge team landing payload with league-wide team metrics.
Wraps `https://api-web.nhle.com/v1/edge/team-landing/...`. When `season`
is `NULL` (default) the `/now` endpoint is used to fetch the current
season.

## Usage

``` r
nhl_edge_team_landing(season = NULL, game_type = 2)
```

## Arguments

- season:

  Optional 4-digit end-year (e.g., `2025` for the 2024-25 season), an
  8-character API season (e.g., `"20242025"`), or `NULL` (default) for
  the current season via the `/now` endpoint.

- game_type:

  Integer game type. 1 = preseason, 2 = regular season (default), 3 =
  playoffs.

## Value

A `fastRhockey_data` tibble of team landing metrics, or `NULL` on
failure / empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_team_landing())
#> ── NHL Edge Team Landing ────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:05 UTC
#> # A tibble: 5 × 2
#>         id game_types
#>      <int> <list>    
#> 1 20252026 <int [1]> 
#> 2 20242025 <int [2]> 
#> 3 20232024 <int [2]> 
#> 4 20222023 <int [2]> 
#> 5 20212022 <int [2]> 
# }
```
