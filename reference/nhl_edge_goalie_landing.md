# **NHL Edge Goalie Landing**

Returns the NHL Edge goalie landing-page payload. Wraps
`https://api-web.nhle.com/v1/edge/goalie-landing/...`. When `season` is
`NULL` (default) the `/now` endpoint is used to fetch the current
season.

## Usage

``` r
nhl_edge_goalie_landing(season = NULL, game_type = 2)
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

A `fastRhockey_data` tibble containing the goalie landing payload, or
`NULL` on failure / empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_goalie_landing())
#> ── NHL Edge Goalie Landing ──────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:04:39 UTC
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
