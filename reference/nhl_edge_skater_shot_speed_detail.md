# **NHL Edge Skater Shot Speed Detail**

Returns the NHL Edge shot-speed detail payload for a single skater.
Wraps
`https://api-web.nhle.com/v1/edge/skater-shot-speed-detail/{playerId}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_skater_shot_speed_detail(player_id, season = NULL, game_type = 2)
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

A `fastRhockey_data` tibble of shot speeds, or `NULL` on failure / empty
response.

## Examples

``` r
# \donttest{
  try(nhl_edge_skater_shot_speed_detail(player_id = 8478402))
#> ── NHL Edge Skater Shot Speed Detail ────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-11 18:38:30 UTC
#> # A tibble: 10 × 24
#>    game_center_link       game_date game_type player_on_home_team time_in_period
#>    <chr>                  <chr>         <int> <lgl>               <chr>         
#>  1 /gamecenter/ana-vs-ed… 2026-04-…         3 FALSE               19:20         
#>  2 /gamecenter/ana-vs-ed… 2026-04-…         3 FALSE               13:49         
#>  3 /gamecenter/ana-vs-ed… 2026-04-…         3 FALSE               11:14         
#>  4 /gamecenter/edm-vs-an… 2026-04-…         3 FALSE               07:59         
#>  5 /gamecenter/edm-vs-an… 2026-04-…         3 FALSE               05:36         
#>  6 /gamecenter/edm-vs-an… 2026-04-…         3 FALSE               01:42         
#>  7 /gamecenter/edm-vs-an… 2026-04-…         3 FALSE               19:11         
#>  8 /gamecenter/edm-vs-an… 2026-04-…         3 FALSE               11:05         
#>  9 /gamecenter/edm-vs-an… 2026-04-…         3 FALSE               02:27         
#> 10 /gamecenter/edm-vs-an… 2026-04-…         3 FALSE               01:09         
#> # ℹ 19 more variables: shot_speed_imperial <dbl>, shot_speed_metric <dbl>,
#> #   period_descriptor_number <int>, period_descriptor_period_type <chr>,
#> #   period_descriptor_max_regulation_periods <int>, home_team_abbrev <chr>,
#> #   home_team_slug <chr>, home_team_common_name_default <chr>,
#> #   home_team_place_name_with_preposition_default <chr>,
#> #   home_team_place_name_with_preposition_fr <chr>,
#> #   home_team_team_logo_light <chr>, home_team_team_logo_dark <chr>, …
# }
```
