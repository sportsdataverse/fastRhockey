# **NHL Edge Skater Skating Speed Detail**

Returns the NHL Edge skating-speed detail payload for a single skater
(velocity bursts, top speeds, etc.). Wraps
`https://api-web.nhle.com/v1/edge/skater-skating-speed-detail/{playerId}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_skater_skating_speed_detail(player_id, season = NULL, game_type = 2)
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

A `fastRhockey_data` tibble of skating-speed metrics, or `NULL` on
failure / empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_skater_skating_speed_detail(player_id = 8478402))
#> ── NHL Edge Skater Skating Speed Detail ─────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:04:56 UTC
#> # A tibble: 10 × 24
#>    game_center_link       game_date game_type player_on_home_team time_in_period
#>    <chr>                  <chr>         <int> <lgl>               <chr>         
#>  1 /gamecenter/cgy-vs-ed… 2025-10-…         2 TRUE                08:04         
#>  2 /gamecenter/sea-vs-ed… 2026-03-…         2 TRUE                14:12         
#>  3 /gamecenter/edm-vs-va… 2025-10-…         2 FALSE               13:51         
#>  4 /gamecenter/edm-vs-tb… 2025-11-…         2 FALSE               03:30         
#>  5 /gamecenter/wpg-vs-ed… 2025-12-…         2 TRUE                10:57         
#>  6 /gamecenter/wpg-vs-ed… 2025-12-…         2 TRUE                16:30         
#>  7 /gamecenter/min-vs-ed… 2026-01-…         2 TRUE                16:39         
#>  8 /gamecenter/edm-vs-la… 2026-04-…         2 FALSE               08:19         
#>  9 /gamecenter/stl-vs-ed… 2026-01-…         2 TRUE                09:34         
#> 10 /gamecenter/chi-vs-ed… 2025-11-…         2 TRUE                03:49         
#> # ℹ 19 more variables: skating_speed_imperial <dbl>,
#> #   skating_speed_metric <dbl>, period_descriptor_number <int>,
#> #   period_descriptor_period_type <chr>,
#> #   period_descriptor_max_regulation_periods <int>, home_team_abbrev <chr>,
#> #   home_team_slug <chr>, home_team_common_name_default <chr>,
#> #   home_team_place_name_with_preposition_default <chr>,
#> #   home_team_place_name_with_preposition_fr <chr>, …
# }
```
