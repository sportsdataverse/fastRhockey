# **NHL Edge Skater Skating Distance Detail**

Returns the NHL Edge skating-distance detail payload for a single skater
(distance covered, strides, etc.). Wraps
`https://api-web.nhle.com/v1/edge/skater-skating-distance-detail/{playerId}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_skater_skating_distance_detail(
  player_id,
  season = NULL,
  game_type = 2
)
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

A `fastRhockey_data` tibble of skating-distance metrics, or `NULL` on
failure / empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_skater_skating_distance_detail(player_id = 8478402))
#> ── NHL Edge Skater Skating Distance Detail ──────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:04:56 UTC
#> # A tibble: 10 × 29
#>    game_center_link game_date player_on_home_team toi_all toi_even toi_pp toi_pk
#>    <chr>            <chr>     <lgl>                 <int>    <int>  <int>  <int>
#>  1 /gamecenter/edm… 2026-04-… FALSE                  1610     1493    117     NA
#>  2 /gamecenter/edm… 2026-04-… FALSE                  1270     1028    205     37
#>  3 /gamecenter/edm… 2026-04-… FALSE                  1325     1115    178     32
#>  4 /gamecenter/vgk… 2026-04-… TRUE                   1405     1067    338     NA
#>  5 /gamecenter/chi… 2026-04-… TRUE                   1344     1041    303     NA
#>  6 /gamecenter/sea… 2026-03-… TRUE                   1186     1064    106     16
#>  7 /gamecenter/ana… 2026-03-… TRUE                   1349     1032    313      4
#>  8 /gamecenter/edm… 2026-03-… FALSE                  1332     1160    160     12
#>  9 /gamecenter/edm… 2026-03-… FALSE                  1242     1184     NA     58
#> 10 /gamecenter/tbl… 2026-03-… TRUE                   1362     1229    131      2
#> # ℹ 22 more variables: distance_skated_all_imperial <dbl>,
#> #   distance_skated_all_metric <dbl>, distance_skated_even_imperial <dbl>,
#> #   distance_skated_even_metric <dbl>, distance_skated_pp_imperial <dbl>,
#> #   distance_skated_pp_metric <dbl>, home_team_abbrev <chr>,
#> #   home_team_slug <chr>, home_team_common_name_default <chr>,
#> #   home_team_place_name_with_preposition_default <chr>,
#> #   home_team_place_name_with_preposition_fr <chr>, …
# }
```
