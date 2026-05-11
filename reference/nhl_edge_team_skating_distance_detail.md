# **NHL Edge Team Skating Distance Detail**

Returns the NHL Edge skating-distance detail payload for a single team.
Wraps
`https://api-web.nhle.com/v1/edge/team-skating-distance-detail/{teamId}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_team_skating_distance_detail(team_id, season = NULL, game_type = 2)
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

A `fastRhockey_data` tibble of skating-distance metrics, or `NULL` on
failure / empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_team_skating_distance_detail(team_id = 10))
#> ── NHL Edge Team Skating Distance Detail ────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-11 16:20:20 UTC
#> # A tibble: 10 × 26
#>    game_center_link        game_date is_home_team toi_all toi_even toi_pp toi_pk
#>    <chr>                   <chr>     <lgl>          <int>    <int>  <int>  <int>
#>  1 /gamecenter/tor-vs-ott… 2026-04-… FALSE          17858    15642   1200   1016
#>  2 /gamecenter/dal-vs-tor… 2026-04-… TRUE           17714    15425    925   1364
#>  3 /gamecenter/fla-vs-tor… 2026-04-… TRUE           18005    17420    105    480
#>  4 /gamecenter/tor-vs-nyi… 2026-04-… FALSE          17693    15523    335   1835
#>  5 /gamecenter/wsh-vs-tor… 2026-04-… TRUE           17781    15106   1715    960
#>  6 /gamecenter/tor-vs-lak… 2026-04-… FALSE          17635    13775   1460   2400
#>  7 /gamecenter/tor-vs-sjs… 2026-04-… FALSE          17923    16623    340    960
#>  8 /gamecenter/tor-vs-ana… 2026-03-… FALSE          18245    15382   1195   1668
#>  9 /gamecenter/tor-vs-stl… 2026-03-… FALSE          17830    15326   1800    704
#> 10 /gamecenter/nyr-vs-tor… 2026-03-… TRUE           17780    15700   1200    880
#> # ℹ 19 more variables: distance_skated_all_imperial <dbl>,
#> #   distance_skated_all_metric <dbl>, distance_skated_even_imperial <dbl>,
#> #   distance_skated_even_metric <dbl>, distance_skated_pp_imperial <dbl>,
#> #   distance_skated_pp_metric <dbl>, distance_skated_pk_imperial <dbl>,
#> #   distance_skated_pk_metric <dbl>, home_team_common_name_default <chr>,
#> #   home_team_common_name_fr <chr>,
#> #   home_team_place_name_with_preposition_default <chr>, …
# }
```
