# **NHL Edge Goalie 5v5 Detail**

Returns the NHL Edge 5-on-5 advanced-metrics detail payload for a single
goalie. Wraps
`https://api-web.nhle.com/v1/edge/goalie-5v5-detail/{playerId}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_goalie_5v5_detail(player_id, season = NULL, game_type = 2)
```

## Arguments

- player_id:

  Integer NHL player ID (e.g., `8475883` for Andrei Vasilevskiy).

- season:

  Optional 4-digit end-year (e.g., `2025` for the 2024-25 season), an
  8-character API season (e.g., `"20242025"`), or `NULL` (default) for
  the current season via the `/now` endpoint.

- game_type:

  Integer game type. 1 = preseason, 2 = regular season (default), 3 =
  playoffs.

## Value

A `fastRhockey_data` tibble of 5-on-5 advanced metrics, or `NULL` on
failure / empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_goalie_5v5_detail(player_id = 8475883))
#> ── NHL Edge Goalie 5v5 Detail ───────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-11 16:02:20 UTC
#> # A tibble: 8 × 21
#>   game_center_link              save_pctg game_date decision player_on_home_team
#>   <chr>                             <dbl> <chr>     <chr>    <lgl>              
#> 1 /gamecenter/car-vs-phi/2026/…     0.867 2026-05-… W        FALSE              
#> 2 /gamecenter/car-vs-phi/2026/…     1     2026-05-… W        FALSE              
#> 3 /gamecenter/phi-vs-car/2026/…     0.966 2026-05-… W        TRUE               
#> 4 /gamecenter/phi-vs-car/2026/…     1     2026-05-… W        TRUE               
#> 5 /gamecenter/car-vs-ott/2026/…     1     2026-04-… W        FALSE              
#> 6 /gamecenter/car-vs-ott/2026/…     0.941 2026-04-… W        FALSE              
#> 7 /gamecenter/ott-vs-car/2026/…     0.946 2026-04-… W        TRUE               
#> 8 /gamecenter/ott-vs-car/2026/…     1     2026-04-… W        TRUE               
#> # ℹ 16 more variables: home_team_abbrev <chr>, home_team_slug <chr>,
#> #   home_team_common_name_default <chr>, home_team_common_name_fr <chr>,
#> #   home_team_place_name_with_preposition_default <chr>,
#> #   home_team_place_name_with_preposition_fr <chr>,
#> #   home_team_team_logo_light <chr>, home_team_team_logo_dark <chr>,
#> #   away_team_abbrev <chr>, away_team_slug <chr>,
#> #   away_team_common_name_default <chr>, away_team_common_name_fr <chr>, …
# }
```
