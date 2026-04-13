# **NHL Edge Goalie Save Percentage Detail**

Returns the NHL Edge save-percentage detail payload for a single goalie.
Wraps
`https://api-web.nhle.com/v1/edge/goalie-save-percentage-detail/{playerId}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_goalie_save_percentage_detail(player_id, season = NULL, game_type = 2)
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

A `fastRhockey_data` tibble of save-percentage details, or `NULL` on
failure / empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_goalie_save_percentage_detail(player_id = 8475883))
#> ── NHL Edge Goalie Save Percentage Detail ───────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:04:39 UTC
#> # A tibble: 10 × 18
#>    game_center_link             save_pctg game_date decision player_on_home_team
#>    <chr>                            <dbl> <chr>     <chr>    <lgl>              
#>  1 /gamecenter/uta-vs-car/2026…     0.963 2026-04-… W        FALSE              
#>  2 /gamecenter/chi-vs-car/2026…     0.92  2026-04-… W        FALSE              
#>  3 /gamecenter/ott-vs-car/2026…     0.833 2026-04-… L        FALSE              
#>  4 /gamecenter/car-vs-cbj/2026…     0.9   2026-04-… W        TRUE               
#>  5 /gamecenter/car-vs-mtl/2026…     0.833 2026-03-… L        TRUE               
#>  6 /gamecenter/mtl-vs-car/2026…     0.778 2026-03-… L        FALSE              
#>  7 /gamecenter/pit-vs-car/2026…     0.947 2026-03-… W        FALSE              
#>  8 /gamecenter/car-vs-pit/2026…     0.857 2026-03-… W        TRUE               
#>  9 /gamecenter/tbl-vs-car/2026…     0.895 2026-03-… W        FALSE              
#> 10 /gamecenter/car-vs-pit/2026…     0.857 2026-03-… W        TRUE               
#> # ℹ 13 more variables: home_team_abbrev <chr>,
#> #   home_team_common_name_default <chr>, home_team_common_name_fr <chr>,
#> #   home_team_place_name_with_preposition_default <chr>,
#> #   home_team_place_name_with_preposition_fr <chr>,
#> #   home_team_team_logo_light <chr>, home_team_team_logo_dark <chr>,
#> #   away_team_abbrev <chr>, away_team_common_name_default <chr>,
#> #   away_team_place_name_with_preposition_default <chr>, …
# }
```
