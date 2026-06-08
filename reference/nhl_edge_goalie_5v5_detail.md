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

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_center_link | character | URL to the game center page. |
| save_pctg | numeric | Save percentage. |
| game_date | character | Game date. |
| decision | character | Goalie decision (W/L/OT). |
| player_on_home_team | logical | Whether the goalie played for the home team. |
| home_team_abbrev | character | Home team abbreviation. |
| home_team_slug | character | Home team slug. |
| home_team_common_name_default | character | Home team common name (default language). |
| home_team_common_name_fr | character | Home team common name (French). |
| home_team_place_name_with_preposition_default | character | Home team place name with preposition (def). |
| home_team_place_name_with_preposition_fr | character | Home team place name with preposition (FR). |
| home_team_team_logo_light | character | Home team logo URL (light variant). |
| home_team_team_logo_dark | character | Home team logo URL (dark variant). |
| away_team_abbrev | character | Away team abbreviation. |
| away_team_slug | character | Away team slug. |
| away_team_common_name_default | character | Away team common name (default language). |
| away_team_place_name_with_preposition_default | character | Away team place name with preposition (def). |
| away_team_place_name_with_preposition_fr | character | Away team place name with preposition (FR). |
| away_team_team_logo_light | character | Away team logo URL (light variant). |
| away_team_team_logo_dark | character | Away team logo URL (dark variant). |

## Examples

``` r
# \donttest{
  try(nhl_edge_goalie_5v5_detail(player_id = 8475883))
#> ── NHL Edge Goalie 5v5 Detail ───────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-08 11:44:00 UTC
#> # A tibble: 10 × 19
#>    game_center_link             save_pctg game_date player_on_home_team decision
#>    <chr>                            <dbl> <chr>     <lgl>               <chr>   
#>  1 /gamecenter/car-vs-vgk/2026…     0.786 2026-06-… FALSE               NA      
#>  2 /gamecenter/vgk-vs-car/2026…     0.913 2026-06-… TRUE                W       
#>  3 /gamecenter/vgk-vs-car/2026…     0.75  2026-06-… TRUE                L       
#>  4 /gamecenter/mtl-vs-car/2026…     1     2026-05-… TRUE                W       
#>  5 /gamecenter/car-vs-mtl/2026…     1     2026-05-… FALSE               W       
#>  6 /gamecenter/car-vs-mtl/2026…     0.909 2026-05-… FALSE               W       
#>  7 /gamecenter/mtl-vs-car/2026…     0.833 2026-05-… TRUE                W       
#>  8 /gamecenter/mtl-vs-car/2026…     0.733 2026-05-… TRUE                L       
#>  9 /gamecenter/car-vs-phi/2026…     0.867 2026-05-… FALSE               W       
#> 10 /gamecenter/car-vs-phi/2026…     1     2026-05-… FALSE               W       
#> # ℹ 14 more variables: home_team_abbrev <chr>, home_team_slug <chr>,
#> #   home_team_common_name_default <chr>,
#> #   home_team_place_name_with_preposition_default <chr>,
#> #   home_team_place_name_with_preposition_fr <chr>,
#> #   home_team_team_logo_light <chr>, home_team_team_logo_dark <chr>,
#> #   away_team_abbrev <chr>, away_team_slug <chr>,
#> #   away_team_common_name_default <chr>, …
# }
```
