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

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_center_link | character | Link to the NHL game center page for the game. |
| save_pctg | numeric | Save percentage for the game. |
| game_date | character | Game date. |
| decision | character | Goalie decision (win, loss, or overtime/shootout loss). |
| player_on_home_team | logical | Whether the goalie played for the home team. |
| home_team_abbrev | character | Home team abbreviation. |
| home_team_common_name_default | character | Home team common name (default locale). |
| home_team_common_name_fr | character | Home team common name (French locale). |
| home_team_place_name_with_preposition_default | character | Home team place name with preposition (default locale). |
| home_team_place_name_with_preposition_fr | character | Home team place name with preposition (French locale). |
| home_team_team_logo_light | character | Home team light-mode logo URL. |
| home_team_team_logo_dark | character | Home team dark-mode logo URL. |
| away_team_abbrev | character | Away team abbreviation. |
| away_team_common_name_default | character | Away team common name (default locale). |
| away_team_place_name_with_preposition_default | character | Away team place name with preposition (default locale). |
| away_team_place_name_with_preposition_fr | character | Away team place name with preposition (French locale). |
| away_team_team_logo_light | character | Away team light-mode logo URL. |
| away_team_team_logo_dark | character | Away team dark-mode logo URL. |

Returns `NULL` on failure / empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_goalie_save_percentage_detail(player_id = 8475883))
#> ── NHL Edge Goalie Save Percentage Detail ───────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-30 03:09:18 UTC
#> # A tibble: 10 × 18
#>    game_center_link             save_pctg game_date decision player_on_home_team
#>    <chr>                            <dbl> <chr>     <chr>    <lgl>              
#>  1 /gamecenter/car-vs-mtl/2026…     0.958 2026-05-… W        TRUE               
#>  2 /gamecenter/mtl-vs-car/2026…     1     2026-05-… W        FALSE              
#>  3 /gamecenter/mtl-vs-car/2026…     0.846 2026-05-… W        FALSE              
#>  4 /gamecenter/car-vs-mtl/2026…     0.833 2026-05-… W        TRUE               
#>  5 /gamecenter/car-vs-mtl/2026…     0.762 2026-05-… L        TRUE               
#>  6 /gamecenter/phi-vs-car/2026…     0.882 2026-05-… W        FALSE              
#>  7 /gamecenter/phi-vs-car/2026…     0.947 2026-05-… W        FALSE              
#>  8 /gamecenter/car-vs-phi/2026…     0.944 2026-05-… W        TRUE               
#>  9 /gamecenter/car-vs-phi/2026…     1     2026-05-… W        TRUE               
#> 10 /gamecenter/ott-vs-car/2026…     0.926 2026-04-… W        FALSE              
#> # ℹ 13 more variables: home_team_abbrev <chr>,
#> #   home_team_common_name_default <chr>, home_team_common_name_fr <chr>,
#> #   home_team_place_name_with_preposition_default <chr>,
#> #   home_team_place_name_with_preposition_fr <chr>,
#> #   home_team_team_logo_light <chr>, home_team_team_logo_dark <chr>,
#> #   away_team_abbrev <chr>, away_team_common_name_default <chr>,
#> #   away_team_place_name_with_preposition_default <chr>, …
# }
```
