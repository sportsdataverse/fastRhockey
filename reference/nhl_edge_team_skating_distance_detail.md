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

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_center_link | character | Link to the NHL game center page for the game. |
| game_date | character | Game date. |
| is_home_team | logical | Whether the team was the home team. |
| toi_all | integer | Time on ice (all situations), in seconds. |
| toi_even | integer | Time on ice (even strength), in seconds. |
| toi_pp | integer | Time on ice (power play), in seconds. |
| toi_pk | integer | Time on ice (penalty kill), in seconds. |
| distance_skated_all_imperial | numeric | Distance skated (all situations), in miles. |
| distance_skated_all_metric | numeric | Distance skated (all situations), in kilometers. |
| distance_skated_even_imperial | numeric | Distance skated (even strength), in miles. |
| distance_skated_even_metric | numeric | Distance skated (even strength), in kilometers. |
| distance_skated_pp_imperial | numeric | Distance skated (power play), in miles. |
| distance_skated_pp_metric | numeric | Distance skated (power play), in kilometers. |
| distance_skated_pk_imperial | numeric | Distance skated (penalty kill), in miles. |
| distance_skated_pk_metric | numeric | Distance skated (penalty kill), in kilometers. |
| home_team_common_name_default | character | Home team common name (default language). |
| home_team_common_name_fr | character | Home team common name (French). |
| home_team_place_name_with_preposition_default | character | Home team place name with preposition (default). |
| home_team_place_name_with_preposition_fr | character | Home team place name with preposition (French). |
| home_team_team_logo_light | character | URL to the home team light logo. |
| home_team_team_logo_dark | character | URL to the home team dark logo. |
| away_team_common_name_default | character | Away team common name (default language). |
| away_team_place_name_with_preposition_default | character | Away team place name with preposition (default). |
| away_team_place_name_with_preposition_fr | character | Away team place name with preposition (French). |
| away_team_team_logo_light | character | URL to the away team light logo. |
| away_team_team_logo_dark | character | URL to the away team dark logo. |

## Examples

``` r
# \donttest{
  try(nhl_edge_team_skating_distance_detail(team_id = 10))
#> ── NHL Edge Team Skating Distance Detail ────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-25 06:33:43 UTC
#> # A tibble: 10 × 25
#>    game_center_link        game_date is_home_team toi_all toi_even toi_pp toi_pk
#>    <chr>                   <chr>     <lgl>          <int>    <int>  <int>  <int>
#>  1 /gamecenter/tor-vs-nyi… 2026-11-… FALSE          17930    17390    300    240
#>  2 /gamecenter/cbj-vs-tor… 2026-11-… TRUE           17806    15022   1560   1224
#>  3 /gamecenter/lak-vs-tor… 2026-11-… TRUE           18748    16021   1955    772
#>  4 /gamecenter/tor-vs-chi… 2026-11-… FALSE          17699    14635   1800   1264
#>  5 /gamecenter/edm-vs-tor… 2026-11-… TRUE           17988    16908    600    480
#>  6 /gamecenter/min-vs-tor… 2026-11-… TRUE           17879    15483   1200   1196
#>  7 /gamecenter/col-vs-tor… 2026-11-… TRUE           17770    16210    600    960
#>  8 /gamecenter/njd-vs-tor… 2026-11-… TRUE           17647    16415     NA   1232
#>  9 /gamecenter/mtl-vs-tor… 2026-11-… TRUE           17986    15050   2304    632
#> 10 /gamecenter/uta-vs-tor… 2026-11-… TRUE           17831    15595   1200   1036
#> # ℹ 18 more variables: distance_skated_all_imperial <dbl>,
#> #   distance_skated_all_metric <dbl>, distance_skated_even_imperial <dbl>,
#> #   distance_skated_even_metric <dbl>, distance_skated_pp_imperial <dbl>,
#> #   distance_skated_pp_metric <dbl>, distance_skated_pk_imperial <dbl>,
#> #   distance_skated_pk_metric <dbl>, home_team_common_name_default <chr>,
#> #   home_team_place_name_with_preposition_default <chr>,
#> #   home_team_place_name_with_preposition_fr <chr>, …
# }
```
