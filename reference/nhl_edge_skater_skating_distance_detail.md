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

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_center_link | character | Link to the NHL game center page. |
| game_date | character | Game date. |
| player_on_home_team | logical | Whether the player was on the home team. |
| toi_all | integer | Total time on ice across all strengths (seconds). |
| toi_even | integer | Time on ice at even strength (seconds). |
| toi_pk | integer | Time on ice on the penalty kill (seconds). |
| toi_pp | integer | Time on ice on the power play (seconds). |
| distance_skated_all_imperial | numeric | Distance skated at all strengths in miles. |
| distance_skated_all_metric | numeric | Distance skated at all strengths in kilometers. |
| distance_skated_even_imperial | numeric | Distance skated at even strength in miles. |
| distance_skated_even_metric | numeric | Distance skated at even strength in kilometers. |
| distance_skated_pk_imperial | numeric | Distance skated on the penalty kill in miles. |
| distance_skated_pk_metric | numeric | Distance skated on the penalty kill in kilometers. |
| home_team_abbrev | character | Home team abbreviation. |
| home_team_slug | character | Home team URL slug. |
| home_team_common_name_default | character | Home team common name. |
| home_team_place_name_with_preposition_default | character | Home team place name with preposition (English). |
| home_team_place_name_with_preposition_fr | character | Home team place name with preposition (French). |
| home_team_team_logo_light | character | Home team light logo URL. |
| home_team_team_logo_dark | character | Home team dark logo URL. |
| away_team_abbrev | character | Away team abbreviation. |
| away_team_slug | character | Away team URL slug. |
| away_team_common_name_default | character | Away team common name. |
| away_team_place_name_with_preposition_default | character | Away team place name with preposition (English). |
| away_team_place_name_with_preposition_fr | character | Away team place name with preposition (French). |
| away_team_team_logo_light | character | Away team light logo URL. |
| away_team_team_logo_dark | character | Away team dark logo URL. |
| distance_skated_pp_imperial | numeric | Distance skated on the power play in miles. |
| distance_skated_pp_metric | numeric | Distance skated on the power play in kilometers. |

## Examples

``` r
# \donttest{
  try(nhl_edge_skater_skating_distance_detail(player_id = 8478402))
#> ── NHL Edge Skater Skating Distance Detail ──────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-02 12:59:58 UTC
#> # A tibble: 10 × 29
#>    game_center_link game_date player_on_home_team toi_all toi_even toi_pp toi_pk
#>    <chr>            <chr>     <lgl>                 <int>    <int>  <int>  <int>
#>  1 /gamecenter/edm… 2026-11-… FALSE                  1402     1069    273     60
#>  2 /gamecenter/edm… 2026-10-… FALSE                  1400      788    462    150
#>  3 /gamecenter/cgy… 2026-10-… TRUE                   1130     1011     83     36
#>  4 /gamecenter/det… 2026-10-… TRUE                   1587     1128    301    158
#>  5 /gamecenter/tor… 2026-10-… TRUE                   1347     1103    230     14
#>  6 /gamecenter/car… 2026-10-… TRUE                   1502     1248    208     46
#>  7 /gamecenter/min… 2026-10-… TRUE                   1531     1037    380    114
#>  8 /gamecenter/edm… 2026-10-… FALSE                  1192     1068      8    116
#>  9 /gamecenter/uta… 2026-10-… TRUE                   1552     1341    127     84
#> 10 /gamecenter/edm… 2026-10-… FALSE                  1297     1057    140    100
#> # ℹ 22 more variables: distance_skated_all_imperial <dbl>,
#> #   distance_skated_all_metric <dbl>, distance_skated_even_imperial <dbl>,
#> #   distance_skated_even_metric <dbl>, distance_skated_pp_imperial <dbl>,
#> #   distance_skated_pp_metric <dbl>, distance_skated_pk_imperial <dbl>,
#> #   distance_skated_pk_metric <dbl>, home_team_abbrev <chr>,
#> #   home_team_slug <chr>, home_team_common_name_default <chr>,
#> #   home_team_place_name_with_preposition_default <chr>, …
# }
```
