# **NHL Edge Team Skating Speed Detail**

Returns the NHL Edge skating-speed detail payload for a single team.
Wraps
`https://api-web.nhle.com/v1/edge/team-skating-speed-detail/{teamId}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_team_skating_speed_detail(team_id, season = NULL, game_type = 2)
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
| game_center_link | character | Link to the NHL game center page. |
| game_date | character | Game date. |
| game_type | integer | Game type code (2 = regular, 3 = playoffs). |
| is_home_team | logical | Whether the player's team was the home team. |
| time_in_period | character | Time in the period the top speed was recorded. |
| player_id | integer | Unique player identifier. |
| player_slug | character | Player URL slug. |
| player_first_name_default | character | Player first name. |
| player_last_name_default | character | Player last name. |
| skating_speed_imperial | numeric | Top skating speed in miles per hour. |
| skating_speed_metric | numeric | Top skating speed in kilometers per hour. |
| period_descriptor_number | integer | Period number. |
| period_descriptor_period_type | character | Period type (REG, OT, SO). |
| period_descriptor_max_regulation_periods | integer | Maximum number of regulation periods. |
| home_team_common_name_default | character | Home team common name. |
| home_team_place_name_with_preposition_default | character | Home team place name with preposition. |
| home_team_place_name_with_preposition_fr | character | Home team place name with preposition (French). |
| home_team_team_logo_light | character | Home team light logo URL. |
| home_team_team_logo_dark | character | Home team dark logo URL. |
| away_team_common_name_default | character | Away team common name. |
| away_team_place_name_with_preposition_default | character | Away team place name with preposition. |
| away_team_place_name_with_preposition_fr | character | Away team place name with preposition (French). |
| away_team_team_logo_light | character | Away team light logo URL. |
| away_team_team_logo_dark | character | Away team dark logo URL. |

## Examples

``` r
# \donttest{
  try(nhl_edge_team_skating_speed_detail(team_id = 10))
#> ── NHL Edge Team Skating Speed Detail ───────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-12 13:22:05 UTC
#> # A tibble: 10 × 24
#>    game_center_link    game_date game_type is_home_team time_in_period player_id
#>    <chr>               <chr>         <int> <lgl>        <chr>              <int>
#>  1 /gamecenter/sea-vs… 2025-10-…         2 TRUE         02:45            8477939
#>  2 /gamecenter/col-vs… 2026-01-…         2 TRUE         06:23            8477503
#>  3 /gamecenter/tor-vs… 2025-10-…         2 FALSE        01:05            8481122
#>  4 /gamecenter/tor-vs… 2026-01-…         2 FALSE        17:56            8481711
#>  5 /gamecenter/tor-vs… 2026-01-…         2 FALSE        12:30            8478904
#>  6 /gamecenter/pit-vs… 2025-11-…         2 TRUE         09:09            8476853
#>  7 /gamecenter/tor-vs… 2025-10-…         2 FALSE        04:11            8476931
#>  8 /gamecenter/cgy-vs… 2025-10-…         2 TRUE         10:15            8481582
#>  9 /gamecenter/tor-vs… 2026-01-…         2 FALSE        13:29            8475166
#> 10 /gamecenter/det-vs… 2025-10-…         2 TRUE         07:39            8481122
#> # ℹ 18 more variables: player_slug <chr>, player_first_name_default <chr>,
#> #   player_last_name_default <chr>, skating_speed_imperial <dbl>,
#> #   skating_speed_metric <dbl>, period_descriptor_number <int>,
#> #   period_descriptor_period_type <chr>,
#> #   period_descriptor_max_regulation_periods <int>,
#> #   home_team_common_name_default <chr>,
#> #   home_team_place_name_with_preposition_default <chr>, …
# }
```
