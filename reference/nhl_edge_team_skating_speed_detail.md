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
#> ── NHL Edge Team Skating Speed Detail ──────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 05:49:35 UTC
#> # A tibble: 10 × 30
#>    game_center_link    game_date game_type is_home_team time_in_period player_id
#>    <chr>               <chr>         <int> <lgl>        <chr>              <int>
#>  1 /gamecenter/tor-vs… 2026-10-…         2 FALSE        02:05            8486067
#>  2 /gamecenter/tor-vs… 2026-10-…         2 FALSE        03:00            8479520
#>  3 /gamecenter/tor-vs… 2026-10-…         2 FALSE        04:17            8480893
#>  4 /gamecenter/buf-vs… 2026-10-…         2 TRUE         09:34            8479772
#>  5 /gamecenter/tor-vs… 2026-10-…         2 FALSE        03:13            8486067
#>  6 /gamecenter/tor-vs… 2026-10-…         2 FALSE        04:55            8478458
#>  7 /gamecenter/buf-vs… 2026-10-…         2 TRUE         19:31            8484158
#>  8 /gamecenter/nsh-vs… 2026-10-…         2 TRUE         19:38            8478458
#>  9 /gamecenter/nyi-vs… 2026-09-…         2 TRUE         02:54            8484158
#> 10 /gamecenter/tor-vs… 2026-10-…         2 FALSE        03:57            8484158
#> # ℹ 24 more variables: player_slug <chr>, player_first_name_default <chr>,
#> #   player_last_name_default <chr>, player_last_name_cs <chr>,
#> #   player_last_name_fi <chr>, player_last_name_sk <chr>,
#> #   player_last_name_de <chr>, player_last_name_es <chr>,
#> #   player_last_name_sv <chr>, skating_speed_imperial <dbl>,
#> #   skating_speed_metric <dbl>, period_descriptor_number <int>,
#> #   period_descriptor_period_type <chr>, …
# }
```
