# **NHL Edge Skater Distance Top 10**

Returns the NHL Edge top-10 "iron-man / mileage" skating distance
leaderboard for a given `positions`, `strength`, and `sort_by`
combination. Wraps
`https://api-web.nhle.com/v1/edge/skater-distance-top-10/{positions}/{strength}/{sortBy}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_skater_distance_top_10(
  positions,
  strength,
  sort_by,
  season = NULL,
  game_type = 2
)
```

## Arguments

- positions:

  Character positions filter accepted by the Edge API (e.g., `"F"` for
  forwards, `"D"` for defensemen).

- strength:

  Character strength filter accepted by the Edge API (e.g., `"all"`,
  `"ev"`, `"pp"`, `"sh"`).

- sort_by:

  Character sort-by key accepted by the Edge API (e.g., `"total"`).

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
| player_slug | character | URL-friendly player slug. |
| player_headshot | character | Player headshot image URL. |
| player_position | character | Player position. |
| player_sweater_number | integer | Jersey number. |
| player_first_name_default | character | Player first name (default locale). |
| player_first_name_cs | character | Player first name (Czech locale). |
| player_first_name_de | character | Player first name (German locale). |
| player_first_name_es | character | Player first name (Spanish locale). |
| player_first_name_fi | character | Player first name (Finnish locale). |
| player_first_name_sk | character | Player first name (Slovak locale). |
| player_first_name_sv | character | Player first name (Swedish locale). |
| player_last_name_default | character | Player last name (default locale). |
| player_last_name_cs | character | Player last name (Czech locale). |
| player_last_name_sk | character | Player last name (Slovak locale). |
| player_team_abbrev | character | Player team abbreviation. |
| player_team_slug | character | Player team URL-friendly slug. |
| player_team_common_name_default | character | Player team common name (default locale). |
| player_team_place_name_with_preposition_default | character | Player team place name with preposition (default locale). |
| player_team_place_name_with_preposition_fr | character | Player team place name with preposition (French locale). |
| player_team_team_logo_light | character | Player team light-mode logo URL. |
| player_team_team_logo_dark | character | Player team dark-mode logo URL. |
| distance_total_imperial | numeric | Total skating distance (imperial units). |
| distance_total_metric | numeric | Total skating distance (metric units). |
| distance_per60_imperial | numeric | Skating distance per 60 minutes (imperial units). |
| distance_per60_metric | numeric | Skating distance per 60 minutes (metric units). |
| distance_max_per_game_imperial | numeric | Maximum single-game skating distance (imperial units). |
| distance_max_per_game_metric | numeric | Maximum single-game skating distance (metric units). |
| distance_max_per_game_overlay_game_date | character | Game date of the max-per-game performance. |
| distance_max_per_game_overlay_game_type | integer | Game type of the max-per-game performance. |
| distance_max_per_game_overlay_player_first_name_default | character | Max-per-game player first name (default locale). |
| distance_max_per_game_overlay_player_first_name_cs | character | Max-per-game player first name (Czech locale). |
| distance_max_per_game_overlay_player_first_name_de | character | Max-per-game player first name (German locale). |
| distance_max_per_game_overlay_player_first_name_es | character | Max-per-game player first name (Spanish locale). |
| distance_max_per_game_overlay_player_first_name_fi | character | Max-per-game player first name (Finnish locale). |
| distance_max_per_game_overlay_player_first_name_sk | character | Max-per-game player first name (Slovak locale). |
| distance_max_per_game_overlay_player_first_name_sv | character | Max-per-game player first name (Swedish locale). |
| distance_max_per_game_overlay_player_last_name_default | character | Max-per-game player last name (default locale). |
| distance_max_per_game_overlay_player_last_name_cs | character | Max-per-game player last name (Czech locale). |
| distance_max_per_game_overlay_player_last_name_sk | character | Max-per-game player last name (Slovak locale). |
| distance_max_per_game_overlay_away_team_abbrev | character | Away team abbreviation in the max-per-game game. |
| distance_max_per_game_overlay_away_team_score | integer | Away team score in the max-per-game game. |
| distance_max_per_game_overlay_home_team_abbrev | character | Home team abbreviation in the max-per-game game. |
| distance_max_per_game_overlay_home_team_score | integer | Home team score in the max-per-game game. |
| distance_max_per_game_overlay_game_outcome_last_period_type | character | Last period type of the max-per-game game outcome. |
| distance_max_per_game_overlay_game_outcome_ot_periods | integer | Number of overtime periods in the max-per-game game. |
| distance_max_per_game_overlay_period_descriptor_max_regulation_periods | integer | Maximum regulation periods for the max-per-game game. |
| distance_max_per_game_overlay_period_descriptor_number | integer | Period number for the max-per-game descriptor. |
| distance_max_per_game_overlay_period_descriptor_period_type | character | Period type for the max-per-game descriptor. |
| distance_max_per_period_imperial | numeric | Maximum single-period skating distance (imperial units). |
| distance_max_per_period_metric | numeric | Maximum single-period skating distance (metric units). |
| distance_max_per_period_overlay_game_date | character | Game date of the max-per-period performance. |
| distance_max_per_period_overlay_game_type | integer | Game type of the max-per-period performance. |
| distance_max_per_period_overlay_player_first_name_default | character | Max-per-period player first name (default locale). |
| distance_max_per_period_overlay_player_first_name_cs | character | Max-per-period player first name (Czech locale). |
| distance_max_per_period_overlay_player_first_name_de | character | Max-per-period player first name (German locale). |
| distance_max_per_period_overlay_player_first_name_es | character | Max-per-period player first name (Spanish locale). |
| distance_max_per_period_overlay_player_first_name_fi | character | Max-per-period player first name (Finnish locale). |
| distance_max_per_period_overlay_player_first_name_sk | character | Max-per-period player first name (Slovak locale). |
| distance_max_per_period_overlay_player_first_name_sv | character | Max-per-period player first name (Swedish locale). |
| distance_max_per_period_overlay_player_last_name_default | character | Max-per-period player last name (default locale). |
| distance_max_per_period_overlay_player_last_name_cs | character | Max-per-period player last name (Czech locale). |
| distance_max_per_period_overlay_player_last_name_sk | character | Max-per-period player last name (Slovak locale). |
| distance_max_per_period_overlay_away_team_abbrev | character | Away team abbreviation in the max-per-period game. |
| distance_max_per_period_overlay_away_team_score | integer | Away team score in the max-per-period game. |
| distance_max_per_period_overlay_home_team_abbrev | character | Home team abbreviation in the max-per-period game. |
| distance_max_per_period_overlay_home_team_score | integer | Home team score in the max-per-period game. |
| distance_max_per_period_overlay_game_outcome_last_period_type | character | Last period type of the max-per-period game outcome. |
| distance_max_per_period_overlay_game_outcome_ot_periods | integer | Number of overtime periods in the max-per-period game. |
| distance_max_per_period_overlay_period_descriptor_max_regulation_periods | integer | Maximum regulation periods for the max-per-period game. |
| distance_max_per_period_overlay_period_descriptor_number | integer | Period number for the max-per-period descriptor. |
| distance_max_per_period_overlay_period_descriptor_period_type | character | Period type for the max-per-period descriptor. |

Returns `NULL` on failure / empty response.

## Examples

``` r
# \donttest{
  try(nhl_edge_skater_distance_top_10(
      positions = "F",
      strength = "all",
      sort_by = "total"
  ))
#> ── NHL Edge Skater Distance Top 10 ──────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-30 03:09:20 UTC
#> # A tibble: 10 × 71
#>    player_slug             player_headshot player_position player_sweater_number
#>    <chr>                   <chr>           <chr>                           <int>
#>  1 nick-suzuki-8480018     https://assets… C                                  14
#>  2 jack-eichel-8478403     https://assets… C                                   9
#>  3 juraj-slafkovský-84835… https://assets… L                                  20
#>  4 cole-caufield-8481540   https://assets… R                                  13
#>  5 mitch-marner-8478483    https://assets… R                                  93
#>  6 martin-necas-8480039    https://assets… C                                  88
#>  7 ivan-demidov-8484984    https://assets… R                                  93
#>  8 jake-evans-8478133      https://assets… C                                  71
#>  9 alex-newhook-8481618    https://assets… C                                  15
#> 10 nathan-mackinnon-84774… https://assets… C                                  29
#> # ℹ 67 more variables: player_first_name_default <chr>,
#> #   player_first_name_cs <chr>, player_first_name_de <chr>,
#> #   player_first_name_es <chr>, player_first_name_fi <chr>,
#> #   player_first_name_sk <chr>, player_first_name_sv <chr>,
#> #   player_last_name_default <chr>, player_last_name_cs <chr>,
#> #   player_last_name_sk <chr>, player_team_abbrev <chr>,
#> #   player_team_slug <chr>, player_team_common_name_default <chr>, …
# }
```
