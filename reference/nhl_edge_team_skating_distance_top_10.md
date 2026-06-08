# **NHL Edge Team Skating Distance Top 10**

Returns the NHL Edge top-10 team skating-distance leaderboard. Wraps
`https://api-web.nhle.com/v1/edge/team-skating-distance-top-10/{positions}/{strength}/{sortBy}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_team_skating_distance_top_10(
  positions,
  strength,
  sort_by,
  season = NULL,
  game_type = 2
)
```

## Arguments

- positions:

  Character position filter (e.g., `"F"`, `"D"`, `"all"`).

- strength:

  Character strength state (e.g., `"all"`, `"ev"`, `"pp"`, `"pk"`).

- sort_by:

  Character metric to sort the leaderboard by (e.g., `"total"`,
  `"per_60"`).

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
| team_abbrev | character | Team abbreviation. |
| team_slug | character | URL slug for the team. |
| team_common_name_default | character | Team common name (default language). |
| team_place_name_with_preposition_default | character | Team place name with preposition (default language). |
| team_team_logo_light | character | URL to the team light logo. |
| team_team_logo_dark | character | URL to the team dark logo. |
| distance_total_imperial | numeric | Total distance skated, in miles. |
| distance_total_metric | numeric | Total distance skated, in kilometers. |
| distance_per60_imperial | numeric | Distance skated per 60 minutes, in miles. |
| distance_per60_metric | numeric | Distance skated per 60 minutes, in kilometers. |
| distance_max_per_game_imperial | numeric | Maximum distance skated in a single game, in miles. |
| distance_max_per_game_metric | numeric | Maximum distance skated in a single game, in kilometers. |
| distance_max_per_game_overlay_game_date | character | Game date of the max-per-game performance. |
| distance_max_per_game_overlay_game_type | integer | Game type of the max-per-game performance. |
| distance_max_per_game_overlay_away_team_abbrev | character | Away team abbreviation in the max-per-game game. |
| distance_max_per_game_overlay_away_team_score | integer | Away team score in the max-per-game game. |
| distance_max_per_game_overlay_home_team_abbrev | character | Home team abbreviation in the max-per-game game. |
| distance_max_per_game_overlay_home_team_score | integer | Home team score in the max-per-game game. |
| distance_max_per_game_overlay_game_outcome_last_period_type | character | Last period type of the max-per-game game outcome. |
| distance_max_per_game_overlay_game_outcome_ot_periods | integer | Number of overtime periods in the max-per-game game. |
| distance_max_per_game_overlay_period_descriptor_max_regulation_periods | integer | Maximum regulation periods for the max-per-game game. |
| distance_max_per_game_overlay_period_descriptor_number | integer | Period number for the max-per-game game descriptor. |
| distance_max_per_game_overlay_period_descriptor_period_type | character | Period type for the max-per-game game descriptor. |
| distance_max_per_period_imperial | numeric | Maximum distance skated in a single period, in miles. |
| distance_max_per_period_metric | numeric | Maximum distance skated in a single period, in kilometers. |
| distance_max_per_period_overlay_game_date | character | Game date of the max-per-period performance. |
| distance_max_per_period_overlay_game_type | integer | Game type of the max-per-period performance. |
| distance_max_per_period_overlay_away_team_abbrev | character | Away team abbreviation in the max-per-period game. |
| distance_max_per_period_overlay_away_team_score | integer | Away team score in the max-per-period game. |
| distance_max_per_period_overlay_home_team_abbrev | character | Home team abbreviation in the max-per-period game. |
| distance_max_per_period_overlay_home_team_score | integer | Home team score in the max-per-period game. |
| distance_max_per_period_overlay_game_outcome_last_period_type | character | Last period type of the max-per-period game outcome. |
| distance_max_per_period_overlay_game_outcome_ot_periods | integer | Number of overtime periods in the max-per-period game. |
| distance_max_per_period_overlay_period_descriptor_max_regulation_periods | integer | Maximum regulation periods for the max-per-period game. |
| distance_max_per_period_overlay_period_descriptor_number | integer | Period number for the max-per-period game descriptor. |
| distance_max_per_period_overlay_period_descriptor_period_type | character | Period type for the max-per-period game descriptor. |

## Examples

``` r
# \donttest{
  try(nhl_edge_team_skating_distance_top_10(
    positions = "F",
    strength = "all",
    sort_by = "total"
  ))
#> ── NHL Edge Team Skating Distance Top 10 ────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-08 11:44:14 UTC
#> # A tibble: 10 × 36
#>    team_abbrev team_slug           team_common_name_def…¹ team_place_name_with…²
#>    <chr>       <chr>               <chr>                  <chr>                 
#>  1 VGK         vegas-golden-knigh… Golden Knights         Vegas                 
#>  2 MTL         montreal-canadiens… Canadiens              Montréal              
#>  3 CAR         carolina-hurricane… Hurricanes             Carolina              
#>  4 COL         colorado-avalanche… Avalanche              Colorado              
#>  5 BUF         buffalo-sabres-7    Sabres                 Buffalo               
#>  6 ANA         anaheim-ducks-24    Ducks                  Anaheim               
#>  7 MIN         minnesota-wild-30   Wild                   Minnesota             
#>  8 PHI         philadelphia-flyer… Flyers                 Philadelphia          
#>  9 TBL         tampa-bay-lightnin… Lightning              Tampa Bay             
#> 10 UTA         utah-mammoth-68     Mammoth                Utah                  
#> # ℹ abbreviated names: ¹​team_common_name_default,
#> #   ²​team_place_name_with_preposition_default
#> # ℹ 32 more variables: team_team_logo_light <chr>, team_team_logo_dark <chr>,
#> #   distance_total_imperial <dbl>, distance_total_metric <dbl>,
#> #   distance_per60_imperial <dbl>, distance_per60_metric <dbl>,
#> #   distance_max_per_game_imperial <dbl>, distance_max_per_game_metric <dbl>,
#> #   distance_max_per_game_overlay_game_date <chr>, …
# }
```
