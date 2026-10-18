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
#> 2026-10-18 06:16:11.479793: No NHL Edge team skating distance detail returned for team_id=10
#> NULL
# }
```
