# **NHL Edge Skater Shot Speed Detail**

Returns the NHL Edge shot-speed detail payload for a single skater.
Wraps
`https://api-web.nhle.com/v1/edge/skater-shot-speed-detail/{playerId}/...`.
When `season` is `NULL` (default) the `/now` endpoint is used to fetch
the current season.

## Usage

``` r
nhl_edge_skater_shot_speed_detail(player_id, season = NULL, game_type = 2)
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
| game_type | integer | Game type (1 = preseason, 2 = regular, 3 = playoffs). |
| player_on_home_team | logical | Whether the player was on the home team. |
| time_in_period | character | Time within the period of the shot. |
| shot_speed_imperial | numeric | Shot speed in miles per hour. |
| shot_speed_metric | numeric | Shot speed in kilometers per hour. |
| period_descriptor_number | integer | Period number. |
| period_descriptor_period_type | character | Period type (e.g., REG, OT). |
| period_descriptor_max_regulation_periods | integer | Maximum number of regulation periods. |
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

## Examples

``` r
# \donttest{
  try(nhl_edge_skater_shot_speed_detail(player_id = 8478402))
#> ── NHL Edge Skater Shot Speed Detail ────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-18 18:47:48 UTC
#> # A tibble: 10 × 24
#>    game_center_link       game_date game_type player_on_home_team time_in_period
#>    <chr>                  <chr>         <int> <lgl>               <chr>         
#>  1 /gamecenter/edm-vs-cb… 2025-11-…         2 FALSE               00:39         
#>  2 /gamecenter/vgk-vs-ed… 2025-12-…         2 FALSE               08:39         
#>  3 /gamecenter/ana-vs-ed… 2026-03-…         2 FALSE               06:57         
#>  4 /gamecenter/nsh-vs-ed… 2026-01-…         2 FALSE               05:18         
#>  5 /gamecenter/edm-vs-st… 2026-03-…         2 FALSE               09:56         
#>  6 /gamecenter/sjs-vs-ed… 2026-01-…         2 FALSE               07:36         
#>  7 /gamecenter/stl-vs-ed… 2026-01-…         2 FALSE               07:24         
#>  8 /gamecenter/cgy-vs-ed… 2025-12-…         2 FALSE               09:12         
#>  9 /gamecenter/edm-vs-da… 2025-11-…         2 FALSE               04:13         
#> 10 /gamecenter/edm-vs-ws… 2025-11-…         2 FALSE               07:13         
#> # ℹ 19 more variables: shot_speed_imperial <dbl>, shot_speed_metric <dbl>,
#> #   period_descriptor_number <int>, period_descriptor_period_type <chr>,
#> #   period_descriptor_max_regulation_periods <int>, home_team_abbrev <chr>,
#> #   home_team_slug <chr>, home_team_common_name_default <chr>,
#> #   home_team_place_name_with_preposition_default <chr>,
#> #   home_team_place_name_with_preposition_fr <chr>,
#> #   home_team_team_logo_light <chr>, home_team_team_logo_dark <chr>, …
# }
```
