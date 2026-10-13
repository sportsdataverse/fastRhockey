# **HockeyAllsvenskan Player leaderboard**

Current-season top 25 of one metric, with the StatNet player id (the
only stats route that has one).

The current season's top 25 of one metric. The only stats route with a
player id: `player_statnet_id` (StatNet player id). `team_statnet_id` is
the StatNet team id and `team_code` the display code; `metric_value`
stays character (a count for `TP`, a clock for `TOI/GP`). The call is
refused as truncated unless the board is one complete page.

## Usage

``` r
ha_leaders(
  metric,
  phase = "HA",
  team = NULL,
  is_goalkeeper = NULL,
  return_parsed = TRUE
)
```

## Arguments

- metric:

  Leaderboard metric. Seen on the site: TP, G, A, PlusMinus, Hits,
  FOPerc, TOI/GP (skaters).

- phase:

  Game type: HA regular season, HAPlayoff playoffs, HAkval playout
  (relegation series). Allowed: `HA`, `HAPlayoff`, `HAkval`.

- team:

  Team filter; omitted = the whole league (values not probed). `NULL` =
  not sent.

- is_goalkeeper:

  True for a goalkeeper metric. `NULL` = not sent.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the
  raw JSON as a list (stamped with the request under `_request` where
  the body does not say what it answers).

## Value

A tibble (`return_parsed = TRUE`), else the raw JSON list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| rank | integer | Rank in the table. |
| player_name | character | Player name (the table carries no player id). |
| team_code | character | Display code of the team (AIK, MoDo, MORA, NVIF, ÖIK), not the StatNet id. |
| team_statnet_id | character | StatNet team id (AIK, MODO, MIK, NYB, OSIK), not the display code. |
| jersey_number | integer | Jersey number of the player. |
| position | character | Playing position: F forward, D defence (G goalkeeper on goalie metrics). |
| metric | character | Metric code (echo of the request metric). |
| metric_value | character | Value of the metric (a count, a percentage or a clock, as text). |
| player_statnet_id | integer | StatNet player id (the same id space as SHL's isa id, verified on one player). |
| first_name | character | First name of the player. |
| family_name | character | Family name of the player. |
| slug | character | Player slug; joins ha_players. |
| headshot | character | Headshot URL (medium rendition). |
| headshot_small | character | Small headshot URL. |
| headshot_medium | character | Medium headshot URL. |
| team_logo | character | Logo URL of the player's team. |
| team_name | character | Team label as shown on the card (the display team code). |
| team_main_color | character | Team primary colour (hex). |

## Errors

`fastRhockey_no_data`: the site has nothing for the request (a season
before 2005/06, an unknown or pre-2025/26 game, a phase not played, a
body without rows). `fastRhockey_bad_request`: a value outside its
enumerated set, a field the site requires is missing, or HTTP 400/422.
`fastRhockey_asset_fetch`: any other non-2xx, a connection failure after
retries, or an empty, non-JSON, error-shaped, truncated or out-of-scope
200 (the answer is unknown, not empty).

## See also

Other HA Functions:
[`ha_game()`](https://fastRhockey.sportsdataverse.org/reference/ha_game.md),
[`ha_games()`](https://fastRhockey.sportsdataverse.org/reference/ha_games.md),
[`ha_goalie_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_goalie_stats.md),
[`ha_pbp()`](https://fastRhockey.sportsdataverse.org/reference/ha_pbp.md),
[`ha_players()`](https://fastRhockey.sportsdataverse.org/reference/ha_players.md),
[`ha_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ha_schedule.md),
[`ha_skater_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_skater_stats.md),
[`ha_standings()`](https://fastRhockey.sportsdataverse.org/reference/ha_standings.md),
[`ha_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_team_stats.md),
[`ha_teams()`](https://fastRhockey.sportsdataverse.org/reference/ha_teams.md)

## Examples

``` r
# \donttest{
  try(ha_leaders(metric = "TP"))
#> ── HA Leaders Information from hockeyallsvenskan.se ── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-13 01:54:12 UTC
#> # A tibble: 25 × 18
#>     rank player_name     team_code team_statnet_id jersey_number position metric
#>    <int> <chr>           <chr>     <chr>                   <int> <chr>    <chr> 
#>  1     1 Tyler Kelleher  LIF       LIF                        16 F        TP    
#>  2     2 Joel Svensson   IKO       IKO                        13 F        TP    
#>  3     3 Calle Karlsson  IKO       IKO                        14 F        TP    
#>  4     4 Carter Souch    SSK       SSK                        18 F        TP    
#>  5     5 Åke Stakkestad  LIF       LIF                        15 F        TP    
#>  6     6 Tim Theocharid… AIK       AIK                        98 D        TP    
#>  7     7 Patrik Zackris… LIF       LIF                         9 F        TP    
#>  8     8 Giorgio Esteph… VIS       VIS                        91 F        TP    
#>  9     9 Hugo Lejon      VHC       VHC                        58 F        TP    
#> 10    10 Joshua Passolt  LIF       LIF                        12 F        TP    
#> # ℹ 15 more rows
#> # ℹ 11 more variables: metric_value <chr>, player_statnet_id <int>,
#> #   first_name <chr>, family_name <chr>, slug <chr>, headshot <chr>,
#> #   headshot_small <chr>, headshot_medium <chr>, team_logo <chr>,
#> #   team_name <chr>, team_main_color <chr>
# }
```
