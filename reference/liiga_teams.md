# **Liiga teams**

Team directory: every current and past team with logos, venue capacity
and club info.

## Usage

``` r
liiga_teams()
```

## Value

A `fastRhockey_data` tibble (14 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
| `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
| `name` | character | Club name as Liiga lists it, e.g. HIFK or K-Espoo. |
| `contact_info` | character | Club contact details as HTML, in Finnish (company, address, phone). |
| `externals` | character | JSON list of the club's external links (url, label), e.g. its ticket shops. |
| `country_code` | character | Club's country, 3-letter code (e.g. FIN). |
| `country_name` | character | Club's country name, e.g. Finland. |
| `current_venue_capacity` | integer | Capacity of the team's current home arena (spectators). |
| `general_info` | character | Club history and facts as an HTML table, in Finnish (founding year, titles, ...). |
| `url` | character | Club website URL. |
| `locality` | character | Club's home locality; null in every capture (unverified). |
| `logo` | character | Team logo image URL (old.liiga.fi media). |
| `short_name` | character | Team abbreviation. |
| `teamtournamentstats` | character | Team totals per season and tournament, snake_case, percentages and averages sent as STRINGS. |

## Details

Endpoint: `GET https://liiga.fi/api/v2/teams/info`

Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests
to liiga.fi are spaced at least one second apart. A request Liiga
answers with nothing (an unknown game, a tournament not played that
season, a shot map before 2016) raises `fastRhockey_no_data`; a failed
fetch or an error-shaped answer raises `fastRhockey_asset_fetch`; a bad
argument raises `fastRhockey_bad_request`.

## See also

Other Liiga Functions:
[`liiga_game()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game.md),
[`liiga_game_goals()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game_goals.md),
[`liiga_game_lineups()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game_lineups.md),
[`liiga_game_penalties()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game_penalties.md),
[`liiga_game_players_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game_players_stats.md),
[`liiga_game_shootout()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game_shootout.md),
[`liiga_game_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game_stats.md),
[`liiga_game_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_game_team_stats.md),
[`liiga_games()`](https://fastRhockey.sportsdataverse.org/reference/liiga_games.md),
[`liiga_gameweeks()`](https://fastRhockey.sportsdataverse.org/reference/liiga_gameweeks.md),
[`liiga_goals()`](https://fastRhockey.sportsdataverse.org/reference/liiga_goals.md),
[`liiga_player()`](https://fastRhockey.sportsdataverse.org/reference/liiga_player.md),
[`liiga_player_career()`](https://fastRhockey.sportsdataverse.org/reference/liiga_player_career.md),
[`liiga_player_game_log()`](https://fastRhockey.sportsdataverse.org/reference/liiga_player_game_log.md),
[`liiga_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_player_stats.md),
[`liiga_players_info()`](https://fastRhockey.sportsdataverse.org/reference/liiga_players_info.md),
[`liiga_schedule()`](https://fastRhockey.sportsdataverse.org/reference/liiga_schedule.md),
[`liiga_shotmap()`](https://fastRhockey.sportsdataverse.org/reference/liiga_shotmap.md),
[`liiga_standings()`](https://fastRhockey.sportsdataverse.org/reference/liiga_standings.md),
[`liiga_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_team_stats.md),
[`liiga_teams_history()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams_history.md),
[`liiga_tournament()`](https://fastRhockey.sportsdataverse.org/reference/liiga_tournament.md)

## Examples

``` r
# \donttest{
  try(liiga_teams())
#> ── Liiga teams Information from liiga.fi ───────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:14:17 UTC
#> # A tibble: 16 × 14
#>       team_id team_slug name    contact_info externals country_code country_name
#>         <int> <chr>     <chr>   <chr>        <chr>     <chr>        <chr>       
#>  1  495643563 karpat    Kärpät  "<p><strong… "[{\"url… FIN          Finland     
#>  2  933686567 saipa     SaiPa   "<p><strong… "[{\"url… FIN          Finland     
#>  3  679171680 assat     Ässät   "<p><strong… "[{\"url… FIN          Finland     
#>  4  859884935 kalpa     KalPa   "<p><strong… "[{\"url… FIN          Finland     
#>  5  362185137 tappara   Tappara "<p><strong… "[{\"url… FIN          Finland     
#>  6  461765763 kookoo    KooKoo  "<p><strong… "[{\"url… FIN          Finland     
#>  7  168761288 hifk      HIFK    "<p><strong… "[{\"url… FIN          Finland     
#>  8  219244634 jyp       JYP     "<p><strong… "[{\"url… FIN          Finland     
#>  9  951626834 ilves     Ilves   "<p><strong… "[{\"url… FIN          Finland     
#> 10  875886777 pelicans  Pelica… "<p><strong… "[{\"url… FIN          Finland     
#> 11  626537494 sport     Sport   "<p><strong… "[{\"url… FIN          Finland     
#> 12  292293444 jukurit   Jukurit "<p><strong… "[{\"url… FIN          Finland     
#> 13   55786244 hpk       HPK     "<p><strong… "[{\"url… FIN          Finland     
#> 14  624554857 lukko     Lukko   "<p><strong… "[{\"url… FIN          Finland     
#> 15  651304385 tps       TPS     "<p><strong… "[{\"url… FIN          Finland     
#> 16 1368624751 k-espoo   K-Espoo "<p><strong… "[{\"url… FIN          Finland     
#> # ℹ 7 more variables: current_venue_capacity <int>, general_info <chr>,
#> #   url <chr>, locality <chr>, logo <chr>, short_name <chr>,
#> #   teamtournamentstats <chr>
# }
```
