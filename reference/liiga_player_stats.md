# **Liiga player stat tables**

Player stat table summed over a season range, by data_type (the whole
league, unfiltered).

## Usage

``` r
liiga_player_stats(
  season_from,
  season_to,
  tournament,
  data_type = "basicStats"
)
```

## Arguments

- season_from:

  First season (END year) of the range, e.g. 2026.

- season_to:

  Last season (END year) of the range, e.g. 2026.

- tournament:

  Tournament: runkosarja (regular season), playoffs, playout,
  qualifications, valmistavat_ottelut (preseason) or chl (Champions
  Hockey League).

- data_type:

  Stat table: basicStats, basicStatsGk, advancedStats, goalStats,
  shotStats, skatingStats, faceOffStats, penaltyStats,
  powerplayPenaltykillStats, passes, winningShotComp, winningShotCompGk,
  gameTime.

## Value

A `fastRhockey_data` tibble (45 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `player_id` | integer | Player id; the same number in lineups, rosters, stat tables and the profile (fihaId). |
| `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
| `team_name` | character | Team's display name, e.g. HPK or Tappara. |
| `team_short_name` | character | Team abbreviation. |
| `role` | character | Position: CENTER, LEFT_WING, RIGHT_WING, LEFT_DEFENSEMAN, RIGHT_DEFENSEMAN, SEVENTH_DEFENSEMAN, GOALIE. |
| `first_name` | character | Player's first name. |
| `last_name` | character | Player's last name. |
| `nationality` | character | Nationality, 3-letter code. |
| `tournament` | character | Tournament of the summed row as Liiga labels it, e.g. regular. |
| `picture_url` | character | URL of the player's photo. |
| `previous_teams_for_tournament` | character | Earlier teams of a player traded during the tournament. |
| `injured` | logical | True when the player is listed as injured. |
| `jersey` | integer | Jersey (sweater) number the player wore. |
| `last_season` | integer | Last season (END year) of the range in which the player appears. |
| `goalkeeper` | logical | True when the player is a goalkeeper. |
| `games` | integer | Games (player, summed over the season range). |
| `played_games` | integer | Played games (player, summed over the season range). |
| `rookie` | logical | True when Liiga flags the player as a rookie. |
| `suspended` | logical | True when the player is listed as suspended. |
| `removed` | logical | Removed from the roster. |
| `time_on_ice` | integer | Time on ice (player, summed over the season range). Seconds. |
| `current` | logical | Player is on the team now. |
| `is_u20` | logical | True when Liiga flags the player as under 20 (U20). |
| `golden_helmets` | integer | Golden helmets (Liiga's team points-leader helmet; reading unverified), summed over the season range. |
| `alumni_points` | integer | Alumni points (meaning undocumented by Liiga), summed over the season range. |
| `goals` | integer | Goals (player, summed over the season range). |
| `assists` | integer | Assists (player, summed over the season range). |
| `points` | integer | Points (player, summed over the season range). |
| `plus` | integer | On ice for goals for (the + of plus/minus). |
| `minus` | integer | On ice for goals against (the - of plus/minus). |
| `plus_minus` | integer | Plus/minus: on-ice goals for minus on-ice goals against. |
| `penalty_minutes` | integer | Penalty minutes (player, summed over the season range). |
| `powerplay_goals` | integer | Powerplay goals (player, summed over the season range). |
| `penaltykill_goals` | integer | Penaltykill goals (player, summed over the season range). |
| `winning_goals` | integer | Winning goals (player, summed over the season range). |
| `shots` | integer | Shots (player, summed over the season range). |
| `shots_into_goal` | integer | Shots into goal (player, summed over the season range). |
| `expected_goals` | numeric | Expected goals (xG) of the player's shots summed over the season range, from Liiga's undocumented model. |
| `contest_won` | integer | Contests (puck battles; definition unverified) the player won, summed over the season range. |
| `contest_lost` | integer | Contests (puck battles; definition unverified) the player lost, summed over the season range. |
| `goals_to_empty_goal` | integer | Goals to empty goal (player, summed over the season range). |
| `time_on_ice_avg` | numeric | Time on ice (player, summed over the season range). Seconds per game. |
| `contest_won_percentage` | numeric | Share of contests won, 0-100, over the season range (definition unverified). |
| `xge` | numeric | Goals minus expectedGoals over the season range (4 goals on 5.0956 xG = -1.0956). |
| `shot_percentage` | numeric | Shot percentage (player, summed over the season range). Percent, 0-100. |

## Details

Endpoint:
`GET https://liiga.fi/api/v2/players/stats/summed/{season_from}/{season_to}/{tournament}/true`

Seasons are END years (2026 = 2025-26), the same key as sdv-py. Requests
to liiga.fi are spaced at least one second apart. A request Liiga
answers with nothing (an unknown game, a tournament not played that
season, a shot map before 2016) raises `fastRhockey_no_data`; a failed
fetch or an error-shaped answer raises `fastRhockey_asset_fetch`; a bad
argument raises `fastRhockey_bad_request`.

The columns below are those of the default `data_type`; the other tables
carry their own stat columns next to `team_id` / `player_id`.

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
[`liiga_players_info()`](https://fastRhockey.sportsdataverse.org/reference/liiga_players_info.md),
[`liiga_schedule()`](https://fastRhockey.sportsdataverse.org/reference/liiga_schedule.md),
[`liiga_shotmap()`](https://fastRhockey.sportsdataverse.org/reference/liiga_shotmap.md),
[`liiga_standings()`](https://fastRhockey.sportsdataverse.org/reference/liiga_standings.md),
[`liiga_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/liiga_team_stats.md),
[`liiga_teams()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams.md),
[`liiga_teams_history()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams_history.md),
[`liiga_tournament()`](https://fastRhockey.sportsdataverse.org/reference/liiga_tournament.md)

## Examples

``` r
# \donttest{
  try(liiga_player_stats(season_from = 2026, season_to = 2026, tournament = "runkosarja"))
#> ── Liiga player stat tables Information from liiga.fi ──────────────────────────
#> ℹ Data updated: 2026-10-18 05:47:34 UTC
#> # A tibble: 531 × 45
#>    player_id    team_id team_name team_short_name role  first_name last_name    
#>        <int>      <int> <chr>     <chr>           <chr> <chr>      <chr>        
#>  1  23017821 1368624751 K-Espoo   KES             OL    Joonas     Rask         
#>  2  60667806  624554857 Lukko     LUK             OL    Petr       Vechet       
#>  3  60665758  495643563 Kärpät    KÄR             VL    Samuel     Jung         
#>  4  40078944  679171680 Ässät     ÄSS             KH    Eeli       Äijälä       
#>  5  28471734  933686567 SaiPa     SAI             OP    Samuli     Niinisaari   
#>  6  60954533  859884935 KalPa     KAL             OL    Cade       Borchardt    
#>  7  60900264  859884935 KalPa     KAL             VL    Tim        Söderlund Le…
#>  8  31296980 1368624751 K-Espoo   KES             VL    Jere       Väisänen     
#>  9  24509817  875886777 Pelicans  PEL             VP    Casimir    Jürgens      
#> 10  40077933  292293444 Jukurit   JUK             VP    Joonas     Eronen       
#> # ℹ 521 more rows
#> # ℹ 38 more variables: nationality <chr>, tournament <chr>, picture_url <chr>,
#> #   previous_teams_for_tournament <chr>, injured <lgl>, jersey <int>,
#> #   last_season <int>, goalkeeper <lgl>, games <int>, played_games <int>,
#> #   rookie <lgl>, suspended <lgl>, removed <lgl>, time_on_ice <int>,
#> #   current <lgl>, is_u20 <lgl>, golden_helmets <int>, alumni_points <int>,
#> #   goals <int>, assists <int>, points <int>, plus <int>, minus <int>, …
# }
```
