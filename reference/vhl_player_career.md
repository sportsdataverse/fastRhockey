# VHL Player Career

Player career by season stage and team (skater and goalie columns in one
frame).

## Usage

``` r
vhl_player_career(player_id, return_parsed = TRUE)
```

## Arguments

- player_id:

  Player id (player_id in any VHL frame), e.g. 34526.

- return_parsed:

  `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's
  parsed record (a named list, the sdv-internal-refs capture shape, plus
  `_request` with the season, stage, tn and the ids asked for).

## Value

A `fastRhockey_data` tibble with one row per season stage and team:

|  |  |  |
|----|----|----|
| col_name | type | description |
| `player_id` | integer | Player id, read from the page's own `/players/{id}/games/` tab link; equals the requested id or the capture refuses. |
| `season` | integer | Season END year (2026 = 2025/26). |
| `stage` | character | Season stage: regular or playoff. |
| `tn` | integer | Tournament id of the season stage (2025/26 regular season = 1374, playoffs = 1375); vhl_tn maps season + stage to it. |
| `team_season_id` | integer | Team-season id (khl/ terminology): one id per team per tournament, from the KHL's sequence (khl/: Spartak = 14934 in 1369; here Metallurg = 15017 in 1374). The `{id}` of `/calendar/{tn}/0/{id}/` and `/stats/players/{tn}/all/{id}/`. Not the stable club id (\*\_club_id) and not the team page id. With tn, the bridge from a player to every tournament team he played for. |
| `team` | character | Team short name (Latin; the report tables' form). |
| `jersey` | integer | Jersey number worn for that team that season (#). |
| `games_played` | integer | Games played (GP). |
| `goals` | integer | Goals (G) the player scored in the row's scope: the game in a box score, the season stage in stats and career rows. |
| `assists` | integer | Assists (A) credited to the player in the row's scope: the game in a box score, the season stage in stats and career rows. |
| `points` | integer | Points, goals + assists (PTS). |
| `plus_minus` | integer | Plus/minus (+/-). |
| `penalty_minutes` | integer | Penalty minutes (PIM). |
| `even_strength_goals` | integer | Even-strength goals (ESG). |
| `power_play_goals` | integer | Power-play goals (PPG). |
| `shorthanded_goals` | integer | Shorthanded goals (SHG). |
| `overtime_goals` | integer | Overtime goals (OTG). |
| `game_winning_goals` | integer | Game-winning goals (GWG). |
| `shootout_deciding_goals` | integer | Shootout-deciding goals (SDS; the legend says 'Shootouts deciding shots'). |
| `shots_on_goal` | integer | Shots on goal (SOG). |
| `shot_pct` | numeric | Shooting percentage, goals / shots on goal x 100 (%SOG); 0-100 scale; null when the page shows '-'. |
| `shots_per_game` | numeric | Shots on goal per game (S/G). |
| `faceoffs` | integer | Faceoffs taken (FO). |
| `faceoffs_won` | integer | Faceoffs won (FOW). |
| `faceoff_pct` | numeric | Faceoff win percentage (%FO); 0-100 scale; null when the page shows '-' (no faceoffs taken). |
| `toi_per_game` | character | Average time on ice per game (TOI/G), 'm:ss' / 'mm:ss' string; null when the page shows '-'. |
| `shifts_per_game` | numeric | Average shifts per game (Sft/G); integer in a one-game report, decimal in season tables. |
| `hits` | integer | Body checks delivered (Hits). |
| `blocked_shots` | integer | Blocked shots (BLS). |
| `wins` | integer | Goalie wins (W). |
| `losses` | integer | Goalie losses (L). |
| `shootouts_played` | integer | Shootouts played (SOP). |
| `shots_against` | integer | Shots on goal faced (the SOG column of goalie tables). |
| `goals_against` | integer | Goals allowed (GA). |
| `saves` | integer | Saves (Sv): shots on goal the goalie stopped in the row's scope (the game, or the season stage in stats and career rows). |
| `save_pct` | numeric | Save percentage (%Sv); 0-100 scale (93.8 = 93.8 %); null when the page shows '-'. |
| `gaa` | numeric | Goals-against average (GAA) = 60 x GA / TOI minutes; null when the page shows '-'. |
| `shutouts` | integer | Shutouts (SO): games in the row's scope in which the goalie allowed no goal. |
| `toi` | character | Time on ice (TOI), 'mm:ss' string; minutes run past 59 over a season ('2314:48'); null when the page shows '-'. |

With `return_parsed = FALSE`, the page's parsed record (a named list)
instead.

## Details

Source: `GET https://www.vhlru.ru/en/players/{player_id}/`
(server-rendered HTML, keyless; parsed with rvest). Unofficial use of a
public source; payloads are not redistributed.

## Owner flag (sanctions)

The VHL is run by Russian league operators, and several clubs are owned
or sponsored by sanctioned state companies. Fetching on your own machine
is fine; a published data release or a hosted mirror of this data needs
the owner's sanctions / legal sign-off first.

## Errors

- `fastRhockey_no_data`: the VHL has nothing for the request (a season
  without that stage, a game not in the season's calendar, an unknown
  game, player or team page).

- `fastRhockey_bad_request`: a bad `season` / `stage` / id, a missing
  argument, or HTTP 400 / 422.

- `fastRhockey_asset_fetch`: the page could not be fetched, or it is not
  the page asked for (another season, game or player; a missing table or
  header drift).

## See also

Other VHL Functions:
[`vhl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/vhl_game_info.md),
[`vhl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/vhl_goalie_box.md),
[`vhl_goalie_stats()`](https://fastRhockey.sportsdataverse.org/reference/vhl_goalie_stats.md),
[`vhl_goals()`](https://fastRhockey.sportsdataverse.org/reference/vhl_goals.md),
[`vhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/vhl_pbp.md),
[`vhl_penalties()`](https://fastRhockey.sportsdataverse.org/reference/vhl_penalties.md),
[`vhl_player()`](https://fastRhockey.sportsdataverse.org/reference/vhl_player.md),
[`vhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/vhl_player_box.md),
[`vhl_roster()`](https://fastRhockey.sportsdataverse.org/reference/vhl_roster.md),
[`vhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/vhl_schedule.md),
[`vhl_skater_stats()`](https://fastRhockey.sportsdataverse.org/reference/vhl_skater_stats.md),
[`vhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/vhl_standings.md),
[`vhl_tn()`](https://fastRhockey.sportsdataverse.org/reference/vhl_tn.md)

## Examples

``` r
# \donttest{
  try(vhl_player_career(player_id = 34526))
#> ── VHL Player Career Information from vhlru.ru ─────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:12:45 UTC
#> # A tibble: 5 × 39
#>   player_id season stage      tn team_season_id team   jersey games_played goals
#>       <int>  <int> <chr>   <int>          <int> <chr>   <int>        <int> <int>
#> 1     34526   2027 regular  1430          15516 Dynam…     NA            0     0
#> 2     34526   2026 playoff  1375          15424 Magni…     31           11     3
#> 3     34526   2026 regular  1374          15016 Magni…     31           60    25
#> 4     34526   2025 regular  1306          14252 Omski…     94           57    14
#> 5     34526   2024 regular  1230          13642 Ryaza…     37           27     3
#> # ℹ 30 more variables: assists <int>, points <int>, plus_minus <int>,
#> #   penalty_minutes <int>, even_strength_goals <int>, power_play_goals <int>,
#> #   shorthanded_goals <int>, overtime_goals <int>, game_winning_goals <int>,
#> #   shootout_deciding_goals <int>, shots_on_goal <int>, shot_pct <dbl>,
#> #   shots_per_game <dbl>, faceoffs <int>, faceoffs_won <int>,
#> #   faceoff_pct <dbl>, toi_per_game <chr>, shifts_per_game <dbl>, hits <int>,
#> #   blocked_shots <int>, wins <int>, losses <int>, shootouts_played <int>, …
# }
```
