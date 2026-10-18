# **Liiga game lineups**

One game's lineups, both teams: line, role, jersey, captaincy and bio.

## Usage

``` r
liiga_game_lineups(season, game_id)
```

## Arguments

- season:

  Season END year (2026 = 2025-26). Game ids repeat across seasons, so a
  game is (season, game_id).

- game_id:

  Game id (game_id in liiga_schedule or liiga_games), e.g. 1; unique
  only within a season.

## Value

A `fastRhockey_data` tibble (32 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `season` | integer | Season END year (2026 = 2025-26). |
| `game_id` | integer | Game id, unique only within a season: key a game as (season, game_id). |
| `side` | character | Which team of the game the row belongs to: home or away. |
| `player_id` | integer | Player id; the same number in lineups, rosters, stat tables and the profile (fihaId). |
| `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
| `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
| `team_name` | character | Team's display name, e.g. HPK or Tappara. |
| `line` | integer | Line number 1-4; null for goalkeepers and extras. |
| `country_of_birth` | character | Country of birth, 3-letter code. |
| `place_of_birth` | character | Player's place of birth (town or city). |
| `date_of_birth` | character | Date of birth, YYYY-MM-DD. |
| `nationality` | character | Nationality, 3-letter code. |
| `first_name` | character | Player's first name. |
| `last_name` | character | Player's last name. |
| `role` | character | Position: CENTER, LEFT_WING, RIGHT_WING, LEFT_DEFENSEMAN, RIGHT_DEFENSEMAN, SEVENTH_DEFENSEMAN, GOALIE. |
| `role_code` | character | Position code, Finnish: KH = centre, VL/OL = left/right wing, VP/OP = left/right defence, 7. P = seventh defenceman, MV = goalkeeper. |
| `handedness` | character | Shoots LEFT or RIGHT. |
| `height` | integer | Player's height in centimetres. |
| `weight` | integer | Player's weight in kilograms. |
| `captain` | logical | True when the player is the team captain (C). |
| `rookie` | logical | True when Liiga flags the player as a rookie. |
| `alternate_captain` | logical | True when the player is an alternate captain (A). |
| `jersey` | integer | Jersey (sweater) number the player wore. |
| `picture_url` | character | URL of the player's photo. |
| `injured` | logical | True when the player is listed as injured. |
| `suspended` | logical | True when the player is listed as suspended. |
| `removed` | logical | Removed from the roster. |
| `awards` | character | Player awards of the game (stars). |
| `sponsors` | character | Player's sponsors; null in every capture (unverified). |
| `extra_description` | character | Extra profile text for the player; null in every capture (meaning unverified). |
| `extra_strengths` | character | Extra profile field for the player's strengths; null in every capture (unverified). |
| `extra_rookie_picture` | logical | Flag for an extra rookie picture on the player's profile (meaning unverified). |

## Details

Endpoint: `GET https://liiga.fi/api/v2/games/{season}/{game_id}`

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
[`liiga_teams()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams.md),
[`liiga_teams_history()`](https://fastRhockey.sportsdataverse.org/reference/liiga_teams_history.md),
[`liiga_tournament()`](https://fastRhockey.sportsdataverse.org/reference/liiga_tournament.md)

## Examples

``` r
# \donttest{
  try(liiga_game_lineups(game_id = 1, season = 2026))
#> ── Liiga game lineups Information from liiga.fi ────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 05:47:19 UTC
#> # A tibble: 62 × 32
#>    season game_id side  player_id  team_id team_slug team_name  line
#>     <int>   <int> <chr>     <int>    <int> <chr>     <chr>     <int>
#>  1   2026       1 home   21870028 55786244 hpk       HPK           1
#>  2   2026       1 home   22709389 55786244 hpk       HPK           2
#>  3   2026       1 home   23029644 55786244 hpk       HPK           3
#>  4   2026       1 home   23806498 55786244 hpk       HPK           1
#>  5   2026       1 home   29885743 55786244 hpk       HPK           1
#>  6   2026       1 home   30417647 55786244 hpk       HPK           1
#>  7   2026       1 home   30670073 55786244 hpk       HPK           3
#>  8   2026       1 home   30851669 55786244 hpk       HPK           2
#>  9   2026       1 home   31585390 55786244 hpk       HPK           2
#> 10   2026       1 home   32919839 55786244 hpk       HPK           4
#> # ℹ 52 more rows
#> # ℹ 24 more variables: country_of_birth <chr>, place_of_birth <chr>,
#> #   date_of_birth <chr>, nationality <chr>, first_name <chr>, last_name <chr>,
#> #   role <chr>, role_code <chr>, handedness <chr>, height <int>, weight <int>,
#> #   captain <lgl>, rookie <lgl>, alternate_captain <lgl>, jersey <int>,
#> #   picture_url <chr>, injured <lgl>, suspended <lgl>, removed <lgl>,
#> #   awards <chr>, sponsors <chr>, extra_description <chr>, …
# }
```
