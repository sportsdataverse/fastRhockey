# **National League playout and qualification series**

Playout and league-qualification series by round: one row per game, with
its series score.

## Usage

``` r
nl_playouts(season = NULL, return_parsed = TRUE)
```

## Arguments

- season:

  Season END year (`2026` is 2025/26), sent to the API unchanged.
  Changing it changes every list, so it scopes the answer. Lists are
  served for 2019-2027 (the app's season picker) and patchily before:
  2012 and 2017 answered, 2010/2014/2015/2016/2018 answered an empty
  list (probed 2026-10-12). `NULL` (the default) leaves the choice to
  the API.

- return_parsed:

  Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble;
  `FALSE` returns the raw JSON body as a nested list (api keys already
  dropped).

## Value

A `fastRhockey_data` tibble (39 columns) with the columns below, or the
raw JSON body when `return_parsed = FALSE`. A valid but empty payload
returns a zero-row tibble with these columns.

|  |  |  |
|----|----|----|
| col_name | type | description |
| series_group | integer | Series number within the round (1 = first pairing; 0 seen in playouts) |
| series_home_team_short_name | character | Abbreviation of the team with home ice |
| series_away_team_short_name | character | Abbreviation of the other team |
| series_home_team_id | numeric | Team with home ice in the series. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
| series_away_team_id | numeric | The other team in the series. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
| series_home_team_result | integer | Series wins of the team with home ice |
| series_away_team_result | integer | Series wins of the other team |
| series_best_of | integer | Series length in games (7; 2 in the play-in) |
| series_title | character | Series length label, e.g. 'Best of 7' |
| round | character | Round key: final, semifinal, quarterfinal, playIn (/playoffs); leagueQualifications, playoutFinal (/playouts) |
| game_id | numeric | SIHF game id `<season(4)><competition(4)><sequence(6)>` (whole number; e.g. 20261105000325, 1105 = National League). The same id as the sihf family's game_id; its first four digits are the season END year. |
| home_team_short_name | character | Home team abbreviation (e.g. HCD) |
| home_team_name | character | Home team display name |
| away_team_short_name | character | Away team abbreviation |
| away_team_name | character | Away team display name |
| home_team_id | numeric | Home team. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
| away_team_id | numeric | Away team. SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
| vimeo_video_id | numeric | Full-game video. Vimeo video id (whole number): vimeo.com/`<id>`. |
| date | character | Scheduled start, ISO 8601 with UTC offset |
| status | character | Game state: finished, canceled, beforeStartOfPlay (others appear live) |
| base_status | character | Coarse game state; equal to status in every sampled game |
| home_team_result | integer | Home team goals (final or current score; 0 before the game) |
| away_team_result | integer | Away team goals (final or current score; 0 before the game) |
| is_overtime | logical | Game went to overtime |
| is_shootout | logical | Game went to a shootout |
| highlight_vimeo_video_id | numeric | Highlights video. Vimeo video id (whole number): vimeo.com/`<id>`. |
| has_highlight | logical | A highlight video exists |
| is_current | logical | Game belongs to the current game day |
| is_current_for_team | logical | Game is the team's current game (team-scoped views) |
| tv_partner | character | Broadcaster logo image URLs |
| spectators | integer | Attendance (whole number; the API sends a digit string). |
| has_my_sports | logical | Broadcast on MySports |
| referees | character | Officials as '#`<number>` `<name>`' strings (referees and linesmen; one entry in some games) |
| is_exhibition | logical | Exhibition game, not NL competition (competition code other than 1105) |
| home_team_coach | character | Home head coach name; filled in a game's `overview`, null in the season lists; unobserved: null, absent or empty in every sampled row |
| away_team_coach | character | Away head coach name; filled in a game's `overview`, null in the season lists; unobserved: null, absent or empty in every sampled row |
| game_time | integer | Elapsed game minutes; 0 on every game in the season lists, 60 in a finished game's overview |
| show_game_time | logical | App flag: show gameTime |
| arena | character | Name of the arena the game is played in |

## Details

**robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows
`/api`. The owner approved building on it on 2026-10-12, for polite,
incremental use only: requests are spaced at least one second apart (on
every attempt, retries included), and a bulk backfill or a published
mirror needs the owner's sign-off first. Crawl incrementally: fetch a
finished game once (keyed by `game_id`), find new games with one
[`nl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nl_schedule.md)
call per run, and fetch season tables at most once per game day.

Seasons are END years and go on the wire unchanged. Ids (`*_id`) are
whole-number `numeric` (a 14-digit `game_id` exceeds R's 32-bit
integer); every other whole-number column is `integer`. The club backend
key the API ships in `/teams/{id}/info` is dropped before parsing and
never returned.

Endpoint: `GET https://www.nationalleague.ch/api/games/playouts`.

## Errors

Raises a classed condition (all inherit `fastRhockey_error`):
`fastRhockey_no_data` for a 404 or an empty answer (no games or rows for
that season, team or phase), `fastRhockey_asset_fetch` for a failed
fetch, an error object in a 200, a body without the ids it must carry,
or an answer for another season, team, game or player, and
`fastRhockey_bad_request` for a 400/422 or an argument that is not a
whole number.

## See also

Other National League Functions:
[`nl_current_games()`](https://fastRhockey.sportsdataverse.org/reference/nl_current_games.md),
[`nl_game_overview()`](https://fastRhockey.sportsdataverse.org/reference/nl_game_overview.md),
[`nl_lineups()`](https://fastRhockey.sportsdataverse.org/reference/nl_lineups.md),
[`nl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/nl_pbp.md),
[`nl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/nl_player_box.md),
[`nl_player_season()`](https://fastRhockey.sportsdataverse.org/reference/nl_player_season.md),
[`nl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_player_stats.md),
[`nl_playoffs()`](https://fastRhockey.sportsdataverse.org/reference/nl_playoffs.md),
[`nl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nl_schedule.md),
[`nl_shots()`](https://fastRhockey.sportsdataverse.org/reference/nl_shots.md),
[`nl_standings()`](https://fastRhockey.sportsdataverse.org/reference/nl_standings.md),
[`nl_team_info()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_info.md),
[`nl_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_player_stats.md),
[`nl_team_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_schedule.md),
[`nl_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_stats.md),
[`nl_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/nl_top_scorers.md)

## Examples

``` r
# \donttest{
  try(nl_playouts(season = 2026))
#> ── National League Playouts Information from nationalleague.ch ─────────────────
#> ℹ Data updated: 2026-10-30 00:19:16 UTC
#> # A tibble: 4 × 39
#>   series_group series_home_team_sho…¹ series_away_team_sho…² series_home_team_id
#>          <int> <chr>                  <chr>                                <dbl>
#> 1            0 HCAP                   HCA                                 101152
#> 2            0 HCAP                   HCA                                 101152
#> 3            0 HCAP                   HCA                                 101152
#> 4            0 HCAP                   HCA                                 101152
#> # ℹ abbreviated names: ¹​series_home_team_short_name,
#> #   ²​series_away_team_short_name
#> # ℹ 35 more variables: series_away_team_id <dbl>,
#> #   series_home_team_result <int>, series_away_team_result <int>,
#> #   series_best_of <int>, series_title <chr>, round <chr>, game_id <dbl>,
#> #   home_team_short_name <chr>, home_team_name <chr>,
#> #   away_team_short_name <chr>, away_team_name <chr>, home_team_id <dbl>, …
# }
```
