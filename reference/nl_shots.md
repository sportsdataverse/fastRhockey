# **National League shots with location and xG**

One game's shot attempts with rink location (metres) and expected goals
(xG); NL games only.

## Usage

``` r
nl_shots(game_id, return_parsed = TRUE)
```

## Arguments

- game_id:

  SIHF game id, 14 digits `<season(4)><competition(4)><sequence(6)>`
  (`game_id` in
  [`nl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nl_schedule.md);
  the same id as the SIHF family's `game_id`), e.g. `20261105000325`. A
  whole number or a digit string; 14 digits are exact as a double, so do
  not format them through `integer`.

- return_parsed:

  Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble;
  `FALSE` returns the raw JSON body as a nested list (api keys already
  dropped).

## Value

A `fastRhockey_data` tibble (20 columns) with the columns below, or the
raw JSON body when `return_parsed = FALSE`. A valid but empty payload
returns a zero-row tibble with these columns.

|  |  |  |
|----|----|----|
| col_name | type | description |
| game_id | numeric | SIHF game id `<season(4)><competition(4)><sequence(6)>` (whole number; e.g. 20261105000325, 1105 = National League). The same id as the sihf family's game_id; its first four digits are the season END year. |
| team_side | character | home or away: the side of the team this row belongs to. |
| team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
| player_id | numeric | SIHF licence number (whole number; e.g. 305519 = Tobias Geisser). Joins the sihf family's player_id and every SIHF \*LicenceNr. |
| player_number | character | Shooter's sweater number, from the game's lineup (a string). |
| player_first_name | character | Shooter's first name, from the game's lineup. |
| player_last_name | character | Shooter's last name, from the game's lineup. |
| period | integer | Period number (1-3; overtime follows) |
| time | integer | Game clock elapsed in seconds (period 2 starts at 1200) |
| strength | character | Strength: EQ (even), PP (power play), PK (shorthanded) |
| shot_type | character | Outcome: SOG (saved shot on goal), MISS, POST, BLOCK, GOAL |
| pos_x | numeric | Across-ice position in metres, 0-30 (rink width). Both axes run in metres on a 30 x 60 m rink. |
| pos_y | numeric | Distance in metres from the end boards of the attacked end, 0-60 along the rink length. Shots are mirrored to one attacking end (inferred); above 30 is a shot from beyond centre; the goal line is 4 m out. |
| pos_x_percentage | numeric | posX / 30 (verified on 475 shots) |
| pos_y_percentage | numeric | pos_y / 24, the depth of the app's zone drawing; above 1 for long shots. |
| video_time | integer | Seconds into the game video (= unix - unixStart; verified on 475 shots) |
| xg | numeric | Expected goals of this shot, 0-1. Non-zero from the 2023 season (one sampled game per season; 0 in the 2021 and 2022 games); always 0 on a BLOCK. |
| xg_sum | numeric | Running sum of this team's xg up to and including this shot |
| unix | numeric | Wall-clock time of the shot, Unix epoch seconds |
| unix_start | numeric | Wall-clock start of the game video, Unix epoch seconds |

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

Endpoint: `GET https://www.nationalleague.ch/api/games/{gameId}`.

The six game-page wrappers
([`nl_game_overview()`](https://fastRhockey.sportsdataverse.org/reference/nl_game_overview.md),
[`nl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/nl_pbp.md),
[`nl_lineups()`](https://fastRhockey.sportsdataverse.org/reference/nl_lineups.md),
[`nl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/nl_player_box.md),
[`nl_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_stats.md),
`nl_shots()`) each read the same `/games/{gameId}` body, one request
each.

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
[`nl_playouts()`](https://fastRhockey.sportsdataverse.org/reference/nl_playouts.md),
[`nl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nl_schedule.md),
[`nl_standings()`](https://fastRhockey.sportsdataverse.org/reference/nl_standings.md),
[`nl_team_info()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_info.md),
[`nl_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_player_stats.md),
[`nl_team_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_schedule.md),
[`nl_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_stats.md),
[`nl_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/nl_top_scorers.md)

## Examples

``` r
# \donttest{
  try(nl_shots(game_id = 20261105000325))
#> ── National League Shots Information from nationalleague.ch ────────────────────
#> ℹ Data updated: 2026-10-18 05:50:34 UTC
#> # A tibble: 105 × 20
#>    game_id team_side team_id player_id player_number player_first_name
#>      <dbl> <chr>       <dbl>     <dbl> <chr>         <chr>            
#>  1 2.03e13 home       101151    308175 77            Nico             
#>  2 2.03e13 away       102127    313213 28            Dario            
#>  3 2.03e13 away       102127    156475 71            Julian           
#>  4 2.03e13 home       101151    312365 36            Simon            
#>  5 2.03e13 away       102127    342927 50            Juuso            
#>  6 2.03e13 away       102127    142801 72            Samuel           
#>  7 2.03e13 home       101151    309618 66            Valentin         
#>  8 2.03e13 home       101151    340162 6             Klas             
#>  9 2.03e13 home       101151    152416 90            Sven             
#> 10 2.03e13 home       101151    154648 96            Chris            
#> # ℹ 95 more rows
#> # ℹ 14 more variables: player_last_name <chr>, period <int>, time <int>,
#> #   strength <chr>, shot_type <chr>, pos_x <dbl>, pos_y <dbl>,
#> #   pos_x_percentage <dbl>, pos_y_percentage <dbl>, video_time <int>, xg <dbl>,
#> #   xg_sum <dbl>, unix <dbl>, unix_start <dbl>
# }
```
