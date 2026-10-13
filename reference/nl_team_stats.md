# **National League team game totals**

One game's team totals: shots by period, faceoffs by zone, power play
and penalty kill.

## Usage

``` r
nl_team_stats(game_id, return_parsed = TRUE)
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

A `fastRhockey_data` tibble (40 columns) with the columns below, or the
raw JSON body when `return_parsed = FALSE`. A valid but empty payload
returns a zero-row tibble with these columns.

|  |  |  |
|----|----|----|
| col_name | type | description |
| game_id | numeric | SIHF game id `<season(4)><competition(4)><sequence(6)>` (whole number; e.g. 20261105000325, 1105 = National League). The same id as the sihf family's game_id; its first four digits are the season END year. |
| team_side | character | home or away: the side of the team this row belongs to. |
| team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
| g | integer | Number of goals scored |
| ga | integer | Goals against: number of goals conceded |
| sa | integer | Shots on goal against |
| sog1 | integer | Shots on goal in the 1st period |
| sog2 | integer | Shots on goal in the 2nd period |
| sog3 | integer | Shots on goal in the 3rd period |
| sog_ot | integer | Shots on goal in overtime, all periods |
| sog_ot1 | integer | Shots on goal in overtime period 1 |
| sog_ot2 | integer | Shots on goal in overtime period 2 |
| sog_ot3 | integer | Shots on goal in overtime period 3 |
| shm | integer | Shots that missed the net |
| shp | integer | Shots that hit the post |
| shb | integer | Own shots blocked by an opponent |
| bks | integer | Opponent shots blocked |
| fow_d | integer | Defensive-zone faceoffs won (0 in every sampled game) |
| fow_n | integer | Neutral-zone faceoffs won (0 in every sampled game) |
| fow_o | integer | Offensive-zone faceoffs won (0 in every sampled game) |
| fol_n | integer | Neutral-zone faceoffs lost (0 in every sampled game) |
| fol_o | integer | Offensive-zone faceoffs lost (0 in every sampled game) |
| fol_d | integer | Defensive-zone faceoffs lost (0 in every sampled game) |
| ppt | integer | Power-play time, seconds |
| ppo | integer | Power-play opportunities |
| ppg | integer | Power-play goals |
| ppga | integer | Shorthanded goals allowed while on the power play |
| pkt | integer | Shorthanded time, seconds |
| pks | integer | Times shorthanded |
| pkg | integer | Shorthanded goals scored |
| pkga | integer | Power-play goals allowed while shorthanded |
| faceoffs | integer | Faceoffs taken (upstream `fo`: fow + fol). Not penalties. |
| pim | integer | Penalty minutes |
| sog | integer | Number of shots on goal |
| sog_percentage | numeric | Shooting percentage, 100 \* g / sog |
| pp_percentage | numeric | Power-play percentage, 100 \* ppg / ppo |
| pk_percentage | numeric | Penalty-kill percentage, 0-100 |
| fow | integer | Number of faceoffs won |
| fol | integer | Number of faceoffs lost |
| faceoffs_percentage | numeric | Faceoff win percentage (upstream `foPercentage`); -1 without faceoffs. |

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
`nl_team_stats()`,
[`nl_shots()`](https://fastRhockey.sportsdataverse.org/reference/nl_shots.md))
each read the same `/games/{gameId}` body, one request each.

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
[`nl_shots()`](https://fastRhockey.sportsdataverse.org/reference/nl_shots.md),
[`nl_standings()`](https://fastRhockey.sportsdataverse.org/reference/nl_standings.md),
[`nl_team_info()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_info.md),
[`nl_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_player_stats.md),
[`nl_team_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_schedule.md),
[`nl_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/nl_top_scorers.md)

## Examples

``` r
# \donttest{
  try(nl_team_stats(game_id = 20261105000325))
#> ── National League Team Stats Information from nationalleague.ch ───────────────
#> ℹ Data updated: 2026-10-13 01:58:37 UTC
#> # A tibble: 2 × 40
#>   game_id team_side team_id     g    ga    sa  sog1  sog2  sog3 sog_ot sog_ot1
#>     <dbl> <chr>       <dbl> <int> <int> <int> <int> <int> <int>  <int>   <int>
#> 1 2.03e13 home       101151     4     3    22    11    16     9      0       0
#> 2 2.03e13 away       102127     3     4    36     7     8     7      0       0
#> # ℹ 29 more variables: sog_ot2 <int>, sog_ot3 <int>, shm <int>, shp <int>,
#> #   shb <int>, bks <int>, fow_d <int>, fow_n <int>, fow_o <int>, fol_n <int>,
#> #   fol_o <int>, fol_d <int>, ppt <int>, ppo <int>, ppg <int>, ppga <int>,
#> #   pkt <int>, pks <int>, pkg <int>, pkga <int>, faceoffs <int>, pim <int>,
#> #   sog <int>, sog_percentage <dbl>, pp_percentage <dbl>, pk_percentage <dbl>,
#> #   fow <int>, fol <int>, faceoffs_percentage <dbl>
# }
```
