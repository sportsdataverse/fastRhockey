# **National League game lineups**

One game's lineups: every dressed player by unit, with licence number,
position and line.

## Usage

``` r
nl_lineups(game_id, return_parsed = TRUE)
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

A `fastRhockey_data` tibble (13 columns) with the columns below, or the
raw JSON body when `return_parsed = FALSE`. A valid but empty payload
returns a zero-row tibble with these columns.

|  |  |  |
|----|----|----|
| col_name | type | description |
| game_id | numeric | SIHF game id `<season(4)><competition(4)><sequence(6)>` (whole number; e.g. 20261105000325, 1105 = National League). The same id as the sihf family's game_id; its first four digits are the season END year. |
| team_side | character | home or away: the side of the team this row belongs to. |
| team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
| unit | character | Lineup group: goalkeeper, or the line number 1-4 as a string. |
| player_id | numeric | SIHF licence number (whole number; e.g. 305519 = Tobias Geisser). Joins the sihf family's player_id and every SIHF \*LicenceNr. |
| position | character | goalkeeper, defender or forwarder (sic) |
| line | integer | Line number, an integer (goalkeepers 1-2 = first / backup) |
| first_name | character | First (given) name of the player |
| last_name | character | Last (family) name of the player |
| number | character | Sweater number, a string |
| is_topscorer | logical | Wears the team's PostFinance Top Scorer jersey |
| is_captain | logical | Whether the player is the team captain |
| first_goalkeeper | logical | Starting goaltender |

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
`nl_lineups()`,
[`nl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/nl_player_box.md),
[`nl_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_stats.md),
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
[`nl_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_stats.md),
[`nl_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/nl_top_scorers.md)

## Examples

``` r
# \donttest{
  try(nl_lineups(game_id = 20261105000325))
#> ── National League Lineups Information from nationalleague.ch ──────────────────
#> ℹ Data updated: 2026-10-18 06:17:12 UTC
#> # A tibble: 44 × 13
#>    game_id team_side team_id unit  player_id position  line first_name last_name
#>      <dbl> <chr>       <dbl> <chr>     <dbl> <chr>    <int> <chr>      <chr>    
#>  1 2.03e13 home       101151 goal…    150983 goalkee…     1 Sandro     Aeschlim…
#>  2 2.03e13 home       101151 goal…    309536 goalkee…     2 Luca       Hollenst…
#>  3 2.03e13 home       101151 1        153736 defender     1 Lukas      Frick    
#>  4 2.03e13 home       101151 1        340162 defender     1 Klas       Dahlbeck 
#>  5 2.03e13 home       101151 1        301617 forward…     1 Tino       Kessler  
#>  6 2.03e13 home       101151 1        346405 forward…     1 Filip      Zadina   
#>  7 2.03e13 home       101151 1        347698 forward…     1 Rasmus     Asplund  
#>  8 2.03e13 home       101151 2        152416 defender     2 Sven       Jung     
#>  9 2.03e13 home       101151 2        322236 defender     2 Calle      Andersson
#> 10 2.03e13 home       101151 2        309618 forward…     2 Valentin   Nussbaum…
#> # ℹ 34 more rows
#> # ℹ 4 more variables: number <chr>, is_topscorer <lgl>, is_captain <lgl>,
#> #   first_goalkeeper <lgl>
# }
```
