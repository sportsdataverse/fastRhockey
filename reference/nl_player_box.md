# **National League player box score**

One game's player box score: goals, assists, shots, faceoffs, ice time,
goaltending.

## Usage

``` r
nl_player_box(game_id, return_parsed = TRUE)
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

A `fastRhockey_data` tibble (38 columns) with the columns below, or the
raw JSON body when `return_parsed = FALSE`. A valid but empty payload
returns a zero-row tibble with these columns.

|  |  |  |
|----|----|----|
| col_name | type | description |
| game_id | numeric | SIHF game id `<season(4)><competition(4)><sequence(6)>` (whole number; e.g. 20261105000325, 1105 = National League). The same id as the sihf family's game_id; its first four digits are the season END year. |
| team_side | character | home or away: the side of the team this row belongs to. |
| team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
| player_id | numeric | SIHF licence number (whole number; e.g. 305519 = Tobias Geisser). Joins the sihf family's player_id and every SIHF \*LicenceNr. |
| number | character | Sweater number, a string |
| first_name | character | First (given) name of the player |
| last_name | character | Last (family) name of the player |
| short_name | character | Initial and last name, e.g. 'S. Aeschlimann' |
| name | character | First and last name |
| position | character | goalkeeper, defender or forwarder (sic) |
| fow | integer | Number of faceoffs won |
| fol | integer | Number of faceoffs lost |
| sog | integer | Number of shots on goal |
| shm | integer | Shots that missed the net |
| shp | integer | Shots that hit the post |
| shb | integer | Own shots blocked by an opponent |
| bks | integer | Opponent shots blocked |
| toi | integer | Time on ice, seconds |
| toi_eq | integer | Even-strength time on ice, seconds |
| toi_pp | integer | Power-play time on ice, seconds |
| toi_pk | integer | Shorthanded time on ice, seconds |
| pim | integer | Penalty minutes |
| g | integer | Number of goals scored |
| a1 | integer | First (primary) assists credited |
| a2 | integer | Second (secondary) assists credited |
| pl | integer | Plus count: on ice for an even-strength or shorthanded goal for |
| mi | integer | Minus count: on ice for a goal against |
| mip | integer | Goaltender time played, hundredths of a minute (6000 = 60:00; = seconds / 0.6) |
| ga | integer | Goals against: number of goals conceded |
| svs | integer | Number of saves made by the goaltender |
| sa | integer | Shots on goal against |
| gaa | numeric | Goals-against average; 0 in every sampled row, even for goaltenders who conceded |
| save_percentage | numeric | Save percentage, 0-100 |
| faceoffs | integer | Faceoffs taken (upstream `fo`: fow + fol). Not penalties. |
| faceoffs_percentage | numeric | Faceoff win percentage (upstream `foPercentage`); -1 without faceoffs. |
| a | integer | Assists (a1 + a2) |
| pts | integer | Points (goals + assists) |
| pl_mi | integer | Plus/minus (pl - mi) |

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
`nl_player_box()`,
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
[`nl_lineups()`](https://fastRhockey.sportsdataverse.org/reference/nl_lineups.md),
[`nl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/nl_pbp.md),
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
  try(nl_player_box(game_id = 20261105000325))
#> ── National League Player Box Score Information from nationalleague.ch ─────────
#> ℹ Data updated: 2026-10-18 06:17:14 UTC
#> # A tibble: 44 × 38
#>    game_id team_side team_id player_id number first_name last_name   short_name 
#>      <dbl> <chr>       <dbl>     <dbl> <chr>  <chr>      <chr>       <chr>      
#>  1 2.03e13 home       101151    150983 29     Sandro     Aeschlimann S. Aeschli…
#>  2 2.03e13 home       101151    309536 51     Luca       Hollenstein L. Hollens…
#>  3 2.03e13 home       101151    153736 38     Lukas      Frick       L. Frick   
#>  4 2.03e13 home       101151    340162 6      Klas       Dahlbeck    K. Dahlbeck
#>  5 2.03e13 home       101151    152416 90     Sven       Jung        S. Jung    
#>  6 2.03e13 home       101151    322236 55     Calle      Andersson   C. Anderss…
#>  7 2.03e13 home       101151    153207 20     Michael    Fora        M. Fora    
#>  8 2.03e13 home       101151    312287 57     Davyd      Barandun    D. Barandun
#>  9 2.03e13 home       101151    308175 77     Nico       Gross       N. Gross   
#> 10 2.03e13 home       101151    301617 72     Tino       Kessler     T. Kessler 
#> # ℹ 34 more rows
#> # ℹ 30 more variables: name <chr>, position <chr>, fow <int>, fol <int>,
#> #   sog <int>, shm <int>, shp <int>, shb <int>, bks <int>, toi <int>,
#> #   toi_eq <int>, toi_pp <int>, toi_pk <int>, pim <int>, g <int>, a1 <int>,
#> #   a2 <int>, pl <int>, mi <int>, mip <int>, ga <int>, svs <int>, sa <int>,
#> #   gaa <dbl>, save_percentage <dbl>, faceoffs <int>,
#> #   faceoffs_percentage <dbl>, a <int>, pts <int>, pl_mi <int>
# }
```
