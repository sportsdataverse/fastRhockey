# **National League standings**

Standings with team season stats.

## Usage

``` r
nl_standings(season = NULL, phase = NULL, return_parsed = TRUE)
```

## Arguments

- season:

  Season END year (`2026` is 2025/26), sent to the API unchanged.
  Changing it changes every list, so it scopes the answer. Lists are
  served for 2019-2027 (the app's season picker) and patchily before:
  2012 and 2017 answered, 2010/2014/2015/2016/2018 answered an empty
  list (probed 2026-10-12). `NULL` (the default) leaves the choice to
  the API.

- phase:

  SIHF phase id (`4940` is the 2025/26 Regular Season, `5297` the
  playoffs); phase ids change every season. `NULL` (the default) leaves
  the choice to the API: on `/player` that served the regular season
  (verified, byte-identical to `phase = 4940`); on the other routes that
  default is assumed, not verified.

- return_parsed:

  Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble;
  `FALSE` returns the raw JSON body as a nested list (api keys already
  dropped).

## Value

A `fastRhockey_data` tibble (123 columns) with the columns below, or the
raw JSON body when `return_parsed = FALSE`. A valid but empty payload
returns a zero-row tibble with these columns.

|  |  |  |
|----|----|----|
| col_name | type | description |
| rank | integer | Position of the team in the standings table |
| team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
| name | character | Team name as the league app displays it |
| short_name | character | Team abbreviation |
| streak | character | Last results, most recent first: W or L |
| gp | integer | Number of games played |
| gw | integer | Wins, all kinds |
| gwot | integer | Overtime + shootout wins (gwo + gwpe) |
| gwpe | integer | Number of wins decided in a shootout |
| gl | integer | Losses, all kinds |
| glot | integer | Overtime + shootout losses (glo + glpe) |
| glpe | integer | Shootout losses |
| po | integer | Standings points: 3 per regulation win, 2 per overtime/shootout win, 1 per overtime/shootout loss (verified) |
| g | integer | Number of goals scored |
| geq | integer | Even-strength goals (geq + gpp + gpk can fall short of g) |
| gpp | integer | Power-play goals |
| gpk | integer | Shorthanded goals |
| ga | integer | Goals against: number of goals conceded |
| gaeq | integer | Even-strength goals against |
| gapp | integer | Shorthanded goals allowed on the power play |
| gapk | integer | Power-play goals allowed while shorthanded |
| go | integer | Goal difference, g - ga (verified) |
| ppo | integer | Power-play opportunities |
| pks | integer | Times shorthanded |
| sog | integer | Number of shots on goal |
| penalties | integer | Penalties taken (upstream `fo`: fo2 + fo5 + fo10 + gm + mp). Not faceoffs: see face_offs / fot. |
| pim | integer | Penalty minutes |
| fow | integer | Number of faceoffs won |
| fol | integer | Number of faceoffs lost |
| shm | integer | Shots that missed the net |
| shp | integer | Shots that hit the post |
| shb | integer | Own shots blocked by an opponent |
| bks | integer | Opponent shots blocked |
| sa | integer | Shots on goal against |
| sam | integer | Shots against that missed the net; 0 on every 2026 team row |
| sap | integer | Shots against that hit the post; 0 on every 2026 team row |
| sab | integer | Shots against that were blocked; 0 on every 2026 team row |
| sort_order | integer | App sort key; not the rank |
| website | character | Club website URL |
| position_change | character | Rank movement since the last round ('none' on every 2026 row) |
| is_live | logical | Team is playing now |
| current_result | integer | Team's goals in a live game; 0 otherwise |
| current_opponent_result | integer | Opponent's goals in a live game; 0 otherwise |
| pos | integer | Standings position (= rank on every 2026 row) |
| hg | integer | Home games played (gp / 2 on every 2026 row) |
| gsow | integer | Shootout wins; duplicate of gwpe (equal on every 2026 row) |
| gfd | integer | Goals by defenders |
| gff | integer | Goals by forwards |
| gfi | integer | Goals by import (non-Swiss-licence) players (inferred; cf. the sihf/ teamGoalPosNation table) |
| gasow | integer | Shootout losses; duplicate of glpe (equal on every 2026 row) |
| pp | integer | Power-play time, seconds |
| bp | integer | Shorthanded (box-play) time, seconds |
| fo2 | integer | Minor (2-minute) penalties |
| fo5 | integer | Major (5-minute) penalties |
| fo10 | integer | 10-minute misconducts |
| gm | integer | Game misconducts |
| mp | integer | Match penalties |
| ps | integer | Shootout attempts taken |
| psg | integer | Goals scored in shootouts |
| psm | integer | Shootout misses (ps = psg + psm on every 2026 team row) |
| psa | integer | Shootout shots faced (psga + pssvs) |
| psga | integer | Shootout goals against |
| pssvs | integer | Saves made in shootouts |
| spec | integer | Total home attendance |
| cap | integer | Total home arena capacity over the home games |
| fowoz | integer | Offensive-zone faceoffs won |
| fownz | integer | Neutral-zone faceoffs won |
| fowdz | integer | Defensive-zone faceoffs won |
| foloz | integer | Offensive-zone faceoffs lost |
| folnz | integer | Neutral-zone faceoffs lost |
| foldz | integer | Defensive-zone faceoffs lost |
| sgo | integer | Not determined. Equals gff + gfd on every team row; skater values are close to but not always g |
| sslot | integer | Shots from the slot |
| sag | integer | Shots on goal against that were saved; 0 on every 2026 team row (use sa) |
| sago | integer | Shots on goal against that went in; 0 on every 2026 team row (use ga) |
| saslot | integer | Slot shots against |
| qualification_status | character | Clinch marker ('none' on every 2026 row) |
| is_qualified_chl | logical | Qualified for the Champions Hockey League |
| goal_efficiency | numeric | Shooting percentage, 100 \* g / sog (verified); -1 with no shots |
| ppg_efficiency | numeric | Power-play percentage, 100 \* gpp / ppo (verified) |
| ppggp | numeric | Power-play goals per game |
| ppopgp | numeric | Power-play opportunities per game |
| ppgp | numeric | Power-play time per game, seconds |
| gpg | numeric | Goals scored per game played |
| gagp | numeric | Goals against per game |
| gappgp | numeric | Shorthanded goals against per game |
| geqgp | numeric | Even-strength goals per game |
| gaeqgp | numeric | Even-strength goals against per game |
| bks_per_game | numeric | Blocked shots per game |
| pim_per_game | numeric | Penalty minutes per game |
| ps_percentage | numeric | Shootout success percentage, 0-100 |
| attendance | integer | Average home attendance (spec / hg) |
| capacity | integer | Arena capacity (cap / hg) |
| utilization | numeric | Capacity utilisation, 100 \* spec / cap |
| soggp | numeric | Shots on goal per game |
| sslotgp | numeric | Slot shots per game |
| shmgp | numeric | Missed shots per game |
| shpgp | numeric | Post hits per game |
| gpkgp | numeric | Shorthanded goals per game |
| pk_efficiency | numeric | Penalty-kill percentage, 100 \* (1 - gapk / pks) (verified) |
| pksgp | numeric | Times shorthanded per game |
| gapkgp | numeric | Power-play goals against per game |
| bpgp | numeric | Shorthanded time per game, seconds |
| fot | integer | Faceoffs taken (fow + fol) |
| face_offs_percentage | numeric | Faceoff win percentage, 0-100; -1 with no faceoffs |
| fogp | numeric | Faceoffs taken per game |
| fowgp | numeric | Faceoffs won per game |
| folgp | numeric | Faceoffs lost per game |
| fopoz | numeric | Offensive-zone faceoff win percentage, 0-100 |
| fopnz | numeric | Neutral-zone faceoff win percentage, 0-100 |
| fopdz | numeric | Defensive-zone faceoff win percentage, 0-100 |
| sagp | numeric | Shots on goal against per game |
| saslotgp | numeric | Slot shots against per game |
| gffd | integer | Goals by forwards and defenders (gff + gfd; verified) |
| gffp | numeric | Share of goals by forwards, 100 \* gff / g |
| gfdp | numeric | Share of goals by defenders, 100 \* gfd / g |
| gfip | numeric | Share of goals by import players, 100 \* gfi / g |
| pssvs_percentage | numeric | Shootout save percentage, 0-100; -1 with no shootout shots faced |
| pgp | numeric | Standings points per game |
| gwit | integer | Wins in regulation time (gw = gwit + overtime/shootout wins; verified) |
| gwo | integer | Number of wins in overtime |
| glit | integer | Losses in regulation time (gl = glit + overtime/shootout losses; verified) |
| glo | integer | Overtime losses |

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

Endpoint: `GET https://www.nationalleague.ch/api/teams`.

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
[`nl_team_info()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_info.md),
[`nl_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_player_stats.md),
[`nl_team_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_schedule.md),
[`nl_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_stats.md),
[`nl_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/nl_top_scorers.md)

## Examples

``` r
# \donttest{
  try(nl_standings(phase = 4940, season = 2026))
#> ── National League Standings Information from nationalleague.ch ────────────────
#> ℹ Data updated: 2026-10-18 06:17:24 UTC
#> # A tibble: 14 × 123
#>     rank team_id name      short_name streak    gp    gw  gwot  gwpe    gl  glot
#>    <int>   <dbl> <chr>     <chr>      <chr>  <int> <int> <int> <int> <int> <int>
#>  1     1  101151 HC Davos  HCD        "[\"W…    52    39     5     3    13     5
#>  2     2  103138 Fribourg… FRI        "[\"O…    52    37    13     8    15     2
#>  3     3  103140 Genève-S… GSHC       "[\"W…    52    31     5     1    21     3
#>  4     4  101139 ZSC Lions ZSC        "[\"O…    52    29     4     2    23     8
#>  5     5  101150 HC Lugano HCL        "[\"L…    52    28     2     2    24     7
#>  6     6  103141 Lausanne… LHC        "[\"O…    52    29     7     2    23     5
#>  7     7  101060 SCRJ Lak… SCRJ       "[\"L…    52    27     6     2    25     6
#>  8     8  101144 EV Zug    EVZ        "[\"W…    52    23     3     2    29     9
#>  9     9  102126 SC Bern   SCB        "[\"W…    52    24     9     5    28     5
#> 10    10  102128 EHC Biel… EHCB       "[\"W…    52    24    10     7    28     5
#> 11    11  102127 SCL Tige… SCL        "[\"O…    52    20     4     1    32     8
#> 12    12  101149 EHC Klot… EHCK       "[\"L…    52    20     5     0    32     8
#> 13    13  101152 HC Ambri… HCAP       "[\"O…    52    20     6     3    32     5
#> 14    14  103144 HC Ajoie  HCA        "[\"O…    52    13     4     3    39     7
#> # ℹ 112 more variables: glpe <int>, po <int>, g <int>, geq <int>, gpp <int>,
#> #   gpk <int>, ga <int>, gaeq <int>, gapp <int>, gapk <int>, go <int>,
#> #   ppo <int>, pks <int>, sog <int>, penalties <int>, pim <int>, fow <int>,
#> #   fol <int>, shm <int>, shp <int>, shb <int>, bks <int>, sa <int>, sam <int>,
#> #   sap <int>, sab <int>, sort_order <int>, website <chr>,
#> #   position_change <chr>, is_live <lgl>, current_result <int>,
#> #   current_opponent_result <int>, pos <int>, hg <int>, gsow <int>, …
# }
```
