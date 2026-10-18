# **National League single player season stats**

One player's season stats.

## Usage

``` r
nl_player_season(player_id, season = NULL, phase = NULL, return_parsed = TRUE)
```

## Arguments

- player_id:

  SIHF licence number (`player_id` in
  [`nl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_player_stats.md);
  joins the SIHF family's `player_id`), e.g. `305519`. A whole number or
  a digit string.

- season:

  Season END year (`2026` is 2025/26), sent to the API unchanged. `NULL`
  (the default) answers the *current* season (`gp` 7 in 2027 against 51
  for `season = 2026` for one player), so pass it for history.

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

A `fastRhockey_data` tibble (145 columns) with the columns below, or the
raw JSON body when `return_parsed = FALSE`. A valid but empty payload
returns a zero-row tibble with these columns.

|  |  |  |
|----|----|----|
| col_name | type | description |
| player_id | numeric | SIHF licence number (whole number; e.g. 305519 = Tobias Geisser). Joins the sihf family's player_id and every SIHF \*LicenceNr. |
| first_name | character | First (given) name of the player |
| last_name | character | Last (family) name of the player |
| team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
| team_short_name | character | Team abbreviation |
| team_name | character | Team name as the league app displays it |
| number | character | Sweater number, a string |
| position | character | goalkeeper, defender or forwarder (sic); one of 502 rows in the full 2026 body has the integer 0 |
| topscorer_points | integer | PostFinance Top Scorer game score (an app fantasy rating, not on-ice points) |
| topscorer_point_average | numeric | PostFinance Top Scorer game score per game |
| topscorer_market_value | integer | PostFinance Top Scorer game market value |
| topscorer_market_value_trend | integer | Market-value trend: 1 up, 0 flat, -1 down |
| topscorer_amount | integer | CHF amount credited to the team's Top Scorer jersey holder (non-zero for 14 players, one per team) |
| is_topscorer | logical | Wears the team's PostFinance Top Scorer jersey (one per team) |
| nationality | character | Nationality; unobserved: null or empty in every sampled row |
| birth | character | Date of birth, YYYY-MM-DD |
| height | integer | Height; unobserved: null or empty in every sampled row |
| weight | integer | Weight; unobserved: null or empty in every sampled row |
| hand | character | Shooting or catching hand; unobserved: null or empty in every sampled row |
| lcrid | integer | Not determined (small integer, 0-4 in 2026) |
| noc | integer | Not determined (small integer, 0-2 in 2026) |
| gp | integer | Number of games played |
| g | integer | Number of goals scored |
| a1 | integer | First (primary) assists credited |
| a2 | integer | Second (secondary) assists credited |
| penalties | integer | Penalties taken (upstream `fo`: fo2 + fo5 + fo10 + gm + mp). Not faceoffs: see face_offs / fot. |
| pim | integer | Penalty minutes |
| geq | integer | Even-strength goals |
| gpp | integer | Power-play goals |
| gpk | integer | Shorthanded goals |
| gwg | integer | Game-winning goals |
| ps | integer | Shootout shots taken |
| pg | integer | Not determined; 0 on every 2026 row |
| pl | integer | Plus count: on ice for an even-strength or shorthanded goal for |
| mi | integer | Minus count: on ice for a goal against |
| toi | integer | Time on ice, seconds |
| toi_eq | integer | Even-strength time on ice, seconds |
| toipp | integer | Power-play time on ice, seconds (lower-case pp, unlike the box score's toiPp) |
| toi_pk | integer | Shorthanded time on ice, seconds |
| sog | integer | Number of shots on goal |
| shm | integer | Shots that missed the net |
| shp | integer | Shots that hit the post |
| shb | integer | Own shots blocked by an opponent |
| bks | integer | Opponent shots blocked |
| fow | integer | Number of faceoffs won |
| fol | integer | Number of faceoffs lost |
| shi | integer | Number of shifts played |
| gwgk | integer | Not determined; 0 on every 2026 row |
| gw | integer | Goaltender wins (incl. overtime and shootout) |
| gl | integer | Goaltender losses (incl. overtime and shootout) |
| fk | integer | Games started in goal (inferred: fk \<= gp; 42 of 44 for the 2026 games-played leader) |
| ga | integer | Goals against: number of goals conceded |
| sa | integer | Shots on goal against |
| svs | integer | Number of saves made by the goaltender |
| so | integer | Number of shutouts (games without a goal conceded) |
| mip | integer | Goaltender time played, hundredths of a minute (6000 = 60:00; = seconds / 0.6) |
| assists | integer | Assists (a1 + a2; verified on every 2026 row) |
| points | integer | Points (g + assists; verified) |
| goal_efficiency | numeric | Shooting percentage, 100 \* g / sog (verified); -1 with no shots |
| save_percentage | numeric | Save percentage, 0-100 |
| face_offs_percentage | numeric | Faceoff win percentage, 0-100; -1 with no faceoffs |
| face_offs | integer | Faceoffs taken (fow + fol; verified) |
| is_captain | logical | Whether the player is the team captain |
| first_goalkeeper | logical | Team's first-choice goaltender |
| ga_per_game | numeric | Goals against per game (ga / gp; verified) |
| points_per_game | numeric | Points per game |
| svs_per_game | numeric | Goaltender saves per game played |
| bks_per_game | numeric | Blocked shots per game |
| sog_per_game | numeric | Shots on goal per game |
| shm_per_game | numeric | Missed shots per game |
| shp_per_game | numeric | Post hits per game |
| fow_per_game | numeric | Faceoffs won per game |
| fol_per_game | numeric | Faceoffs lost per game |
| toi_eq_per_game | numeric | Even-strength time on ice per game, seconds |
| toi_pp_per_game | numeric | Power-play time on ice per game, seconds |
| toi_pk_per_game | numeric | Shorthanded time on ice per game, seconds |
| face_offs_per_game_percentage | numeric | Not determined; 0 on the sampled rows |
| toi_average | numeric | Time on ice per game, seconds |
| plus_minus | integer | Plus/minus (pl - mi; verified) |
| tgs | integer | Team's total game time, seconds (about 3600 per game incl. overtime) |
| sep | integer | Goaltender time played, seconds |
| toi_percentage | numeric | Share of the team's game time in goal, 100 \* sep / tgs (verified) |
| gwit | integer | Wins in regulation time (gw = gwit + overtime/shootout wins; verified) |
| glit | integer | Losses in regulation time (gl = glit + overtime/shootout losses; verified) |
| sag | integer | Shots on goal against that were saved (= svs on every 2026 goaltender row) |
| sago | integer | Shots on goal against that went in (= ga on every 2026 goaltender row) |
| sam | integer | Shots against that missed the net |
| sap | integer | Shots against that hit the post |
| sab | integer | Shots against that were blocked |
| saslot | integer | Slot shots against |
| sagoslot | integer | Slot shots against that went in |
| aeq | integer | Even-strength assists |
| app | integer | Power-play assists |
| apk | integer | Shorthanded assists |
| ha | integer | Hat tricks (inferred; at most 2 in 2026) |
| fo2 | integer | Minor (2-minute) penalties |
| fo5 | integer | Major (5-minute) penalties |
| fo10 | integer | 10-minute misconducts |
| gm | integer | Game misconducts |
| mp | integer | Match penalties |
| pm | integer | Not determined; 0 on every 2026 row |
| pwg | integer | Not determined; 0 on every 2026 row |
| sgo | integer | Not determined. Equals gff + gfd on every team row; skater values are close to but not always g |
| fowoz | integer | Offensive-zone faceoffs won |
| fownz | integer | Neutral-zone faceoffs won |
| fowdz | integer | Defensive-zone faceoffs won |
| foloz | integer | Offensive-zone faceoffs lost |
| folnz | integer | Neutral-zone faceoffs lost |
| foldz | integer | Defensive-zone faceoffs lost |
| psg | integer | Goals scored in shootouts |
| pssvs | integer | Saves made in shootouts |
| got | integer | Overtime goals (a subset of g) |
| ot_rank | integer | Overtime-scoring rank; 0 on the 2026 rows (filled in 2027) |
| ot_g | integer | Overtime goals in the overtime table; 0 on every 2026 row |
| ot_a1 | integer | Overtime first assists; 0 on every 2026 row |
| ot_a2 | integer | Overtime second assists; 0 on every 2026 row |
| ot_toi | integer | Overtime time on ice, seconds; 0 on every 2026 row |
| ot_a | integer | Overtime assists; 0 on every 2026 row |
| ot_p | integer | Overtime points; 0 on every 2026 row |
| psgwg | integer | Game-deciding shootout goals; 0 on every 2026 row |
| sogslot | integer | Shots on goal from the slot |
| pswg | integer | Shootout winners (inferred) |
| toishi | numeric | Time on ice per shift, seconds |
| shigp | numeric | Shifts per game |
| saslotgp | numeric | Slot shots against per game |
| samgp | numeric | Missed shots against per game |
| sapgp | numeric | Post hits against per game |
| svs_pslot | numeric | Slot save percentage, 100 \* (saslot - sagoslot) / saslot |
| sa_per_game | numeric | Shots on goal against per game |
| otw | integer | Goaltender overtime or shootout wins |
| otl | integer | Goaltender overtime or shootout losses |
| fopoz | numeric | Offensive-zone faceoff win percentage, 0-100 |
| fopnz | numeric | Neutral-zone faceoff win percentage, 0-100 |
| fopdz | numeric | Defensive-zone faceoff win percentage, 0-100 |
| fot | integer | Faceoffs taken (fow + fol) |
| fogp | numeric | Faceoffs taken per game |
| psm | integer | Shootout misses (inferred; ps = psg + psm fails for 67 of 447 skaters) |
| ps_percentage | numeric | Shootout success percentage, 0-100 |
| gw_shutout | integer | Not determined; equals gw on goaltender rows |
| sogslot_per_game | numeric | Slot shots on goal per game |
| psag | integer | Shootout goals against (equals ps on goaltender rows) |
| pssvs_percentage | numeric | Shootout save percentage, 0-100; -1 with no shootout shots faced |
| show_rank | logical | App flag: show the row's rank |
| chf | integer | CHF credited for the player's points: 300 \* points on every 2026 row (PostFinance youth-fund scheme; inferred) |
| team_stats | character | Per-team split for a player who changed teams; unobserved: null or empty in every sampled row |

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

Endpoint: `GET https://www.nationalleague.ch/api/player/{playerId}`.

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
  try(nl_player_season(phase = 4940, player_id = 305519, season = 2026))
#> ── National League Player Season Information from nationalleague.ch ────────────
#> ℹ Data updated: 2026-10-18 06:17:15 UTC
#> # A tibble: 1 × 145
#>   player_id first_name last_name team_id team_short_name team_name number
#>       <dbl> <chr>      <chr>       <dbl> <chr>           <chr>     <chr> 
#> 1    305519 Tobias     Geisser    101144 EVZ             EV Zug    42    
#> # ℹ 138 more variables: position <chr>, topscorer_points <int>,
#> #   topscorer_point_average <dbl>, topscorer_market_value <int>,
#> #   topscorer_market_value_trend <int>, topscorer_amount <int>,
#> #   is_topscorer <lgl>, nationality <chr>, birth <chr>, height <int>,
#> #   weight <int>, hand <chr>, lcrid <int>, noc <int>, gp <int>, g <int>,
#> #   a1 <int>, a2 <int>, penalties <int>, pim <int>, geq <int>, gpp <int>,
#> #   gpk <int>, gwg <int>, ps <int>, pg <int>, pl <int>, mi <int>, toi <int>, …
# }
```
