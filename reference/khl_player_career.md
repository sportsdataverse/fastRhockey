# **KHL Player Career**

Player career: one row per season tournament.

## Usage

``` r
khl_player_career(player_id, headers = NULL, return_parsed = TRUE)
```

## Arguments

- player_id:

  Required. khl.ru player id (the box score / roster player_id).

- headers:

  Named list of extra request headers, merged over the defaults
  (`X-Requested-With`, `Origin`, `Referer`). The cookie jar and `sessid`
  are kept in-process, so nothing needs reusing across calls.

- return_parsed:

  Logical. `TRUE` (default) returns the parsed `fastRhockey_data`
  tibble; `FALSE` returns the raw khl.ru envelope as a list (`status`,
  `data`, `errors`; betting fields removed).

## Value

A `fastRhockey_data` tibble (40 columns; zero rows with the same columns
when the payload is empty), or the raw body as a list when
`return_parsed = FALSE`. Raises `fastRhockey_no_data` (nothing there, or
an unknown season / stage), `fastRhockey_asset_fetch` (an error
envelope, an answer for another id than the one asked, or a failed
fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP 400
/ 422).

|  |  |  |
|----|----|----|
| col_name | types | description |
| player_id | integer | khl.ru player id; equals webcaster player khl_id |
| season_label | character | Season label as sent (26/27, 2025/2026) |
| tournament_id | integer | khl.ru tournament (stage) id, tnId: 1369 = 2025-26 regular season, 1370 its playoff; equals webcaster stage khl_id |
| team_season_id | integer | Team-season id (tid; one per tournament; joins nothing in the webcaster API) |
| club_id | integer | khl.ru CLUB id (stable across seasons; joins webcaster team khl_id) |
| team_name | character | Team name as the site prints it |
| jn | integer | Jersey number (not an id; 0 on a team event) |
| gp | integer | Games played by the player |
| g | integer | Goals scored by the player |
| a | integer | Assists credited to the player |
| pts | integer | Points (goals plus assists) |
| pm | integer | Plus-minus rating of the player |
| plus | integer | Plus (on ice for a goal for) |
| minus | integer | Minus (on ice for a goal against) |
| pim | integer | Penalty minutes |
| eqg | integer | Even-strength goals |
| ppg | integer | Power-play goals |
| shg | integer | Short-handed goals |
| otg | integer | Overtime goals scored by the player |
| gwg | integer | Game-winning goals |
| sds | integer | Shootout-deciding goals |
| sog | integer | Shots on goal by the player |
| sog_pct | numeric | Shooting percentage |
| sog_avg | numeric | Shots on goal per game |
| fo | integer | Faceoffs taken by the player |
| fow | integer | Faceoffs won by the player |
| fo_pct | numeric | Faceoff win percentage |
| toi_avg | character | Average time on ice per game (mm:ss) |
| sft_avg | numeric | Average shifts per game |
| tie_avg | character | Average even-strength time on ice per game |
| sfte_avg | numeric | Average even-strength shifts per game |
| tipp_avg | character | Average power-play time on ice per game |
| sftpp_avg | numeric | Average power-play shifts per game |
| tish_avg | character | Average short-handed time on ice per game |
| sftsh_avg | numeric | Average short-handed shifts per game |
| hits | integer | Hits (body checks) |
| bls | integer | Shots blocked by the player |
| foa | integer | Faceoffs lost by the player |
| tka | integer | Takeaways by the player |
| p_intc | integer | Pass interceptions |

## Details

Endpoint: `POST https://www.khl.ru/rest/players/stats/`.

khl.ru REST needs a primed session (a GET of the site root sets the
cookie jar and the Bitrix `sessid`, then each call POSTs a form); this
is done for you, one session per host per R session, re-primed when the
site rotates its cookie or the `sessid` expires. No route takes a year:
`season` (the END year, 2026 = 2025/26) and `stage` are mapped to the
tournament id (tnId) through the webcaster season table, or pass
`tournament_id` (the All-Star games have their own tnId).

Ids and counts are integers (sdv-py Int64); a column holding values past
2^31 (epoch milliseconds) is returned as a whole-number double.

At least one second is left between two requests to a host.

**Owner flag (sanctions).** The KHL and its webcaster platform are run
by Russian operators, some clubs by sanctioned state companies. Fetching
on your own machine is fine; any published data release, hosted mirror
or redistribution needs owner (sanctions / legal) sign-off first.

## See also

Other KHL Functions:
[`khl_game_header()`](https://fastRhockey.sportsdataverse.org/reference/khl_game_header.md),
[`khl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/khl_game_info.md),
[`khl_game_protocol()`](https://fastRhockey.sportsdataverse.org/reference/khl_game_protocol.md),
[`khl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/khl_pbp.md),
[`khl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_box.md),
[`khl_player_profile()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_profile.md),
[`khl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_stats.md),
[`khl_playoff_bracket()`](https://fastRhockey.sportsdataverse.org/reference/khl_playoff_bracket.md),
[`khl_roster()`](https://fastRhockey.sportsdataverse.org/reference/khl_roster.md),
[`khl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/khl_schedule.md),
[`khl_shots()`](https://fastRhockey.sportsdataverse.org/reference/khl_shots.md),
[`khl_standings()`](https://fastRhockey.sportsdataverse.org/reference/khl_standings.md),
[`khl_stat_filters()`](https://fastRhockey.sportsdataverse.org/reference/khl_stat_filters.md),
[`khl_webcaster_event()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_event.md),
[`khl_webcaster_events()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_events.md),
[`khl_webcaster_leaders()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_leaders.md),
[`khl_webcaster_players()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_players.md),
[`khl_webcaster_players_light()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_players_light.md),
[`khl_webcaster_stages()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_stages.md),
[`khl_webcaster_tables()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_tables.md),
[`khl_webcaster_team()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_team.md),
[`khl_webcaster_teams()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_teams.md)

## Examples

``` r
# \donttest{
  try(khl_player_career(player_id = 41928))
#> ── KHL Player Career from khl.ru ───────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-13 01:54:46 UTC
#> # A tibble: 6 × 40
#>   player_id season_label tournament_id team_season_id club_id team_name    jn
#>       <int> <chr>                <int>          <int>   <int> <chr>     <int>
#> 1     41928 26/27                 1436          15558       7 Спартак       4
#> 2     41928 25/26                 1369          14934       7 Спартак       4
#> 3     41928 25/26                 1370          15461       7 Спартак       4
#> 4     41928 23/24                 1217          13612       7 Спартак       4
#> 5     41928 23/24                 1218          14130       7 Спартак       4
#> 6     41928 22/23                 1154          13026       7 Спартак       4
#> # ℹ 33 more variables: gp <int>, g <int>, a <int>, pts <int>, pm <int>,
#> #   plus <int>, minus <int>, pim <int>, eqg <int>, ppg <int>, shg <int>,
#> #   otg <int>, gwg <int>, sds <int>, sog <int>, sog_pct <dbl>, sog_avg <dbl>,
#> #   fo <int>, fow <int>, fo_pct <dbl>, toi_avg <chr>, sft_avg <dbl>,
#> #   tie_avg <chr>, sfte_avg <dbl>, tipp_avg <chr>, sftpp_avg <dbl>,
#> #   tish_avg <chr>, sftsh_avg <dbl>, hits <int>, bls <int>, foa <int>,
#> #   tka <int>, p_intc <int>
# }
```
