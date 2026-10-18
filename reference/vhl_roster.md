# VHL Team Roster

CURRENT roster of a team page (player names in Cyrillic even on /en/).

## Usage

``` r
vhl_roster(team_page_id, return_parsed = TRUE)
```

## Arguments

- team_page_id:

  Team page id (home_team_page_id / away_team_page_id in vhl_game_info),
  e.g. 390. Neither a club id nor a team-season id.

- return_parsed:

  `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's
  parsed record (a named list, the sdv-internal-refs capture shape, plus
  `_request` with the season, stage, tn and the ids asked for).

## Value

A `fastRhockey_data` tibble with one row per player:

|  |  |  |
|----|----|----|
| col_name | type | description |
| `team_page_id` | integer | Team page id (vhlru.ru `/en/teams/{team_page_id}/`), stable across seasons; for report rows the page id of the row's side. |
| `team` | character | Team short name (Latin; the report tables' form). |
| `calendar_tn` | integer | Tournament id of the team's CURRENT season stage (the page's 'To the calendar' link). |
| `calendar_team_season_id` | integer | The team's team-season id in calendar_tn. |
| `roster_as_of` | character | Date the roster is valid for, as printed ('12.10.2026'). |
| `jersey` | integer | Jersey number (integer). |
| `position` | character | Position in English, lower case: goaltender, defense, forward. |
| `player_id` | integer | Player id: the `{player_id}` of `/en/players/{player_id}/`, stable across seasons and teams; also the photo id on img.khl.ru. Same numeric sequence as khl.ru player ids; whether it equals the same person's khl.ru id is unverified. |
| `player` | character | Player name, 'Surname Given' — in CYRILLIC even on the /en/ page. |
| `born` | character | Birth date 'DD.MM.YYYY' as the public page prints it. |
| `country` | character | Citizenship (English). |

With `return_parsed = FALSE`, the page's parsed record (a named list)
instead.

## Details

The roster is the team page's CURRENT one (names are Cyrillic even on
`/en/`), not a season's.

Source: `GET https://www.vhlru.ru/en/teams/{team_page_id}/`
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
[`vhl_player_career()`](https://fastRhockey.sportsdataverse.org/reference/vhl_player_career.md),
[`vhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/vhl_schedule.md),
[`vhl_skater_stats()`](https://fastRhockey.sportsdataverse.org/reference/vhl_skater_stats.md),
[`vhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/vhl_standings.md),
[`vhl_tn()`](https://fastRhockey.sportsdataverse.org/reference/vhl_tn.md)

## Examples

``` r
# \donttest{
  try(vhl_roster(team_page_id = 390))
#> ── VHL Roster Information from vhlru.ru ────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:12:45 UTC
#> # A tibble: 26 × 11
#>    team_page_id team  calendar_tn calendar_team_season_id roster_as_of jersey
#>           <int> <chr>       <int>                   <int> <chr>         <int>
#>  1          390 Toros        1430                   15531 18.10.2026       28
#>  2          390 Toros        1430                   15531 18.10.2026       97
#>  3          390 Toros        1430                   15531 18.10.2026       31
#>  4          390 Toros        1430                   15531 18.10.2026       25
#>  5          390 Toros        1430                   15531 18.10.2026       71
#>  6          390 Toros        1430                   15531 18.10.2026       36
#>  7          390 Toros        1430                   15531 18.10.2026       49
#>  8          390 Toros        1430                   15531 18.10.2026       46
#>  9          390 Toros        1430                   15531 18.10.2026       19
#> 10          390 Toros        1430                   15531 18.10.2026       88
#> # ℹ 16 more rows
#> # ℹ 5 more variables: position <chr>, player_id <int>, player <chr>,
#> #   born <chr>, country <chr>
# }
```
