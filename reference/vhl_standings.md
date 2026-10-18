# VHL Standings

League standings of one regular season (/standings/league/;
/standings/regular/ ignores the season).

## Usage

``` r
vhl_standings(season, return_parsed = TRUE)
```

## Arguments

- season:

  Season END year (2026 = 2025/26; the calendar covers 2010/11 on). With
  `stage` it is mapped to the site's tournament id, see
  [`vhl_tn()`](https://fastRhockey.sportsdataverse.org/reference/vhl_tn.md).

- return_parsed:

  `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's
  parsed record (a named list, the sdv-internal-refs capture shape, plus
  `_request` with the season, stage, tn and the ids asked for).

## Value

A `fastRhockey_data` tibble with one row per team:

|  |  |  |
|----|----|----|
| col_name | type | description |
| `season` | integer | Season END year (2026 = 2025/26). |
| `stage` | character | Season stage: regular or playoff. |
| `tn` | integer | Tournament id of the season stage (2025/26 regular season = 1374, playoffs = 1375); vhl_tn maps season + stage to it. |
| `rank` | integer | Rank (unlabelled first column). |
| `team` | character | Team short name (Latin; the report tables' form). |
| `team_season_id` | integer | Team-season id (khl/ terminology): one id per team per tournament, from the KHL's sequence (khl/: Spartak = 14934 in 1369; here Metallurg = 15017 in 1374). The `{id}` of `/calendar/{tn}/0/{id}/` and `/stats/players/{tn}/all/{id}/`. Not the stable club id (\*\_club_id) and not the team page id. |
| `games_played` | integer | Games played (GP). |
| `wins` | integer | Regulation wins (W). |
| `ot_wins` | integer | Overtime wins (OTW). |
| `so_wins` | integer | Shootout wins (SOW). |
| `so_losses` | integer | Shootout losses (SOL). |
| `ot_losses` | integer | Overtime losses (OTL). |
| `losses` | integer | Regulation losses (L). |
| `goals_for` | integer | Goals for (left side of the G column '226-131'). |
| `goals_against` | integer | Goals against (right side of the G column). |
| `points` | integer | Points (PTS): 2 per win of any kind, 1 per OT or SO loss. |

With `return_parsed = FALSE`, the page's parsed record (a named list)
instead.

## Details

Regular season only: the site's playoff standings are not read.

Source: `GET https://www.vhlru.ru/en/standings/league/{season}/`
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
[`vhl_roster()`](https://fastRhockey.sportsdataverse.org/reference/vhl_roster.md),
[`vhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/vhl_schedule.md),
[`vhl_skater_stats()`](https://fastRhockey.sportsdataverse.org/reference/vhl_skater_stats.md),
[`vhl_tn()`](https://fastRhockey.sportsdataverse.org/reference/vhl_tn.md)

## Examples

``` r
# \donttest{
  try(vhl_standings(season = 2026))
#> ── VHL Standings Information from vhlru.ru ─────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:12:59 UTC
#> # A tibble: 32 × 16
#>    season stage      tn  rank team     team_season_id games_played  wins ot_wins
#>     <int> <chr>   <int> <int> <chr>             <int>        <int> <int>   <int>
#>  1   2026 regular  1374     1 Metallu…          15017           62    40       5
#>  2   2026 regular  1374     2 Ugra              15035           62    39       4
#>  3   2026 regular  1374     3 Neftyan…          15019           62    34       3
#>  4   2026 regular  1374     4 Khimik            15029           62    35       3
#>  5   2026 regular  1374     5 Magnitka          15016           62    26       7
#>  6   2026 regular  1374     6 Ryazan-…          15024           62    30       4
#>  7   2026 regular  1374     7 Omskie …          15021           62    30       4
#>  8   2026 regular  1374     8 Gornyak…          15008           62    25       3
#>  9   2026 regular  1374     9 Molot             15018           62    22       4
#> 10   2026 regular  1374    10 Chelmet           15033           62    30       3
#> # ℹ 22 more rows
#> # ℹ 7 more variables: so_wins <int>, so_losses <int>, ot_losses <int>,
#> #   losses <int>, goals_for <int>, goals_against <int>, points <int>
# }
```
