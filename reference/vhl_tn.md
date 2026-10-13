# VHL Tournament Id of a Season

The VHL tournament id (`tn`) of a season (END year) and stage. Read from
the calendar's tournament select, fetched once per R session. The tn is
what the site keys every season page by; the wrappers map it for you, so
you need it only to read `tn` columns or build a URL by hand.

## Usage

``` r
vhl_tn(season, stage = "regular")
```

## Arguments

- season:

  Season END year (2026 = 2025/26). The calendar covers 2010/11 on.

- stage:

  `"regular"` (default) or `"playoff"`.

## Value

An integer: the tn (`1374` for the 2025/26 regular season, `1375` for
its playoffs). A length-one vector, not a data frame:

|          |         |                             |
|----------|---------|-----------------------------|
| col_name | type    | description                 |
| (value)  | integer | The tournament number (tn). |

## Details

Source: `GET https://www.vhlru.ru/en/calendar/` (server-rendered HTML,
keyless; parsed with rvest).

## Owner flag (sanctions)

The VHL is run by Russian league operators, and several clubs are owned
or sponsored by sanctioned state companies. Fetching on your own machine
is fine; a published data release or a hosted mirror of this data needs
the owner's sanctions / legal sign-off first.

## Errors

- `fastRhockey_no_data`: the calendar lists no such season and stage.

- `fastRhockey_bad_request`: `season` is not a year or `stage` is not
  `"regular"` / `"playoff"`.

- `fastRhockey_asset_fetch`: the calendar page could not be fetched or
  has no tournament select.

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
[`vhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/vhl_standings.md)

## Examples

``` r
# \donttest{
  try(vhl_tn(2026))
#> [1] 1374
  try(vhl_tn(2026, "playoff"))
#> [1] 1375
# }
```
