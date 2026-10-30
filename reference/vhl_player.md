# VHL Player Card

Player card: position, height, weight, birth date, age, country and
photo.

## Usage

``` r
vhl_player(player_id, return_parsed = TRUE)
```

## Arguments

- player_id:

  Player id (player_id in any VHL frame), e.g. 34526.

- return_parsed:

  `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's
  parsed record (a named list, the sdv-internal-refs capture shape, plus
  `_request` with the season, stage, tn and the ids asked for).

## Value

A `fastRhockey_data` tibble with one row per player (one row):

|  |  |  |
|----|----|----|
| col_name | type | description |
| `player_id` | integer | Player id, read from the page's own `/players/{id}/games/` tab link; equals the requested id or the capture refuses. |
| `name` | character | Player name, 'Surname Given' (Latin), from h1 without the number. |
| `jersey` | integer | Current jersey number (the h1 '#79' badge); null when the page shows none. |
| `position` | character | Position in English (h2.player-card\_\_subtitle): Forward and Goalkeeper seen; the defence label is unverified. |
| `height_cm` | integer | Height in centimetres (Height). |
| `weight_kg` | integer | Weight in kilograms (Weight). |
| `born` | character | Birth date 'DD.MM.YYYY' as the public page prints it. |
| `age` | integer | Age in years on the capture date (from '24 years old'). |
| `country` | character | Country (English); null when the card omits it. |
| `photo_url` | character | Photo URL on img.khl.ru, `/teamplayers/{team_season_id}/{player_id}/320.jpg`. The team-season id is the one the photo was filed under, not the current team: 34526's photo uses 15424 = Magnitka in tn 1375 (25/26 playoffs, verified on /en/stats/players/1375/) while his newest row is 15516 (Dynamo-Altai, 1430). |

With `return_parsed = FALSE`, the page's parsed record (a named list)
instead.

## Details

Source: `GET https://www.vhlru.ru/en/players/{player_id}/`
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
[`vhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/vhl_player_box.md),
[`vhl_player_career()`](https://fastRhockey.sportsdataverse.org/reference/vhl_player_career.md),
[`vhl_roster()`](https://fastRhockey.sportsdataverse.org/reference/vhl_roster.md),
[`vhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/vhl_schedule.md),
[`vhl_skater_stats()`](https://fastRhockey.sportsdataverse.org/reference/vhl_skater_stats.md),
[`vhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/vhl_standings.md),
[`vhl_tn()`](https://fastRhockey.sportsdataverse.org/reference/vhl_tn.md)

## Examples

``` r
# \donttest{
  try(vhl_player(player_id = 34526))
#> ── VHL Player Information from vhlru.ru ────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:21:16 UTC
#> # A tibble: 1 × 10
#>   player_id name         jersey position height_cm weight_kg born    age country
#>       <int> <chr>         <int> <chr>        <int>     <int> <chr> <int> <chr>  
#> 1     34526 Mirzabalaye…     NA Forward        182        79 27.0…    24 NA     
#> # ℹ 1 more variable: photo_url <chr>
# }
```
