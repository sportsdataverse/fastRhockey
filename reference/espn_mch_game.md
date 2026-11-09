# **Get ESPN MCH / WCH Game Metadata (core-v2)**

**Get ESPN MCH / WCH Game Metadata (core-v2)**

## Usage

``` r
espn_mch_game(event_id, ...)

espn_wch_game(event_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier. Use `espn_mch_scoreboard()$game_id` to
  find valid event ids.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with event metadata:

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| id | character | ESPN event identifier. |
| uid | character | Event uid string. |
| date | character | Game date/time (ISO 8601 UTC). |
| name | character | Full game name (e.g. "Chicago Blackhawks at Detroit Red Wings"). |
| short_name | character | Short name (e.g. "CHI \\ DET"). |
| time_valid | logical | Whether the start time is confirmed. |
| competition_id | character | Id of the primary competition (equals `event_id` for MCH). |
| competition_ref | character | `$ref` URL for the primary competition object. |
| venue_ref | character | `$ref` URL for the venue. |
| season_ref | character | `$ref` URL for the season. |
| season_type_ref | character | `$ref` URL for the season type. |
| league_ref | character | `$ref` URL for the league. |
| event_ref | character | `$ref` URL for this event object. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game.md)
(columns inherited); a collection may be empty.

`espn_wch_game()` is the same wrapper for women's college hockey (league
slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    sb <- espn_mch_scoreboard(dates = "20250110")
    espn_mch_game(event_id = sb$game_id[1])
  })
#> ── MENS-COLLEGE-HOCKEY Game data from ESPN core-v2 ──────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 22:50:15 UTC
#> # A tibble: 1 × 14
#>   event_id  id        uid       date  name  short_name time_valid competition_id
#>   <chr>     <chr>     <chr>     <chr> <chr> <chr>      <lgl>      <chr>         
#> 1 401715296 401715296 s:70~l:9… 2025… Minn… MINN @ OSU TRUE       401715296     
#> # ℹ 6 more variables: competition_ref <chr>, venue_ref <chr>, season_ref <chr>,
#> #   season_type_ref <chr>, league_ref <chr>, event_ref <chr>
# }
# \donttest{
  try({
    sb <- espn_wch_scoreboard(dates = "20250110")
    espn_wch_game(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-09 22:50:15.219398: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL
#> ✖ Error: arguments imply differing number of rows: 0, 1
#> data frame with 0 columns and 0 rows
# }
```
