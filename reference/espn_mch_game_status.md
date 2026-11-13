# **Get ESPN MCH / WCH Game Status (core-v2)**

**Get ESPN MCH / WCH Game Status (core-v2)**

## Usage

``` r
espn_mch_game_status(event_id, cid = event_id, ...)

espn_wch_game_status(event_id, cid = event_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier. Use `espn_mch_scoreboard()$game_id` to
  find valid event ids.

- cid:

  Competition identifier. Defaults to `event_id`.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with game status detail:

|                   |           |                                          |
|-------------------|-----------|------------------------------------------|
| col_name          | types     | description                              |
| event_id          | character | ESPN event id (echoed from arg).         |
| cid               | character | Competition id (echoed from arg).        |
| clock             | numeric   | Game clock value in seconds.             |
| display_clock     | character | Game clock display string (e.g. "0:00"). |
| period            | integer   | Current or final period number.          |
| type_id           | character | Status type identifier.                  |
| type_name         | character | Status type name (e.g. "STATUS_FINAL").  |
| type_state        | character | Status state (e.g. "pre", "in", "post"). |
| type_completed    | logical   | Whether the game is complete.            |
| type_description  | character | Status description (e.g. "Final").       |
| type_detail       | character | Status detail string.                    |
| type_short_detail | character | Short status detail string.              |
| status_ref        | character | `$ref` URL for this status object.       |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_status()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_status.md)
(columns inherited); a collection may be empty.

`espn_wch_game_status()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

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
    espn_mch_game_status(event_id = sb$game_id[1])
  })
#> ── MENS-COLLEGE-HOCKEY Game Status data from ESPN core-v2 ──────────────────────
#> ℹ Data updated: 2026-11-13 20:15:55 UTC
#> # A tibble: 1 × 13
#>   event_id  cid       clock display_clock period type_id type_name    type_state
#>   <chr>     <chr>     <dbl> <chr>          <int> <chr>   <chr>        <chr>     
#> 1 401715296 401715296     0 0:00               3 3       STATUS_FINAL post      
#> # ℹ 5 more variables: type_completed <lgl>, type_description <chr>,
#> #   type_detail <chr>, type_short_detail <chr>, status_ref <chr>
# }
# \donttest{
  try({
    sb <- espn_wch_scoreboard(dates = "20250110")
    espn_wch_game_status(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-13 20:15:55.163206: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
