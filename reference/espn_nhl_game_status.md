# **Get ESPN NHL Game Status (core-v2)**

**Get ESPN NHL Game Status (core-v2)**

## Usage

``` r
espn_nhl_game_status(event_id, cid = event_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier. Use `espn_nhl_scoreboard()$game_id` to
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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    sb <- espn_nhl_scoreboard(dates = "20250110")
    espn_nhl_game_status(event_id = sb$game_id[1])
  })
#> ── NHL Game Status data from ESPN core-v2 ───────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:19:10 UTC
#> # A tibble: 1 × 13
#>   event_id  cid       clock display_clock period type_id type_name    type_state
#>   <chr>     <chr>     <dbl> <chr>          <int> <chr>   <chr>        <chr>     
#> 1 401688263 401688263     0 0:00               3 3       STATUS_FINAL post      
#> # ℹ 5 more variables: type_completed <lgl>, type_description <chr>,
#> #   type_detail <chr>, type_short_detail <chr>, status_ref <chr>
# }
```
