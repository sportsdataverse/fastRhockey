# **Get ESPN MCH / WCH Game Prop Bets (core-v2)**

**Get ESPN MCH / WCH Game Prop Bets (core-v2)**

## Usage

``` r
espn_mch_game_propbets(event_id, cid = event_id, ...)

espn_wch_game_propbets(event_id, cid = event_id, ...)
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

A `fastRhockey_data` tibble with one row per prop bet. **Note:** this
endpoint returns 404 for most MCH games; an empty
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) is returned in
that case. When data is available (e.g. playoff games):

|               |           |                                    |
|---------------|-----------|------------------------------------|
| col_name      | types     | description                        |
| event_id      | character | ESPN event id (echoed from arg).   |
| cid           | character | Competition id (echoed from arg).  |
| id            | character | Prop bet identifier.               |
| name          | character | Prop bet name.                     |
| description   | character | Prop bet description.              |
| display_value | character | Prop bet display value.            |
| value         | numeric   | Prop bet numeric value.            |
| propbet_ref   | character | `$ref` URL for this prop bet item. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_propbets()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_propbets.md)
(columns inherited); a collection may be empty.

`espn_wch_game_propbets()` is the same wrapper for women's college
hockey (league slug `womens-college-hockey`): same arguments, same
columns.

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
    espn_mch_game_propbets(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-09 17:18:21.22423: Invalid arguments or no ESPN mens-college-hockey propbets for event 401715296 available!
#> ✖ Args: league = "mens-college-hockey", event_id = "401715296", cid = "401715296"
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try({
    sb <- espn_wch_scoreboard(dates = "20250110")
    espn_wch_game_propbets(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-09 17:18:21.267843: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
