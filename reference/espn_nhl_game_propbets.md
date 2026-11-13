# **Get ESPN NHL Game Prop Bets (core-v2)**

**Get ESPN NHL Game Prop Bets (core-v2)**

## Usage

``` r
espn_nhl_game_propbets(event_id, cid = event_id, ...)
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

A `fastRhockey_data` tibble with one row per prop bet. **Note:** this
endpoint returns 404 for most NHL games; an empty
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
    espn_nhl_game_propbets(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-13 20:16:32.716715: Invalid arguments or no ESPN nhl propbets for event 401688263 available!
#> ✖ Args: league = "nhl", event_id = "401688263", cid = "401688263"
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
