# **Get ESPN MCH / WCH Game Predictor (core-v2)**

**Get ESPN MCH / WCH Game Predictor (core-v2)**

## Usage

``` r
espn_mch_game_predictor(event_id, cid = event_id, ...)

espn_wch_game_predictor(event_id, cid = event_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with ESPN Predictor data. **Note:**
This endpoint is sparse for MCH - most games return an empty tibble with
a warning. When populated, scalar fields from the predictor object are
returned as character columns alongside `event_id` and `cid`.

Columns when populated:

|  |  |  |
|----|----|----|
| col_name | type | description |
| event_id | character | ESPN event identifier (echoed from the argument). |
| cid | character | Competition identifier (echoed from the argument). |
| `<field>` | character | One column per scalar field of the ESPN predictor object, cleaned by [`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html); `predictor_ref` (the `$ref` URL) when the object has no scalars. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_predictor()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_predictor.md)
(columns inherited); a collection may be empty.

`espn_wch_game_predictor()` is the same wrapper for women's college
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
    espn_mch_game_predictor(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-09 22:50:21.132257: Invalid arguments or no ESPN mens-college-hockey game predictor for event 401715296 available!
#> ✖ Args: league = "mens-college-hockey", event_id = "401715296", cid = "401715296"
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try({
    sb <- espn_wch_scoreboard(dates = "20250110")
    espn_wch_game_predictor(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-09 22:50:21.190238: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
