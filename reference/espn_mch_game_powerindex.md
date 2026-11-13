# **Get ESPN MCH / WCH Game Power Index (core-v2)**

**Get ESPN MCH / WCH Game Power Index (core-v2)**

## Usage

``` r
espn_mch_game_powerindex(event_id, cid = event_id, ...)

espn_wch_game_powerindex(event_id, cid = event_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with ESPN Power Index data. **Note:** This
endpoint is sparse for MCH - most games return an empty tibble with a
warning. When populated, columns contain power-index values alongside
`event_id` and `cid`.

Columns when populated:

|  |  |  |
|----|----|----|
| col_name | type | description |
| event_id | character | ESPN event identifier (echoed from the argument). |
| cid | character | Competition identifier (echoed from the argument). |
| pi_ref | character | `$ref` URL of the power-index item (list responses). |
| `<field>` | character | One column per scalar field of the ESPN power-index object (object responses), cleaned by [`janitor::clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.html); `powerindex_ref` when it has no scalars. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_game_powerindex()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_game_powerindex.md)
(columns inherited); a collection may be empty.

`espn_wch_game_powerindex()` is the same wrapper for women's college
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
    espn_mch_game_powerindex(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-13 21:26:10.282351: Invalid arguments or no ESPN mens-college-hockey game power index for event 401715296 available!
#> ✖ Args: league = "mens-college-hockey", event_id = "401715296", cid = "401715296"
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try({
    sb <- espn_wch_scoreboard(dates = "20250110")
    espn_wch_game_powerindex(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-13 21:26:10.342435: 
#> ✖ Args: league = "womens-college-hockey", event_id = NULL, cid = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
