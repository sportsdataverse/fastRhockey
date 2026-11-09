# **Get ESPN NHL Game Power Index (core-v2)**

**Get ESPN NHL Game Power Index (core-v2)**

## Usage

``` r
espn_nhl_game_powerindex(event_id, cid = event_id, ...)
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
endpoint is sparse for NHL - most games return an empty tibble with a
warning. When populated, columns contain power-index values alongside
`event_id` and `cid`.

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
    espn_nhl_game_powerindex(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-09 22:51:07.616039: Invalid arguments or no ESPN nhl game power index for event 401688263 available!
#> ✖ Args: league = "nhl", event_id = "401688263", cid = "401688263"
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
