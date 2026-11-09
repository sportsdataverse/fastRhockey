# **Get ESPN NHL Game Predictor (core-v2)**

**Get ESPN NHL Game Predictor (core-v2)**

## Usage

``` r
espn_nhl_game_predictor(event_id, cid = event_id, ...)
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
This endpoint is sparse for NHL - most games return an empty tibble with
a warning. When populated, scalar fields from the predictor object are
returned as character columns alongside `event_id` and `cid`.

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
    espn_nhl_game_predictor(event_id = sb$game_id[1])
  })
#> ✖ 2026-11-09 17:19:08.213844: Invalid arguments or no ESPN nhl game predictor for event 401688263 available!
#> ✖ Args: league = "nhl", event_id = "401688263", cid = "401688263"
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
