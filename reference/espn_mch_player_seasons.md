# **Get ESPN MCH / WCH Player Seasons (core-v2)**

**Get ESPN MCH / WCH Player Seasons (core-v2)**

## Usage

``` r
espn_mch_player_seasons(athlete_id, ...)

espn_wch_player_seasons(athlete_id, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

An empty [`data.frame()`](https://rdrr.io/r/base/data.frame.html). As of
2026-06-08, ESPN core-v2 `athletes/{{id}}/seasons` returns HTTP 404 for
all queried NHL players (DONE_WITH_CONCERNS: endpoint is not served for
the NHL). For season-level data use
[`espn_mch_player_statisticslog()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_player_statisticslog.md).
There is no column schema: the call always yields a zero-row,
zero-column [`data.frame()`](https://rdrr.io/r/base/data.frame.html).

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_player_seasons()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_seasons.md)
(columns inherited); a collection may be empty.

`espn_wch_player_seasons()` is the same wrapper for women's college
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
  try(espn_mch_player_seasons(athlete_id = "5136656"))
#> ! espn_nhl_player_seasons(): ESPN core-v2 athletes/{id}/seasons returns HTTP 404 for the requested NHL athlete (DONE_WITH_CONCERNS: endpoint not served for NHL).
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_player_seasons(athlete_id = "5263060"))
#> ! espn_nhl_player_seasons(): ESPN core-v2 athletes/{id}/seasons returns HTTP 404 for the requested NHL athlete (DONE_WITH_CONCERNS: endpoint not served for NHL).
#> data frame with 0 columns and 0 rows
# }
```
