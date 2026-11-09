# **Get ESPN MCH / WCH Player vs Player (core-v2)**

**Get ESPN MCH / WCH Player vs Player (core-v2)**

## Usage

``` r
espn_mch_player_vs_player(athlete_id, opp_id, ...)

espn_wch_player_vs_player(athlete_id, opp_id, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric).

- opp_id:

  ESPN opponent athlete identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

An empty [`data.frame()`](https://rdrr.io/r/base/data.frame.html). As of
2026-06-08, ESPN core-v2 `athletes/{id}/vsathlete/{opp_id}` returns HTTP
404 for all queried NHL player pairs (DONE_WITH_CONCERNS: endpoint not
served for NHL). There is no column schema: the call always yields a
zero-row, zero-column
[`data.frame()`](https://rdrr.io/r/base/data.frame.html).

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_player_vs_player()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_vs_player.md)
(columns inherited); a collection may be empty.

`espn_wch_player_vs_player()` is the same wrapper for women's college
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
  try(espn_mch_player_vs_player(athlete_id = "5136656", opp_id = "5136656"))
#> ! espn_nhl_player_vs_player(): ESPN core-v2 athletes/{id}/vsathlete/{opp} returns HTTP 404 for the requested NHL athlete pair (DONE_WITH_CONCERNS: endpoint not served for NHL).
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_player_vs_player(athlete_id = "5263060", opp_id = "5263060"))
#> ! espn_nhl_player_vs_player(): ESPN core-v2 athletes/{id}/vsathlete/{opp} returns HTTP 404 for the requested NHL athlete pair (DONE_WITH_CONCERNS: endpoint not served for NHL).
#> data frame with 0 columns and 0 rows
# }
```
