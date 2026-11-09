# **Get ESPN NHL Player vs Player (core-v2)**

**Get ESPN NHL Player vs Player (core-v2)**

## Usage

``` r
espn_nhl_player_vs_player(athlete_id, opponent_id, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric).

- opponent_id:

  ESPN opponent athlete identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

An empty [`data.frame()`](https://rdrr.io/r/base/data.frame.html). As of
2026-06-08, ESPN core-v2 `athletes/{id}/vsathlete/{opp_id}` returns HTTP
404 for all queried NHL player pairs (DONE_WITH_CONCERNS: endpoint not
served for NHL).

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_player_vs_player(athlete_id = "4024820", opponent_id = "3114"))
#> ! espn_nhl_player_vs_player(): ESPN core-v2 athletes/{id}/vsathlete/{opp} returns HTTP 404 for the requested NHL athlete pair (DONE_WITH_CONCERNS: endpoint not served for NHL).
#> data frame with 0 columns and 0 rows
# }
```
