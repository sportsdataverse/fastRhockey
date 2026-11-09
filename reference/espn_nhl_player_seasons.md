# **Get ESPN NHL Player Seasons (core-v2)**

**Get ESPN NHL Player Seasons (core-v2)**

## Usage

``` r
espn_nhl_player_seasons(athlete_id, ...)
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
[`espn_nhl_player_statisticslog()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_statisticslog.md).

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_player_seasons(athlete_id = "4024820"))
#> ! espn_nhl_player_seasons(): ESPN core-v2 athletes/{id}/seasons returns HTTP 404 for the requested NHL athlete (DONE_WITH_CONCERNS: endpoint not served for NHL).
#> data frame with 0 columns and 0 rows
# }
```
