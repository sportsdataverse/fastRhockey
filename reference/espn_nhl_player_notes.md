# **Get ESPN NHL Player Notes (core-v2)**

**Get ESPN NHL Player Notes (core-v2)**

## Usage

``` r
espn_nhl_player_notes(athlete_id, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

An empty [`data.frame()`](https://rdrr.io/r/base/data.frame.html). As of
2026-06-08, ESPN core-v2 `athletes/{{id}}/notes` returns HTTP 404 for
all queried NHL players (ESPN does not serve this endpoint).

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_player_notes(athlete_id = "4024820"))
#> ! espn_nhl_player_notes(): ESPN core-v2 athletes/{id}/notes is not available for the requested athlete (ESPN answers 404 for most NHL athletes).
#> ! Error: HTTP 404 Not Found from https://sports.core.api.espn.com/v2/sports/hockey/leagues/nhl/athletes/4024820/notes
#> data frame with 0 columns and 0 rows
# }
```
