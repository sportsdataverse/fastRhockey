# **Get ESPN NHL Team Transactions**

**Get ESPN NHL Team Transactions**

## Usage

``` r
espn_nhl_team_transactions(team_id, ...)
```

## Arguments

- team_id:

  ESPN team identifier (character or numeric, e.g. `"4"` for Chicago
  Blackhawks).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per transaction when data is
available. The `/transactions` endpoint currently returns an empty
response for NHL; an empty tibble is returned with a cli warning.
Columns when populated include `team_id`, `id`, `date`, `description`,
and `type`.

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_team_transactions(team_id = "4"))
#> ! ESPN NHL team transactions returned no data for team_id=4. This endpoint may not be populated for NHL.
#> data frame with 0 columns and 0 rows
# }
```
