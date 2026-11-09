# **Get ESPN NHL Team History**

**Get ESPN NHL Team History**

## Usage

``` r
espn_nhl_team_history(team_id, ...)
```

## Arguments

- team_id:

  ESPN team identifier (character or numeric, e.g. `"4"` for Chicago
  Blackhawks).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per season entry when data is
available. The `/history` endpoint currently returns an empty response
for NHL; an empty tibble is returned with a cli warning. Columns when
populated include `team_id`, `season`, `wins`, `losses`, `ties`,
`ot_losses`, `points`, `summary`, and `description`.

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_team_history(team_id = "4"))
#> ! ESPN NHL team history returned no data for team_id=4. This endpoint may not be populated for NHL.
#> data frame with 0 columns and 0 rows
# }
```
