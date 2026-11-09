# **Get ESPN NHL Team Leaders**

**Get ESPN NHL Team Leaders**

## Usage

``` r
espn_nhl_team_leaders(team_id, ...)
```

## Arguments

- team_id:

  ESPN team identifier (character or numeric, e.g. `"4"` for Chicago
  Blackhawks).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per (category, leader)
combination when data is available. The `/leaders` endpoint currently
returns an empty response for NHL; an empty tibble is returned with a
cli warning. Columns when populated include `team_id`, `category_name`,
`category_abbr`, `rank`, `value`, `display_value`, `athlete_id`, and
`athlete_full_name`.

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_team_leaders(team_id = "4"))
#> ! ESPN NHL team leaders returned no data for team_id=4. This endpoint may not be populated for NHL.
#> data frame with 0 columns and 0 rows
# }
```
