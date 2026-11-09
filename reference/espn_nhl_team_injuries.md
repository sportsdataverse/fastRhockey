# **Get ESPN NHL Team Injuries**

**Get ESPN NHL Team Injuries**

## Usage

``` r
espn_nhl_team_injuries(team_id, ...)
```

## Arguments

- team_id:

  ESPN team identifier (character or numeric, e.g. `"4"` for Chicago
  Blackhawks).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per injury entry when data is
available. The `/injuries` endpoint currently returns an empty response
for NHL; an empty tibble is returned with a cli warning. Columns when
populated include `team_id`, `athlete_id`, `athlete_display_name`,
`athlete_position`, `injury_type`, `injury_status`, `injury_date`, and
`injury_description`.

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_team_injuries(team_id = "4"))
#> ! ESPN NHL team injuries returned no data for team_id=4. This endpoint may not be populated for NHL.
#> data frame with 0 columns and 0 rows
# }
```
