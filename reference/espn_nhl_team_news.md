# **Get ESPN NHL Team News**

**Get ESPN NHL Team News**

## Usage

``` r
espn_nhl_team_news(team_id, limit = 50, ...)
```

## Arguments

- team_id:

  ESPN team identifier (character or numeric, e.g. `"4"` for Chicago
  Blackhawks).

- limit:

  Maximum number of articles to return. Default `50`.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per news article when data is
available. The `/news` endpoint currently returns an empty response for
NHL; an empty tibble is returned with a cli warning. Columns when
populated include `team_id`, `id`, `headline`, `description`, `type`,
`published`, `last_modified`, `premium`, and `url`.

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_team_news(team_id = "4", limit = 10))
#> ! ESPN NHL team news returned no data for team_id=4. This endpoint may not be populated for NHL.
#> data frame with 0 columns and 0 rows
# }
```
