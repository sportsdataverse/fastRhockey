# **Get ESPN NHL Coach Record (core-v2)**

**Get ESPN NHL Coach Record (core-v2)**

## Usage

``` r
espn_nhl_coach_record(coach_id, ...)
```

## Arguments

- coach_id:

  ESPN coach identifier (character or numeric). Use
  [`espn_nhl_coaches()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_coaches.md)
  to discover valid coach ids.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with coach career record data, or an empty
tibble when the endpoint is unavailable. **Note**: The coach record path
(`coaches/{id}/record`) returns HTTP 404 for NHL - this endpoint is not
populated by ESPN for the NHL league. An empty tibble is returned
gracefully with a warning:

|          |           |                                                        |
|----------|-----------|--------------------------------------------------------|
| col_name | types     | description                                            |
| coach_id | character | ESPN coach id (echoed from arg).                       |
| ...      | varies    | Additional record fields if returned (sparse for NHL). |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns an empty tibble for NHL (404 from ESPN)
  try({
    ccs <- espn_nhl_coaches(season = 2026)
    espn_nhl_coach_record(coach_id = ccs$coach_id[1])
  })
#> ! 2026-11-13 20:16:25.542122: ESPN nhl coach record for coach 900 is not available (HTTP 404 is normal for NHL).
#> ! Warning: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
