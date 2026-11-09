# **Get ESPN MCH / WCH Coach Record (core-v2)**

**Get ESPN MCH / WCH Coach Record (core-v2)**

## Usage

``` r
espn_mch_coach_record(coach_id, record_type = 0, ...)

espn_wch_coach_record(coach_id, record_type = 0, ...)
```

## Arguments

- coach_id:

  ESPN coach identifier (character or numeric). Use
  [`espn_mch_season_coaches()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_coaches.md)
  to discover valid coach ids.

- record_type:

  Record type path segment (`coaches/{id}/record/{record_type}`);
  default `0`, as sportsdataverse-py. `NULL` drops the segment.

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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_coach_record()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_coach_record.md)
(columns inherited); a collection may be empty.

`espn_wch_coach_record()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns an empty tibble for NHL (404 from ESPN)
  try({
    ccs <- espn_mch_season_coaches(season = 2026)
    # the coach list can be empty for college hockey
    if (nrow(ccs) > 0) espn_mch_coach_record(coach_id = ccs$coach_id[1])
  })
# }
# \donttest{
  # NOTE: Returns an empty tibble for NHL (404 from ESPN)
  try({
    ccs <- espn_wch_season_coaches(season = 2026)
    # the coach list can be empty for college hockey
    if (nrow(ccs) > 0) espn_wch_coach_record(coach_id = ccs$coach_id[1])
  })
# }
```
