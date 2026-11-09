# **Get ESPN MCH / WCH Coach (core-v2)**

**Get ESPN MCH / WCH Coach (core-v2)**

## Usage

``` r
espn_mch_coach(coach_id, ...)

espn_wch_coach(coach_id, ...)
```

## Arguments

- coach_id:

  ESPN coach identifier (character or numeric). Use
  [`espn_mch_season_coaches()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_coaches.md)
  to discover valid coach ids.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with coach detail. Note that
`careerRecords` and `coachSeasons` are sparse (empty arrays) for many
MCH coaches - see
[`espn_mch_coach_record()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_coach_record.md)
for season-scoped records:

|               |           |                                             |
|---------------|-----------|---------------------------------------------|
| col_name      | types     | description                                 |
| coach_id      | character | ESPN coach id (echoed from arg).            |
| id            | character | ESPN coach identifier from response.        |
| uid           | character | Coach uid string.                           |
| first_name    | character | Coach first name.                           |
| last_name     | character | Coach last name.                            |
| date_of_birth | character | Coach date of birth (ISO 8601).             |
| birth_city    | character | Coach birth city.                           |
| birth_country | character | Coach birth country.                        |
| experience    | integer   | Years of coaching experience (if returned). |
| coach_ref     | character | `$ref` URL for this coach object.           |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_coach()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_coach.md)
(columns inherited); a collection may be empty.

`espn_wch_coach()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    ccs <- espn_mch_season_coaches(season = 2026)
    # the coach list can be empty for college hockey
    if (nrow(ccs) > 0) espn_mch_coach(coach_id = ccs$coach_id[1])
  })
# }
# \donttest{
  try({
    ccs <- espn_wch_season_coaches(season = 2026)
    # the coach list can be empty for college hockey
    if (nrow(ccs) > 0) espn_wch_coach(coach_id = ccs$coach_id[1])
  })
# }
```
