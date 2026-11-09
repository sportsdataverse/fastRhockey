# **Get ESPN MCH / WCH Coach Season (core-v2)**

**Get ESPN MCH / WCH Coach Season (core-v2)**

## Usage

``` r
espn_mch_coach_season(coach_id, season = most_recent_nhl_season(), ...)

espn_wch_coach_season(coach_id, season = most_recent_nhl_season(), ...)
```

## Arguments

- coach_id:

  ESPN coach identifier (character or numeric). Use
  [`espn_mch_season_coaches()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_coaches.md)
  to find valid IDs.

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per coach:

|               |           |                                                    |
|---------------|-----------|----------------------------------------------------|
| col_name      | types     | description                                        |
| coach_id      | character | ESPN coach identifier (echoed from arg).           |
| season        | integer   | Season year (echoed from arg).                     |
| espn_id       | character | ESPN internal coach id.                            |
| uid           | character | ESPN unique uid.                                   |
| first_name    | character | Coach first name.                                  |
| last_name     | character | Coach last name.                                   |
| date_of_birth | character | Date of birth (ISO 8601).                          |
| birth_city    | character | Birth city.                                        |
| birth_country | character | Birth country.                                     |
| person_ref    | character | `$ref` URL for the coach person object on core-v2. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_coach_season()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_coach_season.md)
(columns inherited); a collection may be empty.

`espn_wch_coach_season()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_coach_season(coach_id = "900", season = 2026))
#> ✖ 2026-11-09 22:50:12.753016: Invalid arguments or no ESPN mens-college-hockey coach 900 season 2026 data available!
#> ✖ Args: league = "mens-college-hockey", coach_id = "900", season = 2026, coach_first = TRUE
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_coach_season(coach_id = "900", season = 2026))
#> ✖ 2026-11-09 22:50:12.795805: Invalid arguments or no ESPN womens-college-hockey coach 900 season 2026 data available!
#> ✖ Args: league = "womens-college-hockey", coach_id = "900", season = 2026, coach_first = TRUE
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
