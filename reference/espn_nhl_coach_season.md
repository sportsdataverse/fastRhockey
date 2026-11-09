# **Get ESPN NHL Coach Season (core-v2)**

**Get ESPN NHL Coach Season (core-v2)**

## Usage

``` r
espn_nhl_coach_season(coach_id, season = most_recent_nhl_season(), ...)
```

## Arguments

- coach_id:

  ESPN coach identifier (character or numeric). Use
  [`espn_nhl_season_coaches()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_coaches.md)
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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_coach_season(coach_id = "900", season = 2026))
#> ── NHL Coach Season data from ESPN core-v2 ──────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:19:00 UTC
#> # A tibble: 1 × 10
#>   coach_id season espn_id uid      first_name last_name date_of_birth birth_city
#>   <chr>     <int> <chr>   <chr>    <chr>      <chr>     <chr>         <chr>     
#> 1 900        2026 900     s:70~l:… Marco      Sturm     1978-09-08T0… NA        
#> # ℹ 2 more variables: birth_country <chr>, person_ref <chr>
# }
```
