# **Get ESPN NHL Coach (core-v2)**

**Get ESPN NHL Coach (core-v2)**

## Usage

``` r
espn_nhl_coach(coach_id, ...)
```

## Arguments

- coach_id:

  ESPN coach identifier (character or numeric). Use
  [`espn_nhl_coaches()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_coaches.md)
  to discover valid coach ids.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with coach detail. Note that
`careerRecords` and `coachSeasons` are sparse (empty arrays) for many
NHL coaches - see
[`espn_nhl_coach_record()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_coach_record.md)
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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    ccs <- espn_nhl_coaches(season = 2026)
    espn_nhl_coach(coach_id = ccs$coach_id[1])
  })
#> ── NHL Coach data from ESPN core-v2 ─────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:18:59 UTC
#> # A tibble: 1 × 10
#>   coach_id id    uid              first_name last_name date_of_birth  birth_city
#>   <chr>    <chr> <chr>            <chr>      <chr>     <chr>          <chr>     
#> 1 900      900   s:70~l:90~co:900 Marco      Sturm     1978-09-08T07… Dingolfing
#> # ℹ 3 more variables: birth_country <chr>, experience <int>, coach_ref <chr>
# }
```
