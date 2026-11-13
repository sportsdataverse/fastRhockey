# **Get ESPN NHL Athletes Index (core-v2)**

**Get ESPN NHL Athletes Index (core-v2)**

## Usage

``` r
espn_nhl_athletes_index(active = TRUE, limit = 100, page = 1, ...)
```

## Arguments

- active:

  Filter by active status. `TRUE` (default) = active players only;
  `FALSE` = inactive only; `NULL` = no filter.

- limit:

  Number of athletes per page (default `100`; max ~200).

- page:

  Page number (default `1`). Use `page_count` from the result to
  determine the total number of pages.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per athlete in the requested
page. Surface `count` and `page_count` to manage pagination.

|            |           |                                             |
|------------|-----------|---------------------------------------------|
| col_name   | types     | description                                 |
| ref        | character | `$ref` URL for the athlete object.          |
| athlete_id | character | ESPN athlete id parsed from the `$ref` URL. |
| active     | logical   | The `active` filter value echoed from arg.  |
| page       | integer   | The `page` value echoed from arg.           |
| count      | integer   | Total athlete count matching the filter.    |
| page_count | integer   | Total number of pages.                      |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_athletes_index(active = TRUE, limit = 10, page = 1))
#> ── NHL Athletes Index data from ESPN core-v2 ────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:16:23 UTC
#> # A tibble: 10 × 6
#>    ref                                  athlete_id active  page count page_count
#>    <chr>                                <chr>      <lgl>  <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/… 2273       TRUE       1  1074        108
#>  2 http://sports.core.api.espn.com/v2/… 2300       TRUE       1  1074        108
#>  3 http://sports.core.api.espn.com/v2/… 2389       TRUE       1  1074        108
#>  4 http://sports.core.api.espn.com/v2/… 2666       TRUE       1  1074        108
#>  5 http://sports.core.api.espn.com/v2/… 2931       TRUE       1  1074        108
#>  6 http://sports.core.api.espn.com/v2/… 3101       TRUE       1  1074        108
#>  7 http://sports.core.api.espn.com/v2/… 3114       TRUE       1  1074        108
#>  8 http://sports.core.api.espn.com/v2/… 3124       TRUE       1  1074        108
#>  9 http://sports.core.api.espn.com/v2/… 3508       TRUE       1  1074        108
#> 10 http://sports.core.api.espn.com/v2/… 3535       TRUE       1  1074        108
# }
```
