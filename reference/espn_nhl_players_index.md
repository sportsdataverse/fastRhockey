# **Get ESPN NHL Players Index**

Returns a paginated index of NHL athletes from ESPN. This is a
site_v2-named parity alias for
[`espn_nhl_athletes_index()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_athletes_index.md);
both wrap the same `core_v2 /athletes` backing endpoint. Prefer
[`espn_nhl_athletes_index()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_athletes_index.md)
for the established function with the same interface.

## Usage

``` r
espn_nhl_players_index(active = TRUE, limit = 100, page = 1, ...)
```

## Arguments

- active:

  Filter by active status. `TRUE` (default) = active players only;
  `FALSE` = inactive only; `NULL` = no filter.

- limit:

  Number of athletes per page (default `100`; max ~200).

- page:

  Page number (default `1`). Use the `page_count` column in the result
  to determine the total number of pages.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per athlete in the requested
page:

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

[`espn_nhl_athletes_index()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_athletes_index.md)
for the established alias.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_players_index(active = TRUE, limit = 10, page = 1))
#> ── NHL Players Index data from ESPN.com ─────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:19:24 UTC
#> # A tibble: 10 × 6
#>    ref                                  athlete_id active  page count page_count
#>    <chr>                                <chr>      <lgl>  <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/… 2273       TRUE       1  1068        107
#>  2 http://sports.core.api.espn.com/v2/… 2300       TRUE       1  1068        107
#>  3 http://sports.core.api.espn.com/v2/… 2389       TRUE       1  1068        107
#>  4 http://sports.core.api.espn.com/v2/… 2666       TRUE       1  1068        107
#>  5 http://sports.core.api.espn.com/v2/… 2931       TRUE       1  1068        107
#>  6 http://sports.core.api.espn.com/v2/… 3101       TRUE       1  1068        107
#>  7 http://sports.core.api.espn.com/v2/… 3114       TRUE       1  1068        107
#>  8 http://sports.core.api.espn.com/v2/… 3124       TRUE       1  1068        107
#>  9 http://sports.core.api.espn.com/v2/… 3508       TRUE       1  1068        107
#> 10 http://sports.core.api.espn.com/v2/… 3535       TRUE       1  1068        107
# }
```
