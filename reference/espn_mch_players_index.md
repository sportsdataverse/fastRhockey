# **Get ESPN MCH / WCH Players Index**

Returns a paginated index of MCH athletes from ESPN. This is a
site_v2-named parity alias for athletes_index; both wrap the same
`core_v2 /athletes` backing endpoint. Prefer athletes_index for the
established function with the same interface.

## Usage

``` r
espn_mch_players_index(active = TRUE, limit = 100, page = 1, ...)

espn_wch_players_index(active = TRUE, limit = 100, page = 1, ...)
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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_players_index()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_players_index.md)
(columns inherited); a collection may be empty.

`espn_wch_players_index()` is the same wrapper for women's college
hockey (league slug `womens-college-hockey`): same arguments, same
columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

athletes_index for the established alias.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_players_index(active = TRUE, limit = 10, page = 1))
#> ── MENS-COLLEGE-HOCKEY Players Index data from ESPN.com ─── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:16:06 UTC
#> # A tibble: 10 × 6
#>    ref                                  athlete_id active  page count page_count
#>    <chr>                                <chr>      <lgl>  <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/… 2229972    TRUE       1  1291        130
#>  2 http://sports.core.api.espn.com/v2/… 2335087    TRUE       1  1291        130
#>  3 http://sports.core.api.espn.com/v2/… 2501229    TRUE       1  1291        130
#>  4 http://sports.core.api.espn.com/v2/… 2501230    TRUE       1  1291        130
#>  5 http://sports.core.api.espn.com/v2/… 2501231    TRUE       1  1291        130
#>  6 http://sports.core.api.espn.com/v2/… 2501232    TRUE       1  1291        130
#>  7 http://sports.core.api.espn.com/v2/… 2501233    TRUE       1  1291        130
#>  8 http://sports.core.api.espn.com/v2/… 2501234    TRUE       1  1291        130
#>  9 http://sports.core.api.espn.com/v2/… 2501236    TRUE       1  1291        130
#> 10 http://sports.core.api.espn.com/v2/… 2564162    TRUE       1  1291        130
# }
# \donttest{
  try(espn_wch_players_index(active = TRUE, limit = 10, page = 1))
#> ── WOMENS-COLLEGE-HOCKEY Players Index data from ESPN.com ──────────────────────
#> ℹ Data updated: 2026-11-13 20:16:06 UTC
#> # A tibble: 10 × 6
#>    ref                                  athlete_id active  page count page_count
#>    <chr>                                <chr>      <lgl>  <int> <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/… 4913605    TRUE       1   621         63
#>  2 http://sports.core.api.espn.com/v2/… 4913613    TRUE       1   621         63
#>  3 http://sports.core.api.espn.com/v2/… 4914252    TRUE       1   621         63
#>  4 http://sports.core.api.espn.com/v2/… 4915359    TRUE       1   621         63
#>  5 http://sports.core.api.espn.com/v2/… 4915887    TRUE       1   621         63
#>  6 http://sports.core.api.espn.com/v2/… 4915895    TRUE       1   621         63
#>  7 http://sports.core.api.espn.com/v2/… 4916857    TRUE       1   621         63
#>  8 http://sports.core.api.espn.com/v2/… 4916861    TRUE       1   621         63
#>  9 http://sports.core.api.espn.com/v2/… 4916878    TRUE       1   621         63
#> 10 http://sports.core.api.espn.com/v2/… 5263049    TRUE       1   621         63
# }
```
