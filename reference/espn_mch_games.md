# **Get ESPN MCH / WCH Games (core-v2)**

**Get ESPN MCH / WCH Games (core-v2)**

## Usage

``` r
espn_mch_games(dates = NULL, limit = 500, ...)

espn_wch_games(dates = NULL, limit = 500, ...)
```

## Arguments

- dates:

  Optional date string `"YYYYMMDD"` or range `"YYYYMMDD-YYYYMMDD"`. When
  `NULL` (default) the most recent games are returned.

- limit:

  Maximum number of events per page (default `500`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per game event reference:

|            |           |                                              |
|------------|-----------|----------------------------------------------|
| col_name   | types     | description                                  |
| ref        | character | `$ref` URL for the event object.             |
| event_id   | character | ESPN event id parsed from the `$ref` URL.    |
| count      | integer   | Total events in the collection.              |
| page_count | integer   | Total pages in the collection.               |
| dates      | character | Dates filter (echoed from arg, if supplied). |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_games()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_games.md)
(columns inherited); a collection may be empty.

`espn_wch_games()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_games(limit = 10))
#> ── MENS-COLLEGE-HOCKEY Games data from ESPN core-v2 ─────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:15:58 UTC
#> # A tibble: 10 × 4
#>    ref                                                 event_id count page_count
#>    <chr>                                               <chr>    <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019044…    26          3
#>  2 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019045…    26          3
#>  3 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019047…    26          3
#>  4 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019047…    26          3
#>  5 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019048…    26          3
#>  6 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019048…    26          3
#>  7 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019048…    26          3
#>  8 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019049…    26          3
#>  9 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019050…    26          3
#> 10 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019051…    26          3
# }
# \donttest{
  try(espn_wch_games(limit = 10))
#> ── WOMENS-COLLEGE-HOCKEY Games data from ESPN core-v2 ───── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:15:58 UTC
#> # A tibble: 10 × 4
#>    ref                                                 event_id count page_count
#>    <chr>                                               <chr>    <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019039…    20          2
#>  2 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019040…    20          2
#>  3 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019123…    20          2
#>  4 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019041…    20          2
#>  5 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019136…    20          2
#>  6 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019037…    20          2
#>  7 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019038…    20          2
#>  8 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019040…    20          2
#>  9 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019041…    20          2
#> 10 http://sports.core.api.espn.com/v2/sports/hockey/l… 4019037…    20          2
# }
```
