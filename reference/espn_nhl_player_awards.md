# **Get ESPN NHL Player Awards (core-v2)**

**Get ESPN NHL Player Awards (core-v2)**

## Usage

``` r
espn_nhl_player_awards(athlete_id, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per award, or an empty data
frame if the athlete has no award entries. Each `$ref` follows to a
season-award object (e.g. Hart Trophy, Conn Smythe, etc.).

|            |           |                                      |
|------------|-----------|--------------------------------------|
| col_name   | types     | description                          |
| ref        | character | `$ref` URL for the award object.     |
| award_id   | character | Award id parsed from the `$ref` URL. |
| athlete_id | character | ESPN athlete id (echoed from arg).   |
| count      | integer   | Total award count.                   |
| page_count | integer   | Total page count.                    |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_player_awards(athlete_id = "3114"))
#> ── NHL Player Awards data from ESPN core-v2 ─────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:34:40 UTC
#> # A tibble: 20 × 5
#>    ref                                      award_id athlete_id count page_count
#>    <chr>                                    <chr>    <chr>      <int>      <int>
#>  1 http://sports.core.api.espn.com/v2/spor… 498      3114          20          1
#>  2 http://sports.core.api.espn.com/v2/spor… 109      3114          20          1
#>  3 http://sports.core.api.espn.com/v2/spor… 197      3114          20          1
#>  4 http://sports.core.api.espn.com/v2/spor… 498      3114          20          1
#>  5 http://sports.core.api.espn.com/v2/spor… 197      3114          20          1
#>  6 http://sports.core.api.espn.com/v2/spor… 497      3114          20          1
#>  7 http://sports.core.api.espn.com/v2/spor… 498      3114          20          1
#>  8 http://sports.core.api.espn.com/v2/spor… 106      3114          20          1
#>  9 http://sports.core.api.espn.com/v2/spor… 110      3114          20          1
#> 10 http://sports.core.api.espn.com/v2/spor… 112      3114          20          1
#> 11 http://sports.core.api.espn.com/v2/spor… 497      3114          20          1
#> 12 http://sports.core.api.espn.com/v2/spor… 106      3114          20          1
#> 13 http://sports.core.api.espn.com/v2/spor… 497      3114          20          1
#> 14 http://sports.core.api.espn.com/v2/spor… 101      3114          20          1
#> 15 http://sports.core.api.espn.com/v2/spor… 109      3114          20          1
#> 16 http://sports.core.api.espn.com/v2/spor… 498      3114          20          1
#> 17 http://sports.core.api.espn.com/v2/spor… 106      3114          20          1
#> 18 http://sports.core.api.espn.com/v2/spor… 110      3114          20          1
#> 19 http://sports.core.api.espn.com/v2/spor… 112      3114          20          1
#> 20 http://sports.core.api.espn.com/v2/spor… 497      3114          20          1
# }
```
