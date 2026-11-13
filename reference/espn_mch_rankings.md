# **Get ESPN MCH / WCH Rankings (Site v2 polls)**

**Get ESPN MCH / WCH Rankings (Site v2 polls)**

## Usage

``` r
espn_mch_rankings(...)

espn_wch_rankings(...)
```

## Arguments

- ...:

  Passed through to
  [`.retry_request()`](https://fastRhockey.sportsdataverse.org/reference/dot-retry_request.md)
  (e.g. `proxy =`).

## Value

A `fastRhockey_data` tibble with one row per (poll, team), always with
the same 25 columns; zero rows (with those columns) when no poll is
published:

|  |  |  |
|----|----|----|
| col_name | type | description |
| poll_id | integer | ESPN poll identifier. |
| poll_name | character | Poll name. |
| poll_short_name | character | Poll short name. |
| poll_type | character | Poll type code. |
| season | integer | Season end-year. |
| season_type | integer | Season type (2 = regular season, 3 = postseason). |
| week | integer | Poll week within the season type. |
| week_display | character | Poll week display string. |
| poll_date | character | Poll date (ISO 8601). |
| ranked | logical | `TRUE` for a ranked team, `FALSE` for a team receiving votes. |
| team_id | character | ESPN team identifier. |
| rank | integer | Current rank (`NA` when not ranked). |
| previous_rank | integer | Previous poll rank (`0` = unranked). |
| points | numeric | Poll points. |
| first_place_votes | integer | First-place votes (`NA` when not published). |
| trend | character | Rank trend indicator. |
| record_summary | character | Team record summary. |
| team_uid | character | ESPN team UID. |
| team_location | character | Team location. |
| team_name | character | Team name. |
| team_nickname | character | Team nickname. |
| team_abbreviation | character | Team abbreviation. |
| team_color | character | Team colour hex (`NA` when ESPN ships `"NULL"`). |
| team_logo | character | Team logo URL. |
| last_updated | character | Entry last-updated timestamp. |

## Details

ESPN men's college hockey polls (USCHO), league slug
`mens-college-hockey`. `rank` is `NA` on the vote-receiving rows
(`ranked = FALSE`); `previous_rank` keeps ESPN's `0` for a team unranked
in the previous poll. Mirrors sportsdataverse-py `espn_mch_rankings()`.

`espn_wch_rankings()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_rankings())
#> ── MENS-COLLEGE-HOCKEY Rankings data from ESPN.com ──────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:26:28 UTC
#> # A tibble: 33 × 25
#>    poll_id poll_name        poll_short_name  poll_type  season season_type  week
#>      <int> <chr>            <chr>            <chr>       <int>       <int> <int>
#>  1   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     7
#>  2   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     7
#>  3   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     7
#>  4   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     7
#>  5   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     7
#>  6   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     7
#>  7   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     7
#>  8   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     7
#>  9   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     7
#> 10   10624 USCHO Men's Poll USCHO Men's Poll USCHOMENS…   2027           2     7
#> # ℹ 23 more rows
#> # ℹ 18 more variables: week_display <chr>, poll_date <chr>, ranked <lgl>,
#> #   team_id <chr>, rank <int>, previous_rank <int>, points <dbl>,
#> #   first_place_votes <int>, trend <chr>, record_summary <chr>, team_uid <chr>,
#> #   team_location <chr>, team_name <chr>, team_nickname <chr>,
#> #   team_abbreviation <chr>, team_color <chr>, team_logo <chr>,
#> #   last_updated <chr>
# }
# \donttest{
  try(espn_wch_rankings())
#> ── WOMENS-COLLEGE-HOCKEY Rankings data from ESPN.com ────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:26:28 UTC
#> # A tibble: 20 × 25
#>    poll_id poll_name          poll_short_name poll_type season season_type  week
#>      <int> <chr>              <chr>           <chr>      <int>       <int> <int>
#>  1   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#>  2   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#>  3   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#>  4   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#>  5   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#>  6   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#>  7   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#>  8   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#>  9   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#> 10   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#> 11   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#> 12   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#> 13   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#> 14   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#> 15   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#> 16   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#> 17   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#> 18   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#> 19   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#> 20   10604 USCHO Women's Poll USCHO Women's … USCHOWOM…   2027           2     8
#> # ℹ 18 more variables: week_display <chr>, poll_date <chr>, ranked <lgl>,
#> #   team_id <chr>, rank <int>, previous_rank <int>, points <dbl>,
#> #   first_place_votes <int>, trend <chr>, record_summary <chr>, team_uid <chr>,
#> #   team_location <chr>, team_name <chr>, team_nickname <chr>,
#> #   team_abbreviation <chr>, team_color <chr>, team_logo <chr>,
#> #   last_updated <chr>
# }
```
