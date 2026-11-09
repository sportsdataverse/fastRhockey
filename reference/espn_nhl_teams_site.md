# **Get ESPN NHL Teams (site_v2) – alias for `espn_nhl_teams()`**

**Get ESPN NHL Teams (site_v2) – alias for
[`espn_nhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_teams.md)**

## Usage

``` r
espn_nhl_teams_site(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per NHL team. This is identical
to the output of
[`espn_nhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_teams.md).

|                 |           |                            |
|-----------------|-----------|----------------------------|
| col_name        | types     | description                |
| espn_team_id    | character | ESPN team identifier.      |
| team            | character | Team location/city name.   |
| mascot          | character | Team mascot/nickname.      |
| display_name    | character | Team display name.         |
| short_name      | character | Short team display name.   |
| abbreviation    | character | Team abbreviation.         |
| color           | character | Primary color hex.         |
| alternate_color | character | Alternate color hex.       |
| logo            | character | Light-background logo URL. |
| logo_dark       | character | Dark-background logo URL.  |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

[`espn_nhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_teams.md)
which this function wraps.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_teams_site())
#> ── NHL Teams data from ESPN.com ─────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 22:52:04 UTC
#> # A tibble: 32 × 25
#>    abbreviation alternate_color color  display_name     espn_team_id team  logo 
#>    <chr>        <chr>           <chr>  <chr>                   <int> <chr> <chr>
#>  1 ANA          000000          fc4c02 Anaheim Ducks              25 Anah… http…
#>  2 BOS          fdb71a          231f20 Boston Bruins               1 Bost… http…
#>  3 BUF          fdb71a          00468b Buffalo Sabres              2 Buff… http…
#>  4 CGY          000000          dd1a32 Calgary Flames              3 Calg… http…
#>  5 CAR          000000          e30426 Carolina Hurric…            7 Caro… http…
#>  6 CHI          000000          e31937 Chicago Blackha…            4 Chic… http…
#>  7 COL          005ea3          860038 Colorado Avalan…           17 Colo… http…
#>  8 CBJ          e31937          002d62 Columbus Blue J…           29 Colu… http…
#>  9 DAL          000000          20864c Dallas Stars                9 Dall… http…
#> 10 DET          ffffff          e30526 Detroit Red Win…            5 Detr… http…
#> # ℹ 22 more rows
#> # ℹ 18 more variables: logo_dark <chr>, logos_href_3 <chr>, logos_href_4 <chr>,
#> #   logos_href_5 <chr>, logos_href_6 <chr>, logos_href_7 <chr>,
#> #   logos_href_8 <chr>, logos_href_9 <chr>, logos_href_10 <chr>,
#> #   logos_href_11 <chr>, logos_href_12 <chr>, logos_href_13 <chr>,
#> #   logos_href_14 <chr>, logos_href_15 <chr>, logos_href_16 <chr>,
#> #   mascot <chr>, nickname <chr>, short_name <chr>
# }
```
