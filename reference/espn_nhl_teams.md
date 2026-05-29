# **Get ESPN NHL team names and IDs**

**Get ESPN NHL team names and IDs**

## Usage

``` r
espn_nhl_teams()
```

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                 |           |                                         |
|-----------------|-----------|-----------------------------------------|
| col_name        | types     | description                             |
| abbreviation    | character | Team abbreviation.                      |
| alternate_color | character | Team alternate color hex code.          |
| color           | character | Team primary color hex code.            |
| display_name    | character | Full team display name.                 |
| espn_team_id    | integer   | ESPN team identifier.                   |
| team            | character | Team location name.                     |
| logo            | character | URL to the team logo.                   |
| logo_dark       | character | URL to the dark-variant team logo.      |
| logos_href_3    | character | URL to an additional team logo variant. |
| logos_href_4    | character | URL to an additional team logo variant. |
| logos_href_5    | character | URL to an additional team logo variant. |
| logos_href_6    | character | URL to an additional team logo variant. |
| logos_href_7    | character | URL to an additional team logo variant. |
| logos_href_8    | character | URL to an additional team logo variant. |
| logos_href_9    | character | URL to an additional team logo variant. |
| logos_href_10   | character | URL to an additional team logo variant. |
| logos_href_11   | character | URL to an additional team logo variant. |
| logos_href_12   | character | URL to an additional team logo variant. |
| logos_href_13   | character | URL to an additional team logo variant. |
| logos_href_14   | character | URL to an additional team logo variant. |
| logos_href_15   | character | URL to an additional team logo variant. |
| logos_href_16   | character | URL to an additional team logo variant. |
| mascot          | character | Team mascot name.                       |
| nickname        | character | Team nickname.                          |
| short_name      | character | Short team display name.                |

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_teams())
#> ── NHL Teams data from ESPN.com ─────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 18:45:15 UTC
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
