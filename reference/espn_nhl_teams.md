# **Get ESPN NHL team names and IDs**

**Get ESPN NHL team names and IDs**

## Usage

``` r
espn_nhl_teams()
```

## Value

A teams data frame with the following columns:

|                 |           |
|-----------------|-----------|
| col_name        | types     |
| espn_team_id    | integer   |
| abbreviation    | character |
| display_name    | character |
| short_name      | character |
| mascot          | character |
| nickname        | character |
| team            | character |
| color           | character |
| alternate_color | character |
| logo            | character |
| logo_dark       | character |
| logos_href_3    | character |
| logos_href_4    | character |

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_teams())
#> ── NHL Teams data from ESPN.com ─────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 07:21:11 UTC
#> # A tibble: 32 × 25
#>    espn_team_id abbreviation display_name short_name mascot nickname team  color
#>           <int> <chr>        <chr>        <chr>      <chr>  <chr>    <chr> <chr>
#>  1           25 ANA          Anaheim Duc… Ducks      Ducks  Anaheim  Anah… fc4c…
#>  2            1 BOS          Boston Brui… Bruins     Bruins Boston   Bost… 231f…
#>  3            2 BUF          Buffalo Sab… Sabres     Sabres Buffalo  Buff… 0046…
#>  4            3 CGY          Calgary Fla… Flames     Flames Calgary  Calg… dd1a…
#>  5            7 CAR          Carolina Hu… Hurricanes Hurri… Carolina Caro… e304…
#>  6            4 CHI          Chicago Bla… Blackhawks Black… Chicago  Chic… e319…
#>  7           17 COL          Colorado Av… Avalanche  Avala… Colorado Colo… 8600…
#>  8           29 CBJ          Columbus Bl… Blue Jack… Blue … Columbus Colu… 002d…
#>  9            9 DAL          Dallas Stars Stars      Stars  Dallas   Dall… 2086…
#> 10            5 DET          Detroit Red… Red Wings  Red W… Detroit  Detr… e305…
#> # ℹ 22 more rows
#> # ℹ 17 more variables: alternate_color <chr>, logo <chr>, logo_dark <chr>,
#> #   logos_href_3 <chr>, logos_href_4 <chr>, logos_href_5 <chr>,
#> #   logos_href_6 <chr>, logos_href_7 <chr>, logos_href_8 <chr>,
#> #   logos_href_9 <chr>, logos_href_10 <chr>, logos_href_11 <chr>,
#> #   logos_href_12 <chr>, logos_href_13 <chr>, logos_href_14 <chr>,
#> #   logos_href_15 <chr>, logos_href_16 <chr>
# }
```
