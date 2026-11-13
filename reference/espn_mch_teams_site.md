# **Get ESPN MCH / WCH Teams (site_v2) – alias for `espn_mch_teams()`**

**Get ESPN MCH / WCH Teams (site_v2) – alias for `espn_mch_teams()`**

## Usage

``` r
espn_mch_teams_site(limit = 1000, ...)

espn_wch_teams_site(limit = 1000, ...)
```

## Arguments

- limit:

  Maximum number of teams (ESPN `limit`; default `1000`, as
  sportsdataverse-py).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per MCH team. This is identical
to the output of teams.

|  |  |  |
|----|----|----|
| col_name | type | description |
| abbreviation | character | Team abbreviation. |
| display_name | character | Team display name. |
| espn_team_id | integer | ESPN team identifier. |
| team | character | Team location/city name. |
| logo | character | Light-background logo URL. |
| logo_dark | character | Dark-background logo URL. |
| logos_href_3 | character | Logo variant 3 URL (ESPN sends a varying number of logos per team). |
| logos_href_4 | character | Logo variant 4 URL (ESPN sends a varying number of logos per team). |
| logos_href_5 | character | Logo variant 5 URL (ESPN sends a varying number of logos per team). |
| logos_href_6 | character | Logo variant 6 URL (ESPN sends a varying number of logos per team). |
| logos_href_7 | character | Logo variant 7 URL (ESPN sends a varying number of logos per team). |
| logos_href_8 | character | Logo variant 8 URL (ESPN sends a varying number of logos per team). |
| logos_href_9 | character | Logo variant 9 URL (ESPN sends a varying number of logos per team). |
| logos_href_10 | character | Logo variant 10 URL (ESPN sends a varying number of logos per team). |
| logos_href_11 | character | Logo variant 11 URL (ESPN sends a varying number of logos per team). |
| logos_href_12 | character | Logo variant 12 URL (ESPN sends a varying number of logos per team). |
| logos_href_13 | character | Logo variant 13 URL (ESPN sends a varying number of logos per team). |
| logos_href_14 | character | Logo variant 14 URL (ESPN sends a varying number of logos per team). |
| mascot | character | Team mascot/nickname. |
| nickname | character | Team nickname. |
| short_name | character | Short team display name. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_teams_site()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_teams_site.md);
columns verified against a captured MCH body.

`espn_wch_teams_site()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

teams which this function wraps.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_teams_site())
#> ── MENS-COLLEGE-HOCKEY Teams data from ESPN.com ─────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:26:46 UTC
#> # A tibble: 116 × 23
#>    abbreviation display_name espn_team_id team  mascot nickname short_name logo 
#>    <chr>        <chr>               <int> <chr> <chr>  <chr>    <chr>      <chr>
#>  1 ADR          Adrian Bull…       110134 Adri… Bulld… Adrian   Adrian     NA   
#>  2 AF           Air Force F…         2005 Air … Falco… Air For… Air Force  http…
#>  3 AH           Alabama Hun…         2008 Alab… Charg… Alabama… Alabama H… NA   
#>  4 UAA          Alaska Anch…            1 Alas… Seawo… Alaska … Alaska An… http…
#>  5 AKFB         Alaska Nano…          298 Alas… Nanoo… Alaska   Alaska     http…
#>  6 ABCA         Alberta Gol…        14196 Albe… Golde… Alberta  Alberta    NA   
#>  7 AIC          American In…         2022 Amer… Yello… America… American … http…
#>  8 ANMR         Anna Maria …       126807 Anna… Amcats Anna Ma… Anna Maria NA   
#>  9 ASU          Arizona Sta…            9 Ariz… Sun D… Arizona… Arizona S… http…
#> 10 ARIZ         Arizona Wil…           12 Ariz… Wildc… Arizona  Arizona    http…
#> # ℹ 106 more rows
#> # ℹ 15 more variables: logo_dark <chr>, logos_href_3 <chr>, logos_href_4 <chr>,
#> #   logos_href_5 <chr>, logos_href_6 <chr>, logos_href_7 <chr>,
#> #   logos_href_8 <chr>, logos_href_9 <chr>, logos_href_10 <chr>,
#> #   logos_href_11 <chr>, logos_href_12 <chr>, logos_href_13 <chr>,
#> #   logos_href_14 <chr>, alternate_color <chr>, color <chr>
# }
# \donttest{
  try(espn_wch_teams_site())
#> ── WOMENS-COLLEGE-HOCKEY Teams data from ESPN.com ───────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:26:46 UTC
#> # A tibble: 47 × 21
#>    abbreviation display_name     espn_team_id team  logo  logo_dark logos_href_3
#>    <chr>        <chr>                   <int> <chr> <chr> <chr>     <chr>       
#>  1 ASP          Assumption Grey…       127962 Assu… http… https://… NA          
#>  2 BST          Bemidji State B…          132 Bemi… http… https://… https://a.e…
#>  3 BC           Boston College …          103 Bost… http… https://… https://a.e…
#>  4 BU           Boston Universi…          104 Bost… http… https://… https://a.e…
#>  5 BRWN         Brown Bears               225 Brown http… https://… https://a.e…
#>  6 CLAR         Clarkson Golden…         2137 Clar… http… https://… https://a.e…
#>  7 COLG         Colgate Raiders          2142 Colg… http… https://… https://a.e…
#>  8 COR          Cornell Big Red           172 Corn… http… https://… https://a.e…
#>  9 DART         Dartmouth Big G…          159 Dart… http… https://… https://a.e…
#> 10 FP           Franklin Pierce…       121934 Fran… http… https://… https://a.e…
#> # ℹ 37 more rows
#> # ℹ 14 more variables: logos_href_4 <chr>, logos_href_5 <chr>,
#> #   logos_href_6 <chr>, logos_href_7 <chr>, logos_href_8 <chr>,
#> #   logos_href_9 <chr>, logos_href_10 <chr>, logos_href_11 <chr>,
#> #   logos_href_12 <chr>, logos_href_13 <chr>, logos_href_14 <chr>,
#> #   mascot <chr>, nickname <chr>, short_name <chr>
# }
```
