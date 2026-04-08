# **PWHL League Leaders**

Retrieves PWHL league leaders (top scorers or top goalies) for a season.

## Usage

``` r
pwhl_leaders(
  position = "skaters",
  season = most_recent_pwhl_season(),
  game_type = "regular",
  limit = 100
)
```

## Arguments

- position:

  Either "skaters" (default) or "goalies".

- season:

  Season (YYYY) to pull leaders from. Defaults to
  [`most_recent_pwhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_pwhl_season.md).

- game_type:

  Game type: "regular" (default), "preseason", or "playoffs".

- limit:

  Maximum number of leaders to return. Default 100.

## Value

A data frame with league leader statistics, or NULL if unavailable.

## Examples

``` r
# \donttest{
  try(pwhl_leaders(position = "skaters", season = 2025))
#> ── PWHL Leaders - skaters ───────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 06:57:29 UTC
#> # A tibble: 107 × 88
#>    player_id shortname    first_name last_name name  phonetic_name active height
#>    <chr>     <chr>        <chr>      <chr>     <chr> <chr>         <chr>  <chr> 
#>  1 13        H. Knight    Hilary     Knight    Hila… "HILL-AH-REE… 1      "5'11"
#>  2 205       S. Fillier   Sarah      Fillier   Sara… "SAIR-AH FIH… 1      "5'5" 
#>  3 63        D. Watts     Daryl      Watts     Dary… "DAIR-uhl WA… 1      "5'6\…
#>  4 31        M. Poulin    Marie-Phi… Poulin    Mari… ""            1      "5'7" 
#>  5 20        K. Coyne Sc… Kendall    Coyne Sc… Kend… "KEHN-duhl K… 1      "5'2" 
#>  6 89        H. Miller    Hannah     Miller    Hann… "HAN-uh MIH-… 1      "5'9\…
#>  7 36        J. Eldridge  Jessie     Eldridge  Jess… "jeh-see EHL… 1      "5'9\…
#>  8 161       T. Vanišová  Tereza     Vanišová  Tere… " TAH-ree-zu… 1      "5'7" 
#>  9 32        L. Stacey    Laura      Stacey    Laur… "STAY-see"    1      "5'10…
#> 10 21        T. Heise     Taylor     Heise     Tayl… "TAY-luhr HI… 1      "5'10"
#> # ℹ 97 more rows
#> # ℹ 80 more variables: weight <chr>, last_years_club <chr>, age <chr>,
#> #   shoots <chr>, position <chr>, suspension_games_remaining <chr>,
#> #   suspension_indefinite <chr>, rookie <chr>, veteran <chr>,
#> #   draft_eligible <chr>, jersey_number <chr>, team_name <chr>,
#> #   team_code <chr>, team_id <chr>, division <chr>, birthdate <chr>,
#> #   birthdate_year <chr>, hometown <chr>, homeprov <chr>, homecntry <chr>, …
  try(pwhl_leaders(position = "goalies", season = 2025))
#> ── PWHL Leaders - goalies ───────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 06:57:29 UTC
#> # A tibble: 16 × 79
#>    player_id shortname    rookie first_name last_name name  phonetic_name active
#>    <chr>     <chr>        <chr>  <chr>      <chr>     <chr> <chr>         <chr> 
#>  1 134       C. Jackson   0      Carly      Jackson   Carl… "CAR-lee JAK… 1     
#>  2 186       K. Peslarová 0      Klára      Peslarová Klár… "KLAH-ruh PE… 1     
#>  3 28        A. Desbiens  0      Ann-Renée  Desbiens  Ann-… ""            1     
#>  4 123       M. Rooney    0      Maddie     Rooney    Madd… "MAD-dee ROO… 1     
#>  5 222       G. Philips   1      Gwyneth    Philips   Gwyn… "GWIH-nihth … 1     
#>  6 228       K. Osborne   1      Kayle      Osborne   Kayl… "KAY-lee AWZ… 1     
#>  7 64        K. Campbell  0      Kristen    Campbell  Kris… "KRIHS-tehn … 1     
#>  8 211       R. Kirk      1      Raygan     Kirk      Rayg… "ray-GEHN ku… 1     
#>  9 6         A. Frankel   0      Aerin      Frankel   Aeri… "AIR-IN FRAN… 1     
#> 10 85        E. Chuli     0      Elaine     Chuli     Elai… ""            1     
#> 11 155       C. Schroeder 0      Corinne    Schroeder Cori… "kohr-een SH… 1     
#> 12 22        N. Hensley   0      Nicole     Hensley   Nico… "NIHK-ohl HE… 1     
#> 13 59        E. Maschmey… 0      Emerance   Maschmey… Emer… "EH-muhr-ehn… 1     
#> 14 19        E. Söderberg 0      Emma       Söderberg Emma… "EH-mah SOH-… 1     
#> 15 41        A. Levy      0      Abbey      Levy      Abbe… "AH-BEE LEE-… 1     
#> 16 193       L. Morgan    1      Lucy       Morgan    Lucy… ""            1     
#> # ℹ 71 more variables: height <chr>, weight <chr>, position <chr>,
#> #   suspension_games_remaining <chr>, suspension_indefinite <chr>,
#> #   start_date <chr>, veteran <chr>, draft_eligible <chr>, jersey_number <chr>,
#> #   shoots <chr>, catches <chr>, team_name <chr>, team_code <chr>,
#> #   team_id <chr>, division <chr>, birthdate <chr>, birthdate_year <chr>,
#> #   age <chr>, hometown <chr>, homeprov <chr>, homecntry <chr>,
#> #   birthtown <chr>, birthprov <chr>, birthcntry <chr>, hometownprov <chr>, …
# }
```
