# **NHL Draft**

Returns information on the most recent NHL draft picks.

Uses the new NHL API endpoint at `api-web.nhle.com/v1/draft/picks/now`.

## Usage

``` r
nhl_draft()
```

## Value

Returns a data frame of draft picks.

## Examples

``` r
# \donttest{
   try(nhl_draft())
#> ── NHL Draft Data from NHL.com ──────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 04:54:05 UTC
#> # A tibble: 32 × 20
#>    round pick_in_round overall_pick team_id team_abbrev team_name$default  $fr  
#>    <int>         <int>        <int>   <int> <chr>       <chr>              <chr>
#>  1     1             1            1       2 NYI         New York Islanders Isla…
#>  2     1             2            2      28 SJS         San Jose Sharks    Shar…
#>  3     1             3            3      16 CHI         Chicago Blackhawks Blac…
#>  4     1             4            4      68 UTA         Utah Mammoth       Mamm…
#>  5     1             5            5      18 NSH         Nashville Predato… Pred…
#>  6     1             6            6       4 PHI         Philadelphia Flye… Flye…
#>  7     1             7            7       6 BOS         Boston Bruins      Brui…
#>  8     1             8            8      55 SEA         Seattle Kraken     Krak…
#>  9     1             9            9       7 BUF         Buffalo Sabres     Sabr…
#> 10     1            10           10      24 ANA         Anaheim Ducks      Duck…
#> # ℹ 22 more rows
#> # ℹ 14 more variables: team_common_name <df[,2]>,
#> #   team_place_name_with_preposition <df[,2]>, display_abbrev <df[,1]>,
#> #   team_logo_light <chr>, team_logo_dark <chr>, team_pick_history <chr>,
#> #   first_name <df[,1]>, last_name <df[,1]>, position_code <chr>,
#> #   country_code <chr>, height <int>, weight <int>, amateur_league <chr>,
#> #   amateur_club_name <chr>
# }
```
