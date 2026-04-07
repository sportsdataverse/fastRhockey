# **NHL Draft by Year**

Returns draft pick information for a given year.

Uses the NHL API endpoint at
`api-web.nhle.com/v1/draft/picks/{year}/{round}`. When no round is
specified, all rounds (1-7) are fetched and combined.

## Usage

``` r
nhl_draft_year(year, round = NULL)
```

## Arguments

- year:

  Integer. Draft year (e.g. 2023).

- round:

  Integer or NULL. Specific round (1-7) or NULL for all rounds.

## Value

Returns a data frame of draft picks.

## Examples

``` r
# \donttest{
   try(nhl_draft_year(year = 2023, round = 1))
#> ── NHL Draft Year data from NHL.com ─────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 06:13:35 UTC
#> # A tibble: 32 × 20
#>    round pick_in_round overall_pick team_id team_abbrev team_name$default  $fr  
#>    <int>         <int>        <int>   <int> <chr>       <chr>              <chr>
#>  1     1             1            1      16 CHI         Chicago Blackhawks Blac…
#>  2     1             2            2      24 ANA         Anaheim Ducks      Duck…
#>  3     1             3            3      29 CBJ         Columbus Blue Jac… Blue…
#>  4     1             4            4      28 SJS         San Jose Sharks    Shar…
#>  5     1             5            5       8 MTL         Montréal Canadiens Cana…
#>  6     1             6            6      53 ARI         Arizona Coyotes    Coyo…
#>  7     1             7            7       4 PHI         Philadelphia Flye… Flye…
#>  8     1             8            8      15 WSH         Washington Capita… Capi…
#>  9     1             9            9      17 DET         Detroit Red Wings  Red …
#> 10     1            10           10      19 STL         St. Louis Blues    Blue…
#> # ℹ 22 more rows
#> # ℹ 14 more variables: team_common_name <df[,1]>,
#> #   team_place_name_with_preposition <df[,2]>, display_abbrev <df[,1]>,
#> #   team_logo_light <chr>, team_logo_dark <chr>, team_pick_history <chr>,
#> #   first_name <df[,3]>, last_name <df[,4]>, position_code <chr>,
#> #   country_code <chr>, height <int>, weight <int>, amateur_league <chr>,
#> #   amateur_club_name <chr>
# }
```
