# **NHL Records - Franchise Season Results**

Returns franchise season-by-season results from the NHL Records API
(`https://records.nhl.com/site/api/franchise-season-results`).

## Usage

``` r
nhl_records_franchise_season_results(franchise_id = NULL, cayenne_exp = NULL)
```

## Arguments

- franchise_id:

  Optional integer franchise ID. When supplied it is translated into a
  `cayenneExp=franchiseId={franchise_id}` filter unless `cayenne_exp` is
  already provided.

- cayenne_exp:

  Optional Cayenne filter expression string. Takes precedence over
  `franchise_id` when both are supplied.

## Value

A `fastRhockey_data` tibble of franchise season results, or `NULL` on
failure.

## Examples

``` r
# \donttest{
  try(nhl_records_franchise_season_results())
#> ── NHL Records Franchise Season Results ─────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:38 UTC
#> # A tibble: 2,817 × 37
#>       id conference_abbrev conference_name conference_sequence decision
#>    <int> <chr>             <chr>                         <int> <chr>   
#>  1  2476 NA                NA                                0 L       
#>  2  2477 NA                NA                                0 NA      
#>  3  2478 NA                NA                                0 NA      
#>  4  2479 NA                NA                                0 W       
#>  5  2480 NA                NA                                0 L       
#>  6  2481 NA                NA                                0 W       
#>  7  2482 NA                NA                                0 W       
#>  8  2483 NA                NA                                0 L       
#>  9  2484 NA                NA                                0 NA      
#> 10  2485 NA                NA                                0 W       
#> # ℹ 2,807 more rows
#> # ℹ 32 more variables: division_abbrev <chr>, division_name <chr>,
#> #   division_sequence <int>, final_playoff_round <int>, franchise_id <int>,
#> #   game_type_id <int>, games_played <int>, goals <int>, goals_against <int>,
#> #   home_losses <int>, home_overtime_losses <int>, home_ties <int>,
#> #   home_wins <int>, league_sequence <int>, losses <int>,
#> #   overtime_losses <int>, penalty_minutes <int>, playoff_round <int>, …
# }
```
