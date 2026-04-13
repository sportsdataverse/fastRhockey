# **NHL Records - Franchise Playoff Appearances**

Returns franchise playoff appearance counts from the NHL Records API
(`https://records.nhl.com/site/api/franchise-playoff-appearances`).

## Usage

``` r
nhl_records_franchise_playoff_appearances(
  franchise_id = NULL,
  cayenne_exp = NULL
)
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

A `fastRhockey_data` tibble of franchise playoff appearances, or `NULL`
on failure.

## Examples

``` r
# \donttest{
  try(nhl_records_franchise_playoff_appearances())
#> ── NHL Records Franchise Playoff Appearances ────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:37 UTC
#> # A tibble: 37 × 8
#>       id first_season_id franchise_id franchise_name       playoff_seasons
#>    <int>           <int>        <int> <chr>                          <int>
#>  1     1        19171918            1 Montréal Canadiens                86
#>  2     3        19171918            3 St. Louis Eagles                  10
#>  3     5        19171918            5 Toronto Maple Leafs               74
#>  4     6        19241925            6 Boston Bruins                     77
#>  5     7        19241925            7 Montreal Maroons                  11
#>  6     8        19251926            8 Brooklyn Americans                 5
#>  7     9        19251926            9 Philadelphia Quakers               2
#>  8    10        19261927           10 New York Rangers                  63
#>  9    11        19261927           11 Chicago Blackhawks                63
#> 10    12        19261927           12 Detroit Red Wings                 64
#> # ℹ 27 more rows
#> # ℹ 3 more variables: stanley_cup_appearances <int>, stanley_cup_wins <int>,
#> #   years <int>
# }
```
