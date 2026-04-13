# **NHL Records - Franchise Listing**

Returns the franchise listing from the NHL Records API
(`https://records.nhl.com/site/api/franchise`). Optionally filter to a
single franchise via `franchise_id`, which is translated into the
Cayenne filter `id={franchise_id}` (the records API does **not** accept
a path-suffix `franchise/{id}` form — it returns 404).

## Usage

``` r
nhl_records_franchise(franchise_id = NULL, lang = "en")
```

## Arguments

- franchise_id:

  Optional integer franchise ID.

- lang:

  Character language code. Default `"en"`. Currently unused server-side
  but reserved for future use.

## Value

A `fastRhockey_data` tibble of franchises, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_records_franchise())
#> ── NHL Records Franchise ────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:36 UTC
#> # A tibble: 40 × 8
#>       id first_season_id full_name            last_season_id most_recent_team_id
#>    <int>           <int> <chr>                         <int>               <int>
#>  1     1        19171918 Montréal Canadiens               NA                   8
#>  2     2        19171918 Montreal Wanderers         19171918                  41
#>  3     3        19171918 St. Louis Eagles           19341935                  45
#>  4     4        19191920 Hamilton Tigers            19241925                  37
#>  5     5        19171918 Toronto Maple Leafs              NA                  10
#>  6     6        19241925 Boston Bruins                    NA                   6
#>  7     7        19241925 Montreal Maroons           19371938                  43
#>  8     8        19251926 Brooklyn Americans         19411942                  51
#>  9     9        19251926 Philadelphia Quakers       19301931                  39
#> 10    10        19261927 New York Rangers                 NA                   3
#> # ℹ 30 more rows
#> # ℹ 3 more variables: team_abbrev <chr>, team_common_name <chr>,
#> #   team_place_name <chr>
  try(nhl_records_franchise(franchise_id = 5))
#> ── NHL Records Franchise ────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:37 UTC
#> # A tibble: 1 × 8
#>      id first_season_id full_name last_season_id most_recent_team_id team_abbrev
#>   <int>           <int> <chr>     <lgl>                        <int> <chr>      
#> 1     5        19171918 Toronto … NA                              10 TOR        
#> # ℹ 2 more variables: team_common_name <chr>, team_place_name <chr>
# }
```
