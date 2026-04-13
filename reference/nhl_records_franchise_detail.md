# **NHL Records - Franchise Detail**

Returns detailed franchise metadata from the NHL Records API
(`https://records.nhl.com/site/api/franchise-detail`). Optionally filter
by franchise ID or a `cayenneExp` expression.

## Usage

``` r
nhl_records_franchise_detail(franchise_id = NULL, cayenne_exp = NULL)
```

## Arguments

- franchise_id:

  Optional integer franchise ID. When supplied it is translated into a
  `cayenneExp=mostRecentTeamId={franchise_id}` filter unless
  `cayenne_exp` is already provided.

- cayenne_exp:

  Optional Cayenne filter expression string. Takes precedence over
  `franchise_id` when both are supplied.

## Value

A `fastRhockey_data` tibble of franchise details, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_records_franchise_detail())
#> ── NHL Records Franchise Detail ─────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:37 UTC
#> # A tibble: 40 × 13
#>       id active captain_history      coaching_history date_awarded directory_url
#>    <int> <lgl>  <chr>                <chr>            <chr>        <chr>        
#>  1     1 TRUE   "<ul class=\"stripe… "<ul class=\"st… 1917-11-26T… https://www.…
#>  2     2 FALSE   NA                   NA              1917-11-26T… NA           
#>  3     3 FALSE   NA                   NA              1917-11-26T… NA           
#>  4     4 FALSE   NA                   NA              1917-11-26T… NA           
#>  5     5 TRUE   "<ul class=\"stripe… "<ul class=\"st… 1917-11-26T… https://www.…
#>  6     6 TRUE   "<ul class=\"stripe… "<ul class=\"st… 1924-11-01T… https://www.…
#>  7     7 FALSE   NA                   NA              1924-11-01T… NA           
#>  8     8 FALSE   NA                   NA              1925-09-22T… NA           
#>  9     9 FALSE   NA                   NA              1925-11-07T… NA           
#> 10    10 TRUE   "<ul class=\"stripe… "<ul class=\"st… 1926-05-15T… https://www.…
#> # ℹ 30 more rows
#> # ℹ 7 more variables: first_season_id <int>, general_manager_history <chr>,
#> #   hero_image_url <chr>, most_recent_team_id <int>,
#> #   retired_numbers_summary <chr>, team_abbrev <chr>, team_full_name <chr>
# }
```
