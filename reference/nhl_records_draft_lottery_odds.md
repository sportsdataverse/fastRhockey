# **NHL Records - Draft Lottery Odds**

Returns NHL draft lottery odds from the NHL Records API
(`https://records.nhl.com/site/api/draft-lottery-odds`).

## Usage

``` r
nhl_records_draft_lottery_odds(cayenne_exp = NULL)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string.

## Value

A `fastRhockey_data` tibble of draft lottery odds, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_records_draft_lottery_odds())
#> ── NHL Records Draft Lottery Odds ───────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:27 UTC
#> # A tibble: 31 × 5
#>       id draft_year format_content                     odds_content result_notes
#>    <int>      <int> <chr>                              <chr>        <chr>       
#>  1     1       2003 "<p>The 2003 NHL Draft Lottery wa… "<table bor… * In 2003, …
#>  2     2       2002 "<p>The 2002 NHL Draft Lottery wa… "<table bor… ^ In 2002, …
#>  3     3       1999 "<p>The 1999 NHL Draft Lottery wa… "<table bor… ! In 1999, …
#>  4     4       2021 "<p>The 2021 NHL Draft Lottery wa… "<table bor… NA          
#>  5     5       2020 "<p>The 2020 NHL Draft Lottery wa… "<p><strong… NA          
#>  6     6       2019 "<p>The 2019 NHL Draft Lottery wa… "<table bor… NA          
#>  7     7       2018 "<p>The 2018 NHL Draft Lottery wa… "<table bor… NA          
#>  8     8       2017 "<p>The 2017 NHL Draft Lottery wa… "<table bor… NA          
#>  9     9       2016 "<p>The 2016 NHL Draft Lottery wa… "<table bor… NA          
#> 10    10       2015 "<p>The 2015 NHL Draft Lottery wa… "<table bor… NA          
#> # ℹ 21 more rows
# }
```
