# **NHL Records - Draft Lottery Picks**

Returns historical NHL draft lottery picks/results from the NHL Records
API (`https://records.nhl.com/site/api/draft-lottery-picks`).

## Usage

``` r
nhl_records_draft_lottery_picks(cayenne_exp = NULL)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string.

## Value

A `fastRhockey_data` tibble of draft lottery picks, or `NULL` on
failure.

## Examples

``` r
# \donttest{
  try(nhl_records_draft_lottery_picks())
#> ── NHL Records Draft Lottery Picks ──────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:28 UTC
#> # A tibble: 46 × 9
#>       id draft_team_id draft_year effect_on_draft_order    lottery_team_id phase
#>    <int>         <int>      <int> <chr>                              <int> <chr>
#>  1     1             3       2020 Moved from (8th - 15th)…               3 NA   
#>  2     2            26       2020 Moved from 4th to 2nd                 26 NA   
#>  3     3             9       2020 Retained 3rd Selection                 9 NA   
#>  4     4             1       2019 Moved from 3rd to 1st                  1 ""   
#>  5     5             3       2019 Moved from 6th to 2nd                  3 ""   
#>  6     6            16       2019 Moved from 12th to 3rd                16 ""   
#>  7     7             7       2018 Retained 1st Selection                 7 ""   
#>  8     8            12       2018 Moved from 11th to 2nd                12 ""   
#>  9     9             8       2018 Moved from 4th to 3rd                  8 ""   
#> 10    10             1       2017 Moved from 5th to 1st                  1 ""   
#> # ℹ 36 more rows
#> # ℹ 3 more variables: player_id <int>, player_notes <chr>, sequence <int>
# }
```
