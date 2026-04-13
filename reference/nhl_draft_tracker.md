# **NHL Draft Tracker**

Returns the live (real-time) NHL draft tracker picks from the NHL web
service endpoint `draft-tracker/picks/now`.

This is distinct from
[`nhl_draft()`](https://fastRhockey.sportsdataverse.org/reference/nhl_draft.md),
which hits `draft/picks/now` and returns the static draft board. The
draft tracker is only populated during an active NHL draft window and
will return `NULL` (or an empty payload) outside of that window.

## Usage

``` r
nhl_draft_tracker()
```

## Value

Returns a data frame of live draft picks, or `NULL` if no draft is
currently active.

## Examples

``` r
# \donttest{
  try(nhl_draft_tracker())
#> ── NHL Draft Tracker data from NHL.com ──────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:04:29 UTC
#> # A tibble: 32 × 16
#>    pick_in_round overall_pick team_id team_abbrev team_logo_light team_logo_dark
#>            <int>        <int>   <int> <chr>       <chr>           <chr>         
#>  1             1            1       2 NYI         https://assets… https://asset…
#>  2             2            2      28 SJS         https://assets… https://asset…
#>  3             3            3      16 CHI         https://assets… https://asset…
#>  4             4            4      68 UTA         https://assets… https://asset…
#>  5             5            5      18 NSH         https://assets… https://asset…
#>  6             6            6       4 PHI         https://assets… https://asset…
#>  7             7            7       6 BOS         https://assets… https://asset…
#>  8             8            8      55 SEA         https://assets… https://asset…
#>  9             9            9       7 BUF         https://assets… https://asset…
#> 10            10           10      24 ANA         https://assets… https://asset…
#> # ℹ 22 more rows
#> # ℹ 10 more variables: state <chr>, position_code <chr>,
#> #   team_full_name_default <chr>, team_full_name_fr <chr>,
#> #   team_common_name_default <chr>, team_common_name_fr <chr>,
#> #   team_place_name_with_preposition_default <chr>,
#> #   team_place_name_with_preposition_fr <chr>, last_name_default <chr>,
#> #   first_name_default <chr>
# }
```
