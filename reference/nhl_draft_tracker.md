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

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| pick_in_round | integer | Pick number within the round. |
| overall_pick | integer | Overall pick number in the draft. |
| team_id | integer | Unique team identifier. |
| team_abbrev | character | Team abbreviation. |
| team_logo_light | character | URL to the team logo (light variant). |
| team_logo_dark | character | URL to the team logo (dark variant). |
| state | character | Pick state (e.g., on the clock, complete). |
| position_code | character | Player position code. |
| team_full_name_default | character | Team full name (default language). |
| team_full_name_fr | character | Team full name (French). |
| team_common_name_default | character | Team common name (default language). |
| team_common_name_fr | character | Team common name (French). |
| team_place_name_with_preposition_default | character | Team place name with preposition (default). |
| team_place_name_with_preposition_fr | character | Team place name with preposition (French). |
| last_name_default | character | Player last name (default language). |
| first_name_default | character | Player first name (default language). |

## Examples

``` r
# \donttest{
  try(nhl_draft_tracker())
#> ── NHL Draft Tracker data from NHL.com ──────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 18:26:32 UTC
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
