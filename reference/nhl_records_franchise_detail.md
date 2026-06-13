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

A data frame (`fastRhockey_data`) with the following columns:

|                         |           |                                         |
|-------------------------|-----------|-----------------------------------------|
| col_name                | types     | description                             |
| id                      | integer   | Unique franchise detail identifier.     |
| active                  | logical   | Whether the franchise is active.        |
| captain_history         | character | Franchise captain history text.         |
| coaching_history        | character | Franchise coaching history text.        |
| date_awarded            | character | Date the franchise was awarded.         |
| directory_url           | character | Franchise directory URL.                |
| first_season_id         | integer   | Season identifier of the first season.  |
| general_manager_history | character | Franchise general manager history text. |
| hero_image_url          | character | Franchise hero image URL.               |
| most_recent_team_id     | integer   | Most recent team identifier.            |
| retired_numbers_summary | character | Summary of retired jersey numbers.      |
| team_abbrev             | character | Team abbreviation.                      |
| team_full_name          | character | Full team name.                         |

## Examples

``` r
# \donttest{
  try(nhl_records_franchise_detail())
#> ── NHL Records Franchise Detail ─────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-13 03:25:39 UTC
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
