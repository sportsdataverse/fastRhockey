# **NHL Records - Trophy Listing**

Returns the NHL trophy listing from the NHL Records API
(`https://records.nhl.com/site/api/trophy`).

## Usage

``` r
nhl_records_trophy(cayenne_exp = NULL)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string.

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                   |           |                                      |
|-------------------|-----------|--------------------------------------|
| col_name          | types     | description                          |
| id                | integer   | Unique trophy identifier.            |
| brief_description | character | Brief description of the trophy.     |
| category_id       | integer   | Trophy category identifier.          |
| created_on        | character | Date the trophy record was created.  |
| description       | character | Full description of the trophy.      |
| footnote          | logical   | Footnote associated with the trophy. |
| home_page_url     | character | URL to the trophy's home page.       |
| image_url         | character | URL to the trophy's image.           |
| name              | character | Full name of the trophy.             |
| short_name        | character | Short name of the trophy.            |

## Examples

``` r
# \donttest{
  try(nhl_records_trophy())
#> ── NHL Records Trophy ───────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-18 17:04:30 UTC
#> # A tibble: 25 × 10
#>       id brief_description           category_id created_on description footnote
#>    <int> <chr>                             <int> <chr>      <chr>       <lgl>   
#>  1     1 NHL Champion                          1 2026-06-1… "<p><stron… NA      
#>  2     2 Coach of the Year                     3 2026-06-0… "<p><b>Des… NA      
#>  3     3 Sportsmanship and Gentlema…           2 2026-06-0… "<p><b>Des… NA      
#>  4     4 Rookie of the Year                    2 2026-05-1… "<p><b>Des… NA      
#>  5     5 Western Conference Champion           1 2026-05-2… "<p><b>Des… NA      
#>  6     6 Leadership and Humanitaria…           2 2026-06-0… "<p><b>Des… NA      
#>  7     7 MVP of Stanley Cup Playoffs           2 2026-06-1… "<p><b>Des… NA      
#>  8     8 MVP of Regular Season                 2 2026-06-1… "<p><b>Des… NA      
#>  9     9 Goaltender(s) on Team with…           2 2026-04-1… "<p><b>Des… NA      
#> 10    10 Perseverance, Sportsmanshi…           2 2026-06-0… "<p><b>Des… NA      
#> # ℹ 15 more rows
#> # ℹ 4 more variables: home_page_url <chr>, image_url <chr>, name <chr>,
#> #   short_name <chr>
# }
```
