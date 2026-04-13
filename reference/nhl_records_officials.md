# **NHL Records - On-Ice Officials**

Returns NHL on-ice official listings from the NHL Records API
(`https://records.nhl.com/site/api/officials`). Optionally filter by
type (e.g. `"referee"`, `"linesman"`), which switches the resource to
`officials/{type}`.

## Usage

``` r
nhl_records_officials(type = NULL)
```

## Arguments

- type:

  Optional character official type. If supplied, the resource becomes
  `officials/{type}`.

## Value

A `fastRhockey_data` tibble of officials, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_records_officials())
#> ── NHL Records Officials ────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:40 UTC
#> # A tibble: 600 × 23
#>       id active association_url birth_city     birth_date  coach_id country_code
#>    <int> <lgl>  <chr>           <chr>          <chr>          <int> <chr>       
#>  1   100 FALSE  NA              NA             NA                NA NA          
#>  2   101 FALSE  NA              NA             NA                NA NA          
#>  3   102 FALSE  NA              NA             NA                NA NA          
#>  4   103 FALSE  NA              NA             NA                NA NA          
#>  5   104 FALSE  NA              NA             NA                NA NA          
#>  6   105 FALSE  NA              Plympton       1932-12-20…       NA CAN         
#>  7   106 FALSE  NA              Middlesborough 1947-05-28…       NA GBR         
#>  8   107 FALSE  NA              Guelph         NA                NA CAN         
#>  9   108 FALSE  NA              Galt           1930-03-05…       NA CAN         
#> 10   109 FALSE  NA              Toronto        1946-11-06…       NA CAN         
#> # ℹ 590 more rows
#> # ℹ 16 more variables: deceased <lgl>, deceased_date <chr>, first_name <chr>,
#> #   first_playoff_game_id <int>, first_regular_game_id <int>,
#> #   general_manager_id <lgl>, headshot_url <chr>, last_name <chr>,
#> #   nationality_code <chr>, official_type <chr>, officials_schema_id <int>,
#> #   player_id <int>, referree_association_id <int>, state_province_code <chr>,
#> #   sweater_number <int>, thumb_url <chr>
# }
```
