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

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| id | integer | Unique official record identifier. |
| active | logical | Whether the official is currently active. |
| association_url | character | URL to the official's association profile. |
| birth_city | character | Birth city of the official. |
| birth_date | character | Birth date of the official. |
| coach_id | integer | Associated coach identifier, if any. |
| country_code | character | Country code of the official. |
| deceased | logical | Whether the official is deceased. |
| deceased_date | character | Date of death, if applicable. |
| first_name | character | First name of the official. |
| first_playoff_game_id | integer | Game ID of the official's first playoff game. |
| first_regular_game_id | integer | Game ID of the official's first regular game. |
| general_manager_id | logical | Associated general manager identifier. |
| headshot_url | character | URL to the official's headshot image. |
| last_name | character | Last name of the official. |
| nationality_code | character | Nationality code of the official. |
| official_type | character | Type of official (e.g. referee, linesman). |
| officials_schema_id | integer | Officials schema identifier. |
| player_id | integer | Associated player identifier, if any. |
| referree_association_id | integer | Referee association identifier. |
| state_province_code | character | State or province code of the official. |
| sweater_number | integer | Sweater number worn by the official. |
| thumb_url | character | URL to the official's thumbnail image. |

## Examples

``` r
# \donttest{
  try(nhl_records_officials())
#> ── NHL Records Officials ────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-08 11:44:43 UTC
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
