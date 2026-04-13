# **NHL Records - Draft Combine Measurements**

Returns NHL Scouting Combine measurements from the NHL Records API
(`https://records.nhl.com/site/api/combine`).

## Usage

``` r
nhl_records_combine(cayenne_exp = NULL)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string.

## Value

A `fastRhockey_data` tibble of combine measurements, or `NULL` on
failure.

## Examples

``` r
# \donttest{
  try(nhl_records_combine())
#> ── NHL Records Combine ──────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:27 UTC
#> # A tibble: 182 × 10
#>       id amateur_club_name  amateur_league draft_year event first_name last_name
#>    <int> <chr>              <chr>               <int> <chr> <chr>      <chr>    
#>  1     1 Poprad             SLOVAKIA             2022 Benc… Filip      Mesar    
#>  2     2 St. Sebastian’s    HIGH-MA              2019 Benc… Jayden     Struble  
#>  3     3 Avon Old Farms     HIGH-CT              2022 Benc… Brennan    Ali      
#>  4     4 North Bay          OHL                  2022 Benc… Liam       Arnsby   
#>  5     5 USA U-18           NTDP                 2022 Benc… Seamus     Casey    
#>  6     6 Guelph             OHL                  2022 Benc… Jake       Karabela 
#>  7     7 Barrie             OHL                  2024 Benc… Cole       Beaudoin 
#>  8     8 Northeastern Univ. H-EAST               2022 Benc… Jack       Hughes   
#>  9     9 Skelleftea Jr.     SWEDEN-JR            2023 Benc… Axel       Sandin-P…
#> 10    10 Sarnia             OHL                  2024 Benc… Lukas      Fischer  
#> # ℹ 172 more rows
#> # ℹ 3 more variables: player_id <int>, position_code <chr>, value <dbl>
# }
```
