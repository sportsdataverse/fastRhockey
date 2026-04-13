# **NHL Postal Lookup**

Returns broadcast region / geo-lookup information for a given postal (or
ZIP) code from the NHL web service endpoint
`postal-lookup/{postalCode}`. Used internally by NHL.com to determine
local broadcast rights.

## Usage

``` r
nhl_postal_lookup(postal_code)
```

## Arguments

- postal_code:

  Character. Postal or ZIP code to look up (e.g., `"10001"` for New York
  City).

## Value

Returns a list with broadcast region information for the given postal
code.

## Examples

``` r
# \donttest{
  try(nhl_postal_lookup(postal_code = "10001"))
#>   stateProvince networkType   county postalCode country      city
#> 1            NY      50 MHT New York      10001      US Manhattan
#> 2            NY      50 MHT New York      10001      US Manhattan
#> 3            NY      50 MHT New York      10001      US Manhattan
#>     teamName.default           teamName.fr
#> 1  New Jersey Devils  Devils du New Jersey
#> 2 New York Islanders Islanders de New York
#> 3   New York Rangers   Rangers de New York
# }
```
