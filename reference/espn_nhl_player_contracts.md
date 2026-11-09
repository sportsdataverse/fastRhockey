# **Get ESPN NHL Player Contracts (core-v2)**

**Get ESPN NHL Player Contracts (core-v2)**

## Usage

``` r
espn_nhl_player_contracts(athlete_id, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per contract, or an empty data
frame if no contract data is served. As of 2026-06-08, ESPN core-v2
returns `count=0` for all queried NHL players (DONE_WITH_CONCERNS:
endpoint exists but data is not populated).

|             |           |                                               |
|-------------|-----------|-----------------------------------------------|
| col_name    | types     | description                                   |
| ref         | character | `$ref` URL for the contract object.           |
| contract_id | character | Contract id parsed from the `$ref` URL.       |
| athlete_id  | character | ESPN athlete id (echoed from arg).            |
| count       | integer   | Total contract count (0 in practice for NHL). |
| page_count  | integer   | Total page count.                             |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_player_contracts(athlete_id = "4024820"))
#> ! espn_nhl_player_contracts(): No contract data served by ESPN core-v2 for athlete 4024820 in the NHL. The endpoint responds with count=0 for all queried NHL players (DONE_WITH_CONCERNS: endpoint exists but data is not populated).
#> data frame with 0 columns and 0 rows
# }
```
