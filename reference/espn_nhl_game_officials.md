# **Get ESPN NHL Game Officials (core-v2)**

**Get ESPN NHL Game Officials (core-v2)**

## Usage

``` r
espn_nhl_game_officials(event_id, cid = event_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per official (typically 4 - 2
referees + 2 linesmen):

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| official_id | character | ESPN official identifier. |
| first_name | character | Official first name. |
| last_name | character | Official last name. |
| full_name | character | Official full name. |
| display_name | character | Official display name. |
| order | integer | Display order within officials list. |
| position_id | character | Official position identifier. |
| position_name | character | Official position name (e.g. "Referee", "Linesman"). |
| position_display | character | Official position display name. |
| official_ref | character | `$ref` URL for this official object. |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    sb <- espn_nhl_scoreboard(dates = "20250110")
    espn_nhl_game_officials(event_id = sb$game_id[1])
  })
#> ── NHL Game Officials data from ESPN core-v2 ────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:49:47 UTC
#> # A tibble: 4 × 12
#>   event_id  cid    official_id first_name last_name full_name display_name order
#>   <chr>     <chr>  <chr>       <chr>      <chr>     <chr>     <chr>        <int>
#> 1 401688263 40168… 2553192     Derek      Nansen    Derek Na… Derek Nansen     0
#> 2 401688263 40168… 2553195     Brian      Pochmara  Brian Po… Brian Pochm…     0
#> 3 401688263 40168… 4271781     Andrew     Smith     Andrew S… Andrew Smith     0
#> 4 401688263 40168… 4392737     Mitch      Dunning   Mitch Du… Mitch Dunni…     0
#> # ℹ 4 more variables: position_id <chr>, position_name <chr>,
#> #   position_display <chr>, official_ref <chr>
# }
```
