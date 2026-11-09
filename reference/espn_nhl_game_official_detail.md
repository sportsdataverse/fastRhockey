# **Get ESPN NHL Game Official Detail (core-v2)**

**Get ESPN NHL Game Official Detail (core-v2)**

## Usage

``` r
espn_nhl_game_official_detail(event_id, cid = event_id, official_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier.

- cid:

  Competition identifier. Defaults to `event_id`.

- official_id:

  ESPN official identifier. Use
  `espn_nhl_game_officials(event_id)$official_id` to find valid official
  ids for a game.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with official detail:

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| cid | character | Competition id (echoed from arg). |
| official_id | character | ESPN official id (echoed from arg). |
| id | character | ESPN official identifier. |
| first_name | character | Official first name. |
| last_name | character | Official last name. |
| full_name | character | Official full name. |
| display_name | character | Official display name. |
| order | integer | Display order within officials list. |
| position_id | character | Official position identifier. |
| position_name | character | Official position name (e.g. "Linesman", "Referee"). |
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
    sb   <- espn_nhl_scoreboard(dates = "20250110")
    eid  <- sb$game_id[1]
    offs <- espn_nhl_game_officials(event_id = eid)
    espn_nhl_game_official_detail(event_id = eid, official_id = offs$official_id[1])
  })
#> ── NHL Game Official Detail data from ESPN core-v2 ──────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:49:47 UTC
#> # A tibble: 1 × 13
#>   event_id  cid    official_id id    first_name last_name full_name display_name
#>   <chr>     <chr>  <chr>       <chr> <chr>      <chr>     <chr>     <chr>       
#> 1 401688263 40168… 2553192     2553… Derek      Nansen    Derek Na… Derek Nansen
#> # ℹ 5 more variables: order <int>, position_id <chr>, position_name <chr>,
#> #   position_display <chr>, official_ref <chr>
# }
```
