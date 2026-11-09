# **Get ESPN NHL Game Metadata (core-v2)**

**Get ESPN NHL Game Metadata (core-v2)**

## Usage

``` r
espn_nhl_game(event_id, ...)
```

## Arguments

- event_id:

  ESPN event (game) identifier. Use `espn_nhl_scoreboard()$game_id` to
  find valid event ids.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with event metadata:

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_id | character | ESPN event id (echoed from arg). |
| id | character | ESPN event identifier. |
| uid | character | Event uid string. |
| date | character | Game date/time (ISO 8601 UTC). |
| name | character | Full game name (e.g. "Chicago Blackhawks at Detroit Red Wings"). |
| short_name | character | Short name (e.g. "CHI \\ DET"). |
| time_valid | logical | Whether the start time is confirmed. |
| competition_id | character | Id of the primary competition (equals `event_id` for NHL). |
| competition_ref | character | `$ref` URL for the primary competition object. |
| venue_ref | character | `$ref` URL for the venue. |
| season_ref | character | `$ref` URL for the season. |
| season_type_ref | character | `$ref` URL for the season type. |
| league_ref | character | `$ref` URL for the league. |
| event_ref | character | `$ref` URL for this event object. |

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
    espn_nhl_game(event_id = sb$game_id[1])
  })
#> ── NHL Game data from ESPN core-v2 ──────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:49:44 UTC
#> # A tibble: 1 × 14
#>   event_id  id        uid       date  name  short_name time_valid competition_id
#>   <chr>     <chr>     <chr>     <chr> <chr> <chr>      <lgl>      <chr>         
#> 1 401688263 401688263 s:70~l:9… 2025… Chic… CHI @ DET  TRUE       401688263     
#> # ℹ 6 more variables: competition_ref <chr>, venue_ref <chr>, season_ref <chr>,
#> #   season_type_ref <chr>, league_ref <chr>, event_ref <chr>
# }
```
