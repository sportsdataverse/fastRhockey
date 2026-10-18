# **Czech Extraliga (ELH) mobile play-by-play**

Play-by-play of one game as plain text, oldest first
(`match/{season - 1}/mobile/{match_id}.json`): the same entries as
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md),
flattened to text with no team, player or game ids. `comment_id` equals
the full file's `comment_id`; `game_seconds` counts from the start of
the game (`0` before it).

## Usage

``` r
elh_pbp_mobile(season, match_id, return_parsed = TRUE)
```

## Arguments

- season:

  Season by its END year (2027 = 2026-27); the bucket files it under the
  START year (`match/2026/`), mapped here. Play-by-play starts in
  2014-15 (season 2015).

- match_id:

  hokej.cz game id (the S3 file name; `hokejcz_game_id` of
  [`elh_game_ids()`](https://fastRhockey.sportsdataverse.org/reference/elh_game_ids.md)
  /
  [`elh_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/elh_scoreboard.md)),
  e.g. 2928321. Not the onlajny game id.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the
  raw JSON as a list (XML-converted: every attribute a string).

## Value

A tibble (`return_parsed = TRUE`; zero rows for a malformed body), else
the raw JSON list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| comment_id | integer | Onlajny commentary entry id (integer); equals the mobile file's comment id. |
| event_type | character | Entry kind: goal, penalty, time (period start/end), important, normal, expertComment. |
| game_seconds | integer | Seconds from the start of the game. |
| content | character | Czech commentary text (plain, newline line breaks). |

## Errors

As
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md).

## See also

Other ELH Functions:
[`elh_events()`](https://fastRhockey.sportsdataverse.org/reference/elh_events.md),
[`elh_game_ids()`](https://fastRhockey.sportsdataverse.org/reference/elh_game_ids.md),
[`elh_list_objects()`](https://fastRhockey.sportsdataverse.org/reference/elh_list_objects.md),
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md),
[`elh_pbp_goals()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_goals.md),
[`elh_pbp_latest()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_latest.md),
[`elh_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/elh_scoreboard.md),
[`elh_shots()`](https://fastRhockey.sportsdataverse.org/reference/elh_shots.md)

## Examples

``` r
# \donttest{
  try(elh_pbp_mobile(season = 2027, match_id = 2928321))
#> ── ELH Pbp Mobile Information from hokej.cz ────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:02:32 UTC
#> # A tibble: 119 × 4
#>    comment_id event_type game_seconds content                                   
#>         <int> <chr>             <int> <chr>                                     
#>  1   69014008 normal                0 "Vítáme vás u on-line přenosu. Utkání zač…
#>  2   70313395 normal                0 "Hezké nedělní odpoledne z O2 areny! Hoke…
#>  3   70313401 normal                0 "HC Sparta Praha\n\nSpartě vstup do letoš…
#>  4   70313403 normal                0 "HC Vítkovice Ridera\n\nOstravané jsou na…
#>  5   70313544 normal                0 "Vzájemné zápasy\n\nV rámci minulého ročn…
#>  6   70326165 normal                0 "Úvodní sestavy:\n\nHC Sparta Praha: Koře…
#>  7   70328126 time                  1 "Utkání právě začalo."                    
#>  8   70328146 normal                8 "Jako první se kotouče ujímají hosté. Kal…
#>  9   70328174 normal               30 "První střelu na branku ale vyslal Knot n…
#> 10   70328231 normal              103 "Vesalainen za brankou soupeře dojel naho…
#> # ℹ 109 more rows
# }
```
