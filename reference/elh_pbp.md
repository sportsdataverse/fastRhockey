# **Czech Extraliga (ELH) play-by-play commentary**

Play-by-play commentary of one Tipsport Extraliga game from hokej.cz's
S3 bucket (`match/{season - 1}/full/{match_id}.json`), every entry,
oldest first: goals and penalties with team and players.

Ids are **onlajny** ids (the commentary system), not the hokej.cz ids of
[`elh_shots()`](https://fastRhockey.sportsdataverse.org/reference/elh_shots.md):
join the two through
[`elh_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/elh_scoreboard.md),
which maps games and teams between the id spaces (only for days still in
its rolling window). `clock` is the elapsed game clock `mm:ss`; a
clockless entry names only its `phase` (`0` pre-game, `1INT`/`2INT`
intermissions, `3PER`, `ERT` after the game; meanings inferred, the
provider documents none). `period` is derived from the clock (20-minute
periods). `strength` (`EQ` = even strength, inferred, the only value
observed) is set only on goals that carry the on-ice block (not in
2016-17).

## Usage

``` r
elh_pbp(season, match_id, return_parsed = TRUE)
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

[TABLE]

## Errors

`fastRhockey_no_data`: a missing file (404 `NoSuchKey`) or a stub body
without comments. `fastRhockey_asset_fetch`: an error envelope, an empty
or non-JSON 200, or any other non-2xx. `fastRhockey_bad_request`: HTTP
400/422 or a bad `season` / `match_id`.

## See also

Other ELH Functions:
[`elh_events()`](https://fastRhockey.sportsdataverse.org/reference/elh_events.md),
[`elh_game_ids()`](https://fastRhockey.sportsdataverse.org/reference/elh_game_ids.md),
[`elh_list_objects()`](https://fastRhockey.sportsdataverse.org/reference/elh_list_objects.md),
[`elh_pbp_goals()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_goals.md),
[`elh_pbp_latest()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_latest.md),
[`elh_pbp_mobile()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_mobile.md),
[`elh_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/elh_scoreboard.md),
[`elh_shots()`](https://fastRhockey.sportsdataverse.org/reference/elh_shots.md)

## Examples

``` r
# \donttest{
  try(elh_pbp(season = 2027, match_id = 2928321))
#> ── ELH Pbp Information from hokej.cz ───────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:10:11 UTC
#> # A tibble: 119 × 39
#>    comment_id  sort_order event_type label2 time_type phase clock game_seconds
#>         <int>       <dbl> <chr>      <chr>  <chr>     <chr> <chr>        <int>
#>  1   69014008 69014008000 normal     NA     NA        0     NA              NA
#>  2   70313395 70294613000 normal     NA     NA        0     NA              NA
#>  3   70313401 70294614000 normal     NA     NA        0     NA              NA
#>  4   70313403 70313395000 normal     NA     NA        0     NA              NA
#>  5   70313544 70313401000 normal     NA     NA        0     NA              NA
#>  6   70326165 70326165000 normal     NA     NA        0     NA              NA
#>  7   70328126 70328126000 time       NA     begin     NA    00:01            1
#>  8   70328146 70328146000 normal     NA     NA        NA    00:08            8
#>  9   70328174 70328174000 normal     NA     NA        NA    00:30           30
#> 10   70328231 70328231000 normal     NA     NA        NA    01:43          103
#> # ℹ 109 more rows
#> # ℹ 31 more variables: period <int>, home_score <int>, away_score <int>,
#> #   home_shootout_score <int>, away_shootout_score <int>, event_id <int>,
#> #   onlajny_team_id <int>, onlajny_team_global_id <int>, team_code <chr>,
#> #   onlajny_player1_id <int>, onlajny_player1_global_id <int>,
#> #   player1_name <chr>, player1_first_name <chr>, player1_last_name <chr>,
#> #   onlajny_player2_id <int>, onlajny_player2_global_id <int>, …
# }
```
