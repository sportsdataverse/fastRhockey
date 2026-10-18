# **Czech Extraliga (ELH) latest play-by-play entries**

The newest 20 play-by-play entries of one game
(`match/{season - 1}/short/{match_id}.json`); same columns as
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md).

## Usage

``` r
elh_pbp_latest(season, match_id, return_parsed = TRUE)
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

As
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md).

## See also

Other ELH Functions:
[`elh_events()`](https://fastRhockey.sportsdataverse.org/reference/elh_events.md),
[`elh_game_ids()`](https://fastRhockey.sportsdataverse.org/reference/elh_game_ids.md),
[`elh_list_objects()`](https://fastRhockey.sportsdataverse.org/reference/elh_list_objects.md),
[`elh_pbp()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp.md),
[`elh_pbp_goals()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_goals.md),
[`elh_pbp_mobile()`](https://fastRhockey.sportsdataverse.org/reference/elh_pbp_mobile.md),
[`elh_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/elh_scoreboard.md),
[`elh_shots()`](https://fastRhockey.sportsdataverse.org/reference/elh_shots.md)

## Examples

``` r
# \donttest{
  try(elh_pbp_latest(season = 2027, match_id = 2928321))
#> ── ELH Pbp Latest Information from hokej.cz ────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:10:13 UTC
#> # A tibble: 20 × 39
#>    comment_id  sort_order event_type   label2 time_type phase clock game_seconds
#>         <int>       <dbl> <chr>        <chr>  <chr>     <chr> <chr>        <int>
#>  1   70333891 70333891000 normal       NA     NA        NA    50:06         3006
#>  2   70333921 70333921000 normal       NA     NA        NA    50:59         3059
#>  3   70334016 70334016000 important    NA     NA        NA    51:35         3095
#>  4   70334081 70334041000 normal       NA     NA        NA    52:12         3132
#>  5   70334041 70334081000 expertComme… NA     NA        3PER  NA              NA
#>  6   70334158 70334158000 normal       NA     NA        NA    52:43         3163
#>  7   70334217 70334217000 normal       NA     NA        NA    53:54         3234
#>  8   70334219 70334311100 goal         NA     NA        NA    54:18         3258
#>  9   70334333 70334333000 normal       NA     NA        NA    55:00         3300
#> 10   70334386 70334386000 normal       NA     NA        NA    56:27         3387
#> 11   70334504 70334504000 normal       NA     NA        NA    57:46         3466
#> 12   70334535 70334535000 normal       NA     NA        NA    58:00         3480
#> 13   70334575 70334575000 normal       NA     NA        NA    58:47         3527
#> 14   70334608 70334608000 normal       NA     NA        NA    59:27         3567
#> 15   70334662 70334662000 normal       NA     NA        NA    59:49         3589
#> 16   70334666 70334666000 time         NA     end       ERT   NA              NA
#> 17   70335226 70334688000 normal       NA     NA        ERT   NA              NA
#> 18   70334741 70334704000 normal       NA     NA        ERT   NA              NA
#> 19   70334688 70334741000 normal       NA     NA        ERT   NA              NA
#> 20   70334704 70335226000 normal       NA     NA        ERT   NA              NA
#> # ℹ 31 more variables: period <int>, home_score <int>, away_score <int>,
#> #   home_shootout_score <int>, away_shootout_score <int>, event_id <int>,
#> #   onlajny_team_id <int>, onlajny_team_global_id <int>, team_code <chr>,
#> #   onlajny_player1_id <int>, onlajny_player1_global_id <int>,
#> #   player1_name <chr>, player1_first_name <chr>, player1_last_name <chr>,
#> #   onlajny_player2_id <int>, onlajny_player2_global_id <int>,
#> #   player2_name <chr>, player2_first_name <chr>, player2_last_name <chr>, …
# }
```
