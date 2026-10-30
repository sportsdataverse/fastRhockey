# **KHL Webcaster Players (Light)**

Every current player: webcaster id, khl.ru id, jersey and team (one row
per player).

## Usage

``` r
khl_webcaster_players_light(locale = "en", return_parsed = TRUE)
```

## Arguments

- locale:

  Response language: en or ru. One of `en`, `ru`.

- return_parsed:

  Logical. `TRUE` (default) returns the parsed `fastRhockey_data`
  tibble; `FALSE` returns the raw JSON body as a list (betting fields
  and the caller's IP removed).

## Value

A `fastRhockey_data` tibble (10 columns; zero rows with the same columns
when the payload is empty), or the raw body as a list when
`return_parsed = FALSE`. Raises `fastRhockey_no_data` (nothing there, or
an unknown season / stage), `fastRhockey_asset_fetch` (an error
envelope, an answer for another id than the one asked, or a failed
fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP 400
/ 422).

|  |  |  |
|----|----|----|
| col_name | types | description |
| id | integer | Webcaster id of the `row` object (webcaster key space); join to khl.ru through its khl_id |
| shirt_number | integer | Jersey number (not an id) |
| khl_id | integer | khl.ru id of the `row` object (site key space): the join key to khl-rest |
| image | character | Player photo URL on khl.ru |
| name | character | Name (English with locale=en) |
| team_id | integer | Webcaster id of the `team` object (webcaster key space); join to khl.ru through its khl_id |
| team_khl_id | integer | khl.ru club id of the `team` object (site key space): the join key to khl-rest |
| team_name | character | Team name as the site prints it |
| team_location | character | Team city / region (English with locale=en) |
| team | character | Team (webcaster) |

## Details

Endpoint:
`GET https://khl.api.webcaster.pro/api/khl_mobile/players_v2_light.json`.

The webcaster API is keyless JSON. Neither backend takes a year:
`season` (the END year, 2026 = 2025/26) and `stage` are mapped to the
webcaster stage id through the season table of
[`khl_webcaster_stages()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_stages.md)
(cached for the R session), or pass `stage_id`. Honours
`getOption("fastRhockey.proxy")`; betting fields and the caller's IP are
removed from every body.

Ids and counts are integers (sdv-py Int64); a column holding values past
2^31 (epoch milliseconds) is returned as a whole-number double.

At least one second is left between two requests to a host.

**Owner flag (sanctions).** The KHL and its webcaster platform are run
by Russian operators, some clubs by sanctioned state companies. Fetching
on your own machine is fine; any published data release, hosted mirror
or redistribution needs owner (sanctions / legal) sign-off first.

## See also

Other KHL Functions:
[`khl_game_header()`](https://fastRhockey.sportsdataverse.org/reference/khl_game_header.md),
[`khl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/khl_game_info.md),
[`khl_game_protocol()`](https://fastRhockey.sportsdataverse.org/reference/khl_game_protocol.md),
[`khl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/khl_pbp.md),
[`khl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_box.md),
[`khl_player_career()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_career.md),
[`khl_player_profile()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_profile.md),
[`khl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_stats.md),
[`khl_playoff_bracket()`](https://fastRhockey.sportsdataverse.org/reference/khl_playoff_bracket.md),
[`khl_roster()`](https://fastRhockey.sportsdataverse.org/reference/khl_roster.md),
[`khl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/khl_schedule.md),
[`khl_shots()`](https://fastRhockey.sportsdataverse.org/reference/khl_shots.md),
[`khl_standings()`](https://fastRhockey.sportsdataverse.org/reference/khl_standings.md),
[`khl_stat_filters()`](https://fastRhockey.sportsdataverse.org/reference/khl_stat_filters.md),
[`khl_webcaster_event()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_event.md),
[`khl_webcaster_events()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_events.md),
[`khl_webcaster_leaders()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_leaders.md),
[`khl_webcaster_players()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_players.md),
[`khl_webcaster_stages()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_stages.md),
[`khl_webcaster_tables()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_tables.md),
[`khl_webcaster_team()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_team.md),
[`khl_webcaster_teams()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_teams.md)

## Examples

``` r
# \donttest{
  try(khl_webcaster_players_light())
#> ── KHL Webcaster Players (Light) from khl.api.webcaster.pro ────────────────────
#> ℹ Data updated: 2026-10-30 00:15:13 UTC
#> # A tibble: 750 × 10
#>       id shirt_number khl_id image           name  team_id team_khl_id team_name
#>    <int>        <int>  <int> <chr>           <chr>   <int>       <int> <chr>    
#>  1 33797           11  23248 https://www.kh… Abra…      12          54 Amur     
#>  2 43535           97  30941 https://www.kh… Abra…      NA          NA NA       
#>  3 35165           17  30629 https://www.kh… Abro…      42          56 Severstal
#>  4 47215           33  43281 https://www.kh… Agaf…      NA          NA NA       
#>  5 47627           95  40373 https://www.kh… Agla…      NA          NA NA       
#>  6  4993           77  18961 https://www.kh… Adam…      10          34 Avangard 
#>  7 28477           53  35353 https://www.kh… Aimu…      44          24 SKA      
#>  8 38403           19  33710 https://www.kh… Akma…      NA          NA NA       
#>  9  7699           90  15620 https://www.kh… Akol…      NA          NA NA       
#> 10 28149           61  34493 https://www.kh… Alal…      32          38 Salavat …
#> # ℹ 740 more rows
#> # ℹ 2 more variables: team_location <chr>, team <chr>
# }
```
