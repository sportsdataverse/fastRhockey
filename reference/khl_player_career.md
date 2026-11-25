# **KHL Player Career**

Player career: one row per season tournament.

## Usage

``` r
khl_player_career(player_id, headers = NULL, return_parsed = TRUE)
```

## Arguments

- player_id:

  Required. khl.ru player id (the box score / roster player_id).

- headers:

  Named list of extra request headers, merged over the defaults
  (`X-Requested-With`, `Origin`, `Referer`). The cookie jar and `sessid`
  are kept in-process, so nothing needs reusing across calls.

- return_parsed:

  Logical. `TRUE` (default) returns the parsed `fastRhockey_data`
  tibble; `FALSE` returns the raw khl.ru envelope as a list (`status`,
  `data`, `errors`; betting fields removed).

## Value

A `fastRhockey_data` tibble (40 columns; zero rows with the same columns
when the payload is empty), or the raw body as a list when
`return_parsed = FALSE`. Raises `fastRhockey_no_data` (nothing there, or
an unknown season / stage), `fastRhockey_asset_fetch` (an error
envelope, an answer for another id than the one asked, or a failed
fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP 400
/ 422).

|  |  |  |
|----|----|----|
| col_name | types | description |
| player_id | integer | khl.ru player id; equals webcaster player khl_id |
| season_label | character | Season label as sent (26/27, 2025/2026) |
| tournament_id | integer | khl.ru tournament (stage) id, tnId: 1369 = 2025-26 regular season, 1370 its playoff; equals webcaster stage khl_id |
| team_season_id | integer | Team-season id (tid; one per tournament; joins nothing in the webcaster API) |
| club_id | integer | khl.ru CLUB id (stable across seasons; joins webcaster team khl_id) |
| team_name | character | Team name as the site prints it |
| jn | integer | Jersey number (not an id; 0 on a team event) |
| gp | integer | Games played by the player |
| g | integer | Goals scored by the player |
| a | integer | Assists credited to the player |
| pts | integer | Points (goals plus assists) |
| pm | integer | Plus-minus rating of the player |
| plus | integer | Plus (on ice for a goal for) |
| minus | integer | Minus (on ice for a goal against) |
| pim | integer | Penalty minutes |
| eqg | integer | Even-strength goals |
| ppg | integer | Power-play goals |
| shg | integer | Short-handed goals |
| otg | integer | Overtime goals scored by the player |
| gwg | integer | Game-winning goals |
| sds | integer | Shootout-deciding goals |
| sog | integer | Shots on goal by the player |
| sog_pct | numeric | Shooting percentage |
| sog_avg | numeric | Shots on goal per game |
| fo | integer | Faceoffs taken by the player |
| fow | integer | Faceoffs won by the player |
| fo_pct | numeric | Faceoff win percentage |
| toi_avg | character | Average time on ice per game (mm:ss) |
| sft_avg | numeric | Average shifts per game |
| tie_avg | character | Average even-strength time on ice per game |
| sfte_avg | numeric | Average even-strength shifts per game |
| tipp_avg | character | Average power-play time on ice per game |
| sftpp_avg | numeric | Average power-play shifts per game |
| tish_avg | character | Average short-handed time on ice per game |
| sftsh_avg | numeric | Average short-handed shifts per game |
| hits | integer | Hits (body checks) |
| bls | integer | Shots blocked by the player |
| foa | integer | Faceoffs lost by the player |
| tka | integer | Takeaways by the player |
| p_intc | integer | Pass interceptions |

## Details

Endpoint: `POST https://www.khl.ru/rest/players/stats/`.

khl.ru REST needs a primed session (a GET of the site root sets the
cookie jar and the Bitrix `sessid`, then each call POSTs a form); this
is done for you, one session per host per R session, re-primed when the
site rotates its cookie or the `sessid` expires. No route takes a year:
`season` (the END year, 2026 = 2025/26) and `stage` are mapped to the
tournament id (tnId) through the webcaster season table, or pass
`tournament_id` (the All-Star games have their own tnId).

Ids and counts are integers; a column holding values past 2^31 (epoch
milliseconds) is returned as a whole-number double.

At least one second is left between two requests to a host.

Check the provider's terms of use before redistributing this data.

## See also

[family_khl](https://fastRhockey.sportsdataverse.org/reference/family_khl.md)
lists every function in this family.

## Examples

``` r
# \donttest{
  try(khl_player_career(player_id = 41928))
#> Error in .khl_prime(session, timeout) : 
#>   khl.ru served an anti-bot challenge page; try again later or through
#> `getOption('fastRhockey.proxy')`.
# }
```
