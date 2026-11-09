# **KHL Roster**

Club roster (current season only): one row per player.

## Usage

``` r
khl_roster(club_id, headers = NULL, return_parsed = TRUE)
```

## Arguments

- club_id:

  Required. khl.ru CLUB id (7 = Spartak; schedule home_club_id), not a
  team-season id.

- headers:

  Named list of extra request headers, merged over the defaults
  (`X-Requested-With`, `Origin`, `Referer`). The cookie jar and `sessid`
  are kept in-process, so nothing needs reusing across calls.

- return_parsed:

  Logical. `TRUE` (default) returns the parsed `fastRhockey_data`
  tibble; `FALSE` returns the raw khl.ru envelope as a list (`status`,
  `data`, `errors`; betting fields removed).

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
| group | character | List the row comes from (position group / roster section) |
| player_id | integer | khl.ru player id; equals webcaster player khl_id |
| photo | character | Player photo URL on khl.ru (protocol-relative) |
| country | character | Citizenship (Russian) |
| lastname | character | Last name in Russian |
| firstname | character | First name in Russian |
| jn | integer | Jersey number (not an id; 0 on a team event) |
| age | integer | Player age in years |
| favorite | logical | Front-end favourite flag of the signed-in user (always False here) |
| link | character | Relative khl.ru page path |

## Details

Endpoint: `POST https://www.khl.ru/rest/clubs/team/`.

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
  try(khl_roster(club_id = 7))
#> ── KHL Roster from khl.ru ───────────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 22:53:39 UTC
#> # A tibble: 31 × 10
#>    group   player_id photo country lastname firstname    jn   age favorite link 
#>    <chr>       <int> <chr> <chr>   <chr>    <chr>     <int> <int> <lgl>    <chr>
#>  1 goalke…     41683 //im… Россия  Волохин  Евгений      35    21 FALSE    /pla…
#>  2 goalke…     19184 //im… Россия  Загидул… Артём        50    31 FALSE    /pla…
#>  3 goalke…     43224 //im… Россия  Мидлак   Егор         30    19 FALSE    /pla…
#>  4 goalke…     20870 //im… Россия  Федотов  Иван         28    29 FALSE    /pla…
#>  5 defend…     19062 //im… Россия  Брынцев  Александр    74    31 FALSE    /pla…
#>  6 defend…     30201 //im… Россия  Бычков   Роман        70    25 FALSE    /pla…
#>  7 defend…     15299 //im… Россия  Вишневс… Дмитрий      55    36 FALSE    /pla…
#>  8 defend…     43948 //im… Россия  Дронов   Николай      49    18 FALSE    /pla…
#>  9 defend…     41928 //im… США     Кин      Джозеф        4    27 FALSE    /pla…
#> 10 defend…     29100 //im… Россия  Князев   Артемий       8    25 FALSE    /pla…
#> # ℹ 21 more rows
# }
```
