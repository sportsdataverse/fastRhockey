# **NHL Stats API — Miscellaneous Endpoints**

Generic dispatcher for the NHL Stats REST API
(`https://api.nhle.com/stats/rest/{lang}/{endpoint}`). Pass any valid
endpoint name as `endpoint` and the helper will issue the request and
return the parsed `data` element as a `data.frame`. For non-tabular
endpoints, the raw parsed list is returned instead.

## Usage

``` r
nhl_stats_misc(endpoint = "glossary", game_id = NULL, lang = "en")
```

## Arguments

- endpoint:

  Character endpoint path. Known valid values include:

  - **Tabular:** `"franchise"`, `"glossary"`, `"country"`, `"config"`,
    `"players"`, `"team"`, `"game"`, `"componentSeason"`,
    `"milestones/skaters"`, `"milestones/goalies"`,
    `"leaders/skaters/{attribute}"`, `"leaders/goalies/{attribute}"`

  - **Reference / lookup:** `"gameType"`, `"shiftcharts"` (use the
    dedicated `game_id` argument for this one),
    `"content/module/{templateKey}"`, `"team/id/{id}"`

  - **Health check:** `"ping"`

  Note: dedicated wrappers exist for `season`, `draft`,
  `skater/{report}`, `goalie/{report}`, and `team/{report}` — see
  [`nhl_stats_seasons()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_seasons.md),
  [`nhl_stats_draft()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_draft.md),
  [`nhl_stats_skaters()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_skaters.md),
  [`nhl_stats_goalies()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_goalies.md),
  and
  [`nhl_stats_teams()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_teams.md).

- game_id:

  Optional game ID (required for `endpoint = "shiftcharts"`).

- lang:

  Character language code. Default `"en"`.

## Value

Returns a data frame (`fastRhockey_data`) for tabular endpoints, or the
raw parsed list for non-tabular ones.

## Examples

``` r
# \donttest{
  try(nhl_stats_misc(endpoint = "glossary"))
#> ── NHL Stats Misc ───────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:06:01 UTC
#> # A tibble: 321 × 7
#>       id abbreviation   definition first_season_for_stat full_name language_code
#>    <int> <chr>          <chr>                      <int> <chr>     <chr>        
#>  1  1046 General conce… Teams pla…              20092010 5-on-5 s… en           
#>  2  1047 A              Assists c…              19171918 Assists   en           
#>  3  1048 A (5-on-5)     Player's …              20092010 Assists … en           
#>  4  1049 A/60 (5-on-5)  Player's …              20092010 Assists … en           
#>  5  1050 A/GP           In compar…              19171918 Assists … en           
#>  6  1051 Bench          Bench min…                    NA Bench pe… en           
#>  7  1052 Ctry           A player'…                    NA Birth co… en           
#>  8  1053 BkS            A blocked…              19971998 Blocked … en           
#>  9  1054 BkS/60         Player's …              19971998 Blocked … en           
#> 10  1055 BkS/GP         Blocked s…              19971998 Blocked … en           
#> # ℹ 311 more rows
#> # ℹ 1 more variable: last_updated <chr>
  try(nhl_stats_misc(endpoint = "franchise"))
#> ── NHL Stats Misc ───────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:06:01 UTC
#> # A tibble: 40 × 4
#>       id full_name             team_common_name team_place_name
#>    <int> <chr>                 <chr>            <chr>          
#>  1    32 Anaheim Ducks         Ducks            Anaheim        
#>  2     8 Brooklyn Americans    Americans        Brooklyn       
#>  3    16 Philadelphia Flyers   Flyers           Philadelphia   
#>  4     7 Montreal Maroons      Maroons          Montreal       
#>  5     2 Montreal Wanderers    Wanderers        Montreal       
#>  6    36 Columbus Blue Jackets Blue Jackets     Columbus       
#>  7    11 Chicago Blackhawks    Blackhawks       Chicago        
#>  8    33 Florida Panthers      Panthers         Florida        
#>  9    28 Arizona Coyotes       Coyotes          Arizona        
#> 10    29 San Jose Sharks       Sharks           San Jose       
#> # ℹ 30 more rows
  try(nhl_stats_misc(endpoint = "country"))
#> ── NHL Stats Misc ───────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:06:01 UTC
#> # A tibble: 49 × 11
#>    id    country3code country_code country_name   has_player_stats image_url    
#>    <chr> <chr>        <chr>        <chr>                     <int> <chr>        
#>  1 RUS   RUS          RU           Russia                        1 /images/coun…
#>  2 CAN   CAN          CA           Canada                        1 /images/coun…
#>  3 NGA   NGA          NG           Nigeria                       1 /images/coun…
#>  4 NLD   NLD          NL           Netherlands                   1 /images/coun…
#>  5 NOR   NOR          NO           Norway                        1 /images/coun…
#>  6 KAZ   KAZ          KZ           Kazakhstan                    1 /images/coun…
#>  7 FRA   FRA          FR           France                        1 /images/coun…
#>  8 KOR   KOR          KR           Korea (South)                 1 /images/coun…
#>  9 GBR   GBR          GB           United Kingdom                1 /images/coun…
#> 10 YUG   YUG          YU           Yugoslavia                    0 NA           
#> # ℹ 39 more rows
#> # ℹ 5 more variables: ioc_code <chr>, is_active <int>, nationality_name <chr>,
#> #   olympic_url <chr>, thumbnail_url <chr>
# }
```
