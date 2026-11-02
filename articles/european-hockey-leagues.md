# European Hockey Data in R: SHL, Liiga, DEL, KHL, National League and More

**`fastRhockey` reads the public data feeds of thirteen European hockey
leagues.** Sweden’s SHL and HockeyAllsvenskan, Finland’s Liiga,
Germany’s DEL, the multinational ICE Hockey League, the UK’s EIHL,
Denmark’s Metal Ligaen, the KHL and VHL, Switzerland’s National League
and federation (SIHF) feeds, the Czech Extraliga and France’s Ligue
Magnus. Each league has its own prefix, and most offer schedules,
standings, play-by-play, box scores and, for several, shot locations.

These wrappers match the European hockey functions in
[sportsdataverse-py](https://py.sportsdataverse.org/), so the same calls
work from Python.

## What you’ll learn

- Which prefix belongs to which league, and what each league’s feed
  covers.
- The conventions every European function shares: end-year seasons,
  request spacing, raw versus parsed output and classed errors.
- How to get a schedule, standings and a game’s play-by-play for each
  league.

## Before you start: these examples call live websites

Every function in this article calls the league’s own website or data
host. They are not run when this page is built, because some of these
hosts are slow or refuse requests from data-centre networks. Run them
from your own computer. If a call stalls, try again later or from
another network.

If you are new to `fastRhockey`, read [Getting started with
fastRhockey](https://fastRhockey.sportsdataverse.org/articles/introduction.md)
first.

## Which leagues are covered, and with which prefix?

| League | Country | Prefix | Data host | Highlights | Reference |
|----|----|----|----|----|----|
| SHL | Sweden | `shl_` | shl.se | Schedule, standings, play-by-play, box scores, player profiles and careers, team stats | [SHL](https://fastRhockey.sportsdataverse.org/reference/index.html#shl-sweden-) |
| HockeyAllsvenskan | Sweden | `ha_` | hockeyallsvenskan.se | Games, standings, play-by-play, skater, goalie and team stats, leaders | [HockeyAllsvenskan](https://fastRhockey.sportsdataverse.org/reference/index.html#hockeyallsvenskan-sweden-) |
| Liiga | Finland | `liiga_` | liiga.fi | Schedule, standings, game events, goals, lineups, shot maps, player and team stats | [Liiga](https://fastRhockey.sportsdataverse.org/reference/index.html#liiga-finland-) |
| DEL | Germany | `del_` | DEL scoring system feed | Schedule, standings, play-by-play, shots, shifts, faceoffs, box scores | [DEL](https://fastRhockey.sportsdataverse.org/reference/index.html#del-germany-) |
| ICE Hockey League | Austria and neighbours | `icehl_` | ICEHL scoring system feed | Schedule, standings, play-by-play, shots, faceoffs, box scores | [ICE Hockey League](https://fastRhockey.sportsdataverse.org/reference/index.html#ice-hockey-league-austria-) |
| EIHL | United Kingdom | `eihl_` | EIHL scoring system feed | Schedule, standings, play-by-play, shots, box scores | [EIHL](https://fastRhockey.sportsdataverse.org/reference/index.html#eihl-united-kingdom-) |
| Metal Ligaen | Denmark | `metal_` | Metal Ligaen scoring system feed | Schedule, standings, play-by-play, box scores | [Metal Ligaen](https://fastRhockey.sportsdataverse.org/reference/index.html#metal-ligaen-denmark-) |
| KHL | Russia and neighbours | `khl_` | khl.ru and its live-stats feed | Schedule, standings, play-by-play, shots, box scores, player careers, playoff bracket | [KHL](https://fastRhockey.sportsdataverse.org/reference/index.html#khl) |
| VHL | Russia | `vhl_` | vhlru.ru | Schedule, standings, play-by-play, goals, penalties, box scores, player stats | [VHL](https://fastRhockey.sportsdataverse.org/reference/index.html#vhl-russia-) |
| National League | Switzerland | `nl_` | nationalleague.ch | Schedule, standings, play-by-play, shots, lineups, box scores, team and player stats | [National League](https://fastRhockey.sportsdataverse.org/reference/index.html#national-league-switzerland-) |
| Swiss Ice Hockey Federation | Switzerland | `sihf_` | data.sihf.ch | Results, standings, play-by-play, lineups and many player and team stat tables (faceoffs by zone, power play, penalty kill, TOI) | [SIHF](https://fastRhockey.sportsdataverse.org/reference/index.html#swiss-ice-hockey-federation-sihf-) |
| Extraliga (ELH) | Czechia | `elh_` | Czech league data feeds | Scoreboard, game ids, play-by-play, events, shots | [Extraliga](https://fastRhockey.sportsdataverse.org/reference/index.html#extraliga-czechia-) |
| Ligue Magnus | France | `magnus_` | liguemagnus.com | Schedule, standings, play-by-play, lineups, shootouts, competition phases and rounds | [Ligue Magnus](https://fastRhockey.sportsdataverse.org/reference/index.html#ligue-magnus-france-) |

The number of exported functions per prefix, counted from the installed
package:

``` r

prefixes <- c("shl", "ha", "liiga", "del", "icehl", "eihl", "metal",
              "khl", "vhl", "nl", "sihf", "elh", "magnus")
exports <- getNamespaceExports("fastRhockey")

counts <- vapply(prefixes, function(p) sum(startsWith(exports, paste0(p, "_"))), integer(1))
data.frame(prefix = prefixes, functions = unname(counts))
#>    prefix functions
#> 1     shl        29
#> 2      ha        11
#> 3   liiga        23
#> 4     del        12
#> 5   icehl        12
#> 6    eihl         9
#> 7   metal         8
#> 8     khl        23
#> 9     vhl        14
#> 10     nl        17
#> 11   sihf        33
#> 12    elh         9
#> 13 magnus        11
sum(counts)
#> [1] 211
```

## What conventions do all the European functions share?

**Seasons are end years.** `season = 2026` is the 2025-26 season, as
everywhere in `fastRhockey`. Some feeds key seasons by their start year
internally; the wrappers translate for you.

**Requests are spaced and retried.** Every request waits at least one
second after the previous request to the same host. Answers of 429, 500,
502, 503 and 504 are retried, honouring a `Retry-After` header (capped
at 120 seconds). Loops over many games are slow on purpose; keep them
that way.

**Parsed or raw.** Most functions take `return_parsed = TRUE` (the
default) for a tidy data frame, or `FALSE` for the feed’s raw JSON as a
list, which is useful when a field you need isn’t parsed yet.

**Classed errors.** A failed request raises a condition you can catch by
class:

- `fastRhockey_no_data`: the provider has nothing for this request (for
  example, a game that hasn’t started);
- `fastRhockey_asset_fetch`: the answer is unknown, such as a network
  failure;
- `fastRhockey_bad_request`: the arguments are invalid.

All three inherit from `fastRhockey_error`.

``` r

library(fastRhockey)

standings <- tryCatch(
  khl_standings(season = 2026),
  fastRhockey_no_data = function(e) NULL,
  fastRhockey_error = function(e) {
    message("KHL request failed: ", conditionMessage(e))
    NULL
  }
)
```

## How do I get Swedish hockey data (SHL and HockeyAllsvenskan)?

The SHL keys each season and game type by an identifier;
[`shl_ssgt_uuid()`](https://fastRhockey.sportsdataverse.org/reference/shl_ssgt_uuid.md)
finds it for you, and
[`shl_standings()`](https://fastRhockey.sportsdataverse.org/reference/shl_standings.md)
takes it. Games are identified by a `game_uuid`.

``` r

shl_schedule(season = 2026)

ssgt <- shl_ssgt_uuid(2026)          # regular season; shl_ssgt_uuid(2025, "playoff") for playoffs
shl_standings(ssgt_uuid = ssgt)

shl_pbp(game_uuid = "bdhvuc5tex")
shl_player_box(game_uuid = "bdhvuc5tex")
```

HockeyAllsvenskan, Sweden’s second tier, has season-level games,
standings and stat leaders. A single game is addressed by its slug.

``` r

ha_games(season = 2026)
ha_standings(season = 2026)
ha_leaders(metric = "TP")
ha_game(slug = "20260918-aik-modo")
```

## How do I get Finnish Liiga data?

Liiga game functions take a `season` and a `game_id`. `tournament`
selects the regular season (`"runkosarja"`) or the playoffs
(`"playoffs"`).

``` r

liiga_schedule(season = 2026, tournament = "runkosarja")
liiga_standings(season = 2026)

liiga_game(season = 2026, game_id = 1)
liiga_game_goals(season = 2026, game_id = 1)
liiga_shotmap(season = 2026, game_id = 1)
```

## How do I get DEL, ICE Hockey League, EIHL and Metal Ligaen data?

These four leagues use the same scoring system, so their functions share
one shape: season-level calls take `season` and a competition
`league_id`, game-level calls take a `match_id`.

``` r

# Germany
del_schedule(season = 2027, league_id = 1)
del_standings(season = 2027, league_id = 1)
del_pbp(match_id = 4444)
del_shots(match_id = 4444)

# ICE Hockey League
icehl_schedule(season = 2027, league_id = 1)
icehl_pbp(match_id = 8161)

# United Kingdom (EIHL uses league_id = 3 for its league competition)
eihl_schedule(season = 2027, league_id = 3)
eihl_pbp(match_id = 4440)

# Denmark
metal_schedule(season = 2027, league_id = 1)
metal_pbp(match_id = 575)
```

All four share one data host, so the one-second spacing applies across
them together.

## How do I get KHL and VHL data?

``` r

khl_schedule(season = 2026)
khl_standings(season = 2026)
khl_pbp(game_id = 898236, season = 2026)
khl_shots(game_id = 898236, season = 2026)
khl_playoff_bracket(season = 2026)

vhl_schedule(season = 2026)
vhl_standings(season = 2026)
vhl_pbp(game_id = 899223, season = 2026)
```

## How do I get Swiss hockey data (National League and SIHF)?

The National League’s own site and the Swiss federation’s data service
overlap. The federation feed has the deeper stat tables; the league site
has shots and lineups.

``` r

nl_schedule(season = 2026)
nl_standings(season = 2026)
nl_pbp(game_id = 20261105000325)
nl_shots(game_id = 20261105000325)

sihf_results(season = 2026, league_id = 1)
sihf_standings(season = 2026, league_id = 1)
sihf_pbp(game_id = 20261105000325)
```

## How do I get Czech Extraliga data?

``` r

elh_scoreboard(date = "2026-01-15")
elh_game_ids(season = 2027)
elh_pbp(season = 2027, match_id = 2928321)
elh_shots(match_id = 2928321)
```

## How do I get French Ligue Magnus data?

Ligue Magnus organises a season into phases and rounds.
[`magnus_phases()`](https://fastRhockey.sportsdataverse.org/reference/magnus_phases.md)
and
[`magnus_rounds()`](https://fastRhockey.sportsdataverse.org/reference/magnus_rounds.md)
list them, and standings take a `phase_id`.

``` r

magnus_schedule(season = 2027, phase_id = 714, round = 2)
magnus_standings(phase_id = 714)
magnus_pbp(game_id = 77779)
```

## Can I plot European shot locations?

Yes, for the leagues that publish them:
[`del_shots()`](https://fastRhockey.sportsdataverse.org/reference/del_shots.md),
[`icehl_shots()`](https://fastRhockey.sportsdataverse.org/reference/icehl_shots.md),
[`eihl_shots()`](https://fastRhockey.sportsdataverse.org/reference/eihl_shots.md),
[`khl_shots()`](https://fastRhockey.sportsdataverse.org/reference/khl_shots.md),
[`nl_shots()`](https://fastRhockey.sportsdataverse.org/reference/nl_shots.md),
[`elh_shots()`](https://fastRhockey.sportsdataverse.org/reference/elh_shots.md)
and
[`liiga_shotmap()`](https://fastRhockey.sportsdataverse.org/reference/liiga_shotmap.md).
Each provider uses its own coordinate frame, so check the ranges of the
x and y columns and convert to feet from centre ice before drawing them
on a `sportyR` rink. The [rink plots and shot maps
article](https://fastRhockey.sportsdataverse.org/articles/nhl-pwhl-rink-plots-shot-maps.md)
shows the plotting side.

## Where to go next

- The [European Hockey section of the
  reference](https://fastRhockey.sportsdataverse.org/reference/index.html#european-hockey),
  with every function by league.
- [Rink plots and shot
  maps](https://fastRhockey.sportsdataverse.org/articles/nhl-pwhl-rink-plots-shot-maps.md):
  plot shot locations on a rink.
- [Corsi, Fenwick and PDO in
  R](https://fastRhockey.sportsdataverse.org/articles/hockey-advanced-metrics-corsi-fenwick.md):
  turn shot events into shot-share metrics.
- [PWHL and college
  hockey](https://fastRhockey.sportsdataverse.org/articles/pwhl-and-college-hockey.md):
  North American women’s and college hockey.
- [Getting started with
  fastRhockey](https://fastRhockey.sportsdataverse.org/articles/introduction.md),
  including a glossary of hockey analytics terms.
