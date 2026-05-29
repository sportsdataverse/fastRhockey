# **NHL Stats REST API Endpoint Overview**

Wrappers around the NHL Stats REST backend at
`api.nhle.com/stats/rest/{lang}/...`. These cover the documented
Cayenne-filterable endpoints for skater / goalie / team stats, draft
stats, franchise + player listings, glossary, country, config, leaders,
and milestones. Most pages use the consistent `{data: [...], total: N}`
shape and accept `cayenneExp`, `sort`, `limit`, `start` query
parameters.

## Details

### **Skater / goalie / team stats**

|  |  |  |
|----|----|----|
| Function | Endpoint | Purpose |
| [`nhl_stats_skaters()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_rest.html) | `skater/summary` | Skater season stats |
| [`nhl_stats_goalies()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_goalies.html) | `goalie/summary` | Goalie season stats |
| [`nhl_stats_teams()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_rest.html) | `team/summary` | Team season stats |
| [`nhl_stats_misc()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_misc.md) | `miscellaneousSkaterStats` | Misc skater stats + draft + season list |
| [`nhl_stats_skater_leaders()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_skater_leaders.md) | `leaders/skaters/{attribute}` | Skater leaderboards by attribute |
| [`nhl_stats_goalie_leaders()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_goalie_leaders.md) | `leaders/goalies/{attribute}` | Goalie leaderboards by attribute |
| [`nhl_stats_skater_milestones()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_skater_milestones.md) | `skater/milestones` | Skater milestone achievements |
| [`nhl_stats_goalie_milestones()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_goalie_milestones.md) | `goalie/milestones` | Goalie milestone achievements |

### **Franchise / player listings**

|  |  |  |
|----|----|----|
| Function | Endpoint | Purpose |
| [`nhl_stats_franchise()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_franchise.md) | `franchise` | Franchise listing |
| [`nhl_stats_players()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_players.md) | `players` | Players listing (requires `cayenne_exp`) |
| [`nhl_stats_team_listing()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_team_listing.md) | `team` | Top-level team listing |
| [`nhl_stats_game_listing()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_game_listing.md) | `game` | Top-level game listing |

### **Metadata helpers**

|  |  |  |
|----|----|----|
| Function | Endpoint | Purpose |
| [`nhl_stats_glossary()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_glossary.md) | `glossary` | Stat-definition glossary |
| [`nhl_stats_country()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_country.md) | `country` | Country lookup |
| [`nhl_stats_config()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_config.md) | `config` | Configuration payload |
| [`nhl_stats_ping()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_ping.md) | `ping` | Health check |
| [`nhl_stats_content_module()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_content_module.md) | (NHL CMS) | NHL.com content modules |

## Notes

- The `leaders/{skaters,goalies}/{attribute}` endpoint does **not**
  accept `start` / `limit` query parameters (returns 500 if you pass
  them).

- Valid `goalie` leader attributes are restricted to `savePctg`, `gaa`,
  and `shutouts`.
