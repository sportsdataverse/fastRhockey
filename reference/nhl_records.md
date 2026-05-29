# **NHL Records API Endpoint Overview**

Wrappers around the NHL Records backend at
`records.nhl.com/site/api/...`. These cover all 442 documented
record-keeping endpoints (franchises, players, draft, trophies, awards,
attendance, venues, officials, combine). All 25 wrappers route through
`.nhl_records_api()` in `helpers_nhl_records.R` which standardizes the
`cayenneExp` filtering, `sort` + `limit` + `start` pagination, and the
`{data: [...], total: N}` tabular response shape.

## Details

### **Franchise**

|  |  |  |
|----|----|----|
| Function | Endpoint family | Purpose |
| [`nhl_records_franchise()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records.html) | `franchise` | Franchise listing |
| [`nhl_records_franchise_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_franchise_detail.md) | `franchise-detail` | Franchise details |
| [`nhl_records_franchise_totals()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_franchise_totals.md) | `franchise-team-totals` | All-time team totals |
| [`nhl_records_franchise_team_totals()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_franchise_team_totals.md) | `franchise-team-totals` | Team totals (alt) |
| [`nhl_records_franchise_season_results()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_franchise_season_results.md) | `franchise-season-results` | Per-season results |
| [`nhl_records_franchise_playoff_appearances()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_franchise_playoff_appearances.md) | `franchise-playoff-appearances` | Playoff appearances |

### **Player**

|  |  |  |
|----|----|----|
| Function | Endpoint family | Purpose |
| [`nhl_records_player()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records.html) | `player` | Players listing |
| [`nhl_records_player_byteam()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_player_byteam.md) | `player` | Players filtered by team |
| [`nhl_records_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_player_stats.md) | `player-stats` | Player stats |
| [`nhl_records_skater_real_time_stats_career()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_skater_real_time_stats_career.md) | `skater-real-time-stats/career` | Career real-time skater stats |
| [`nhl_records_skater_real_time_stats_season()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_skater_real_time_stats_season.md) | `skater-real-time-stats/season` | Season real-time skater stats |
| [`nhl_records_goalie_career_stats()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_goalie_career_stats.md) | `goalie-career-stats` | Career goalie stats |
| [`nhl_records_goalie_season_stats()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_goalie_season_stats.md) | `goalie-season-stats` | Season goalie stats |
| [`nhl_records_goalie_shutout_streak()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_goalie_shutout_streak.md) | `goalie-shutout-streak` | Goalie shutout-streak records |

### **Draft**

|  |  |  |
|----|----|----|
| Function | Endpoint family | Purpose |
| [`nhl_records_draft()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records.html) | `draft` | Draft listing |
| [`nhl_records_draft_lottery_odds()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_draft_lottery_odds.md) | `draft-lottery-odds` | Lottery odds |
| [`nhl_records_draft_lottery_picks()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_draft_lottery_picks.md) | `draft-lottery-picks` | Lottery pick results |
| [`nhl_records_draft_prospect()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_draft_prospect.md) | `draft-prospect` | Draft-eligible prospects |
| [`nhl_records_combine()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_combine.md) | `draft-combine` | Draft combine measurements |

### **Honors and miscellany**

|  |  |  |
|----|----|----|
| Function | Endpoint family | Purpose |
| [`nhl_records_trophy()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_trophy.md) | `trophy` | Trophy listing |
| [`nhl_records_award_details()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_award_details.md) | `trophy/{id}` | Trophy / award winners |
| [`nhl_records_hof_players()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_hof_players.md) | `hof-player` | Hall of Fame |
| [`nhl_records_officials()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_officials.md) | `officials` | On-ice officials |
| [`nhl_records_attendance()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_attendance.md) | `attendance` | Attendance records |
| [`nhl_records_venue()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_venue.md) | `venue` | Arena / venue listing |
