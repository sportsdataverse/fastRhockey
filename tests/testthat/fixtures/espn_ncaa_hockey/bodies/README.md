# Captured ESPN college hockey bodies (F2 return-table verification)

- `{mch,wch}_*` except the four below: captured from ESPN's public API
  on 2026-06-13. `teams_site` keeps 3 teams, `news` 2 articles, `mch_standings` 2 groups.
- `mch_team_schedule` (team 2005, trimmed to 2 played + 1 unplayed game), `mch_conferences`, `mch_injuries`,
  `mch_transactions`: live site.api.espn.com, 2026-10-18. Injuries, transactions and the WCH roster were empty in every capture.
- `mch_team_roster`: team 104, live site.api.espn.com, 2026-11-24, trimmed to 2 athletes with `links` dropped. College rosters are flat (no position groups), and each athlete carries `injuries: []` and `contracts: []`.
- `test-espn_ncaa_hockey.R` checks each wrapper's documented `@return` table against its output on these bodies.
