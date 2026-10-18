# Captured ESPN college hockey bodies (F2 return-table verification)

- `{mch,wch}_*` except the four below: sdv-internal-refs `espn/inputs/sample_bodies/{mens,womens}-college-hockey/`
  (captured 2026-06-13). `teams_site` keeps 3 teams, `news` 2 articles, `mch_standings` 2 groups.
- `mch_team_schedule` (team 2005, trimmed to 2 played + 1 unplayed game), `mch_conferences`, `mch_injuries`,
  `mch_transactions`: live site.api.espn.com, 2026-10-18. Injuries, transactions and rosters were empty in every capture.
- `test-espn_ncaa_hockey.R` checks each wrapper's documented `@return` table against its output on these bodies.
