# **fastRhockey 0.5.0**

* Major improvements to NHL Game PBP Data parsing with shifts in-line via ```nhl_game_pbp()``` function added to match [```hockeyR```](https://hockeyr.netlify.app)

# **fastRhockey 0.4.2**

* Fixing issues with ```phf_league_info()``` function for team name inconsistency.

# **fastRhockey 0.4.1**

* Minor logic addition for pbp parsing
* More under the hood changes to adapt to tidyselect new version guidelines
* ```load_phf_rosters()``` function added.
* ```load_nhl_rosters()``` function added.

# **fastRhockey 0.4.0**

* Updates logic to add Montreal Force to teams lists/parsing
* Under the hood changes to adapt to tidyselect new version guidelines

# **fastRhockey 0.3.1**

* Updates documentation per CRAN's request

# **fastRhockey 0.3.0**

* Add print method for all functions with a time stamp and description of the data
* Add `phf_team_logos` dataset to package for reference

# **fastRhockey 0.2.1**

* hotfix to `helper_phf_pbp_data()` penalty code
* add `try()` to function examples

# **fastRhockey 0.2.0**

* `espn_nhl_teams()` function added.

# **fastRhockey 0.1.0**

* Prepped for CRAN

# **fastRhockey 0.0.4**

### Loader functions for PHF

* `load_phf_pbp()` function added.
* `load_phf_team_box()` function added.
* `load_phf_player_box()` function added.
* `load_phf_schedule()` function added.
* `update_phf_db()` function added.

### Player and Team Stats, Leaderboards

* `phf_leaders()` function added.
* `phf_standings()` function added.
* `phf_player_stats()` function added.
* `phf_team_stats()` function added.
* `phf_team_roster()` function added.

# **fastRhockey 0.0.3**

### Function naming convention normalization

* `load_phf_game()` --> `phf_game_all()`
* `load_phf_pbp()` --> `phf_pbp()`
* `load_phf_boxscore()` --> `phf_team_box()`
* `load_phf_raw_data()` --> `phf_game_raw()`

# **fastRhockey 0.0.2**

* Added NHL functions from `powerplay` to `fastRhockey`

# **fastRhockey 0.0.1**

* Added a `NEWS.md` file to track changes to the package.
