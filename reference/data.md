# **Data in the package for reference**

A dataset containing the full team names, abbreviations, colors & logos
for all 32 NHL teams.

A dataset containing the full team names, abbreviations, colors & logos
for all PHF teams.

## Usage

``` r
nhl_team_logos

phf_team_logos
```

## Format

A data frame with 32 rows and 11 variables:

- `full_team_name` - full team name

- `team_abbr` - NHL.com team abbreviation

- `team_nick` - lowercase, no spaces team nickname

- `division` - current NHL division

- `conference` - current NHL conference

- `team_logo_espn` - primary team logo from ESPN.com

- `team_color1` - current primary team color

- `team_color2` - current secondary team color

- `team_logo_alternate` - alternate or throwback logo

- `team_color_alt1` - alternate logo primary color

- `team_color_alt2` - alternate logo secondary color

A data frame with 6 rows and 7 variables:

- `full_team_name` - Full team name

- `team_abbr` - PremierHockeyFederation.com team abbreviation

- `team_nick` - Team Nickname

- `team_location` - PHF team location

- `team_color1` - Current primary team color. Full disclosure, I just
  color picked from the logos

- `team_color2` - Current secondary team color. Full disclosure, I just
  color picked from the logos

- `team_logo` - Primary team logo from fastRhockey data repository
