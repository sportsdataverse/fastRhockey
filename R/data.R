#' **Data in the package for reference**
#' @name data
NULL
#' @rdname data
#' @title **NHL Team logos & colors**
#' @description
#' A dataset containing the full team names, abbreviations, colors & logos
#' for all 32 active NHL teams plus historical/relocated franchises retained
#' for joining against older data. Filter on
#' `division != "Relocated"` to limit to active teams.
#' @keywords data
#' @format A data frame with 34 rows and 11 variables:
#'
#'   * `full_team_name` - full team name
#'   * `team_abbr` - NHL.com team abbreviation
#'   * `team_nick` - lowercase, no spaces team nickname
#'   * `division` - current NHL division, or `"Relocated"` for inactive
#'      franchises
#'   * `conference` - current NHL conference, or `"Relocated"` for inactive
#'      franchises
#'   * `team_logo_espn` - primary team logo from ESPN.com
#'   * `team_color1` - current primary team color
#'   * `team_color2` - current secondary team color
#'   * `team_logo_alternate` - alternate or throwback logo
#'   * `team_color_alt1` - alternate logo primary color
#'   * `team_color_alt2` - alternate logo secondary color
"nhl_team_logos"

#' @rdname data
#' @title **PHF Team logos & colors**
#' @description
#' A dataset containing the full team names, abbreviations, colors & logos
#' for all PHF teams.
#' @keywords data
#' @format A data frame with 6 rows and 7 variables:
#'
#'   * `full_team_name` - Full team name
#'   * `team_abbr` - PremierHockeyFederation.com team abbreviation
#'   * `team_nick` - Team Nickname
#'   * `team_location` - PHF team location
#'   * `team_color1` - Current primary team color. Full disclosure, I just color picked from the logos
#'   * `team_color2` - Current secondary team color. Full disclosure, I just color picked from the logos
#'   * `team_logo` - Primary team logo from fastRhockey data repository
"phf_team_logos"

#' @rdname data
#' @title **PWHL Team logos & colors**
#' @description
#' A dataset containing the full team names, abbreviations, colors & logos
#' for all PWHL teams. Carries 14 rows: the 6 founding franchises appear
#' twice each — once under their inaugural 2023-24 identity
#' (`PWHL Boston`, etc., matching the live `pwhl_teams()` payload) and once
#' under their 2024-25 rebranded identity (Boston Fleet, Minnesota Frost,
#' Montréal Victoire, New York Sirens, Ottawa Charge, Toronto Sceptres).
#' Plus the two 2025-26 expansion teams (Seattle Torrent and
#' Vancouver Goldeneyes). The shared `team_abbr` across pre/post rebrand
#' rows makes historical joins straightforward.
#' @keywords data
#' @format A data frame with 14 rows and 7 variables:
#'
#'   * `full_team_name` - Full team name (legacy or rebranded)
#'   * `team_abbr` - HockeyTech team code (BOS, MIN, MON, NY, OTT, TOR for
#'      the six founders; SEA, VAN for the expansion teams)
#'   * `team_nick` - Team nickname (or city, for the legacy
#'      `PWHL <city>` entries)
#'   * `team_location` - Team location / city
#'   * `team_color1` - Best-effort primary team color (hex)
#'   * `team_color2` - Best-effort secondary team color (hex)
#'   * `team_logo` - Primary team logo URL from the live PWHL CDN; `NA`
#'      for the two expansion teams until logos are catalogued
"pwhl_team_logos"
