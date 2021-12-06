#' **Data in the package for reference**
#' @name data
NULL
#' @rdname data
#' @title **NHL Team logos & colors**
#' @description
#' A dataset containing the full team names, abbreviations, colors & logos
#' for all 32 NHL teams.
#' @keywords data
#'
#' @return A data frame with 32 rows and 11 variables:
#' \describe{
#'   \item{full_team_name}{full team name}
#'   \item{team_abbr}{NHL.com team abbreviation}
#'   \item{team_nick}{lowercase, no spaces team nickname}
#'   \item{division}{current NHL division}
#'   \item{conference}{current NHL conference}
#'   \item{team_logo_espn}{primary team logo from ESPN.com}
#'   \item{team_color1}{current primary team color}
#'   \item{team_color2}{current secondary team color}
#'   \item{team_logo_alternate}{alternate or throwback logo}
#'   \item{team_color_alt1}{alternate logo primary color}
#'   \item{team_color_alt2}{alternate logo secondary color}
#' }
"nhl_team_logos"
