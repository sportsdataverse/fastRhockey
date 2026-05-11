# Generate data/pwhl_team_logos.rda from data-raw/pwhl_teams.csv.
#
# The CSV carries 14 rows:
#   * 6 founding teams in their 2023-24 pre-rebrand identity
#     ("PWHL Boston" etc.) — matches what the live HockeyTech API still
#     returns from `pwhl_teams()`.
#   * 6 founding teams in their 2024-25 rebrand identity (Boston Fleet,
#     Minnesota Frost, Montréal Victoire, New York Sirens, Ottawa Charge,
#     Toronto Sceptres).
#   * 2 expansion teams for the 2025-26 season (Seattle Torrent, Vancouver
#     Goldeneyes). team_logo is intentionally NA — no public CDN URL is
#     known to host their logos yet.
#
# Colors are best-effort approximations sourced from public team branding;
# please correct in the CSV if you have authoritative values.

pwhl_team_logos <- utils::read.csv(
    "data-raw/pwhl_teams.csv",
    stringsAsFactors = FALSE,
    na.strings = c("", "NA"),
    encoding = "UTF-8"
)

usethis::use_data(pwhl_team_logos, overwrite = TRUE)
