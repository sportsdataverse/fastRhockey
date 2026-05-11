# Update data/nhl_team_logos.rda for the 2024-25 Arizona -> Utah relocation.
#
# Mirrors the pattern already used for `MNS` (Minnesota North Stars), which
# the dataset retains with `division = "Relocated"` so historical joins still
# work. We:
#
#   * Mark `ARI` (Arizona Coyotes) as `division = "Relocated"`,
#     `conference = "Relocated"` — the franchise relocated to Utah for the
#     2024-25 season.
#   * Add a fresh `UTA` row for the Utah franchise (originally `Utah Hockey
#     Club` in 2024-25, rebranded `Utah Mammoth` for 2025-26). Best-effort
#     colors and logo URLs; please update with authoritative values when
#     available.
#
# Re-run this script any time the franchise list changes.

load("data/nhl_team_logos.rda")

# 1. Mark Arizona as relocated (parallel to the existing MNS entry).
ari_idx <- which(nhl_team_logos$team_abbr == "ARI")
if (length(ari_idx) == 1) {
    nhl_team_logos$division[ari_idx]   <- "Relocated"
    nhl_team_logos$conference[ari_idx] <- "Relocated"
}

# 2. Add UTA if it isn't already present.
if (!"UTA" %in% nhl_team_logos$team_abbr) {
    utah <- data.frame(
        full_team_name      = "Utah Mammoth",
        team_abbr           = "UTA",
        team_nick           = "mammoth",
        division            = "Central",
        conference          = "Western",
        team_logo_espn      = "https://a.espncdn.com/i/teamlogos/nhl/500/uta.png",
        team_color1         = "#71AFE5",
        team_color2         = "#000000",
        team_logo_alternate = NA_character_,
        team_color_alt1     = NA_character_,
        team_color_alt2     = NA_character_,
        stringsAsFactors    = FALSE
    )
    nhl_team_logos <- dplyr::bind_rows(nhl_team_logos, utah)
}

# Stable order: active teams first (sorted by abbr), then relocated rows
# (also sorted) at the bottom.
active    <- nhl_team_logos[nhl_team_logos$division != "Relocated", ]
relocated <- nhl_team_logos[nhl_team_logos$division == "Relocated", ]
active    <- active[order(active$team_abbr), ]
relocated <- relocated[order(relocated$team_abbr), ]
nhl_team_logos <- dplyr::bind_rows(active, relocated)

usethis::use_data(nhl_team_logos, overwrite = TRUE)
