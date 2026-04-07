# zzz.R — Package hooks for fastRhockey
#
# .onLoad() downloads and caches the xG models from the fastRhockey-nhl-data
# GitHub repository, then loads them into a package environment. The models
# are used by helper_nhl_calculate_xg() and helper_nhl_prepare_xg_data().

# Package environment for xG model objects (avoids locked-binding issues)
.xg_env <- new.env(parent = emptyenv())
.xg_env$xg_model_5v5 <- NULL
.xg_env$xg_model_st <- NULL
.xg_env$xg_model_ps <- NULL
.xg_env$xg_feature_names_5v5 <- NULL
.xg_env$xg_feature_names_st <- NULL

# Base URL for downloading xG models from fastRhockey-nhl-data
.xg_base_url <- paste0(
    "https://raw.githubusercontent.com/sportsdataverse/",
    "fastRhockey-nhl-data/main/models/"
)

#' @importFrom utils packageName
.onLoad <- function(libname, pkgname) {
    # Attempt to load xG models silently. If xgboost is not installed or
    # model download fails, the package still loads — only helper_nhl_calculate_xg()
    # will error with a helpful message.
    tryCatch(
        .load_xg_models(pkgname),
        error = function(e) {
            packageStartupMessage(
                "Note: xG models could not be loaded. ",
                "helper_nhl_calculate_xg() will not be available. Reason: ",
                e$message
            )
        }
    )
}

#' Load xG models from cache or download from GitHub
#'
#' Models are cached in the user's R cache directory
#' (\code{tools::R_user_dir()}) so they are only downloaded once.
#'
#' @keywords internal
.load_xg_models <- function(pkgname) {
    if (!requireNamespace("xgboost", quietly = TRUE)) {
        return(invisible(NULL))
    }

    cache_dir <- tools::R_user_dir(pkgname, which = "cache")
    if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
    }

    # File names for the models and feature lists
    model_files <- list(
        xg_5v5_json = "xg_model_5v5.json",
        xg_st_json = "xg_model_st.json",
        xg_meta = "xg_model_meta.rds"
    )

    # Download any missing files
    for (fname in model_files) {
        local_path <- file.path(cache_dir, fname)
        if (!file.exists(local_path)) {
            url <- paste0(.xg_base_url, fname)
            tryCatch(
                utils::download.file(
                    url,
                    local_path,
                    mode = "wb",
                    quiet = TRUE
                ),
                error = function(e) {
                    if (file.exists(local_path)) {
                        file.remove(local_path)
                    }
                    stop(
                        "Failed to download ",
                        fname,
                        ": ",
                        e$message,
                        call. = FALSE
                    )
                }
            )
        }
    }

    # Load xgboost models from JSON
    path_5v5 <- file.path(cache_dir, model_files$xg_5v5_json)
    path_st <- file.path(cache_dir, model_files$xg_st_json)
    path_meta <- file.path(cache_dir, model_files$xg_meta)

    .xg_env$xg_model_5v5 <- xgboost::xgb.load(path_5v5)
    .xg_env$xg_model_st <- xgboost::xgb.load(path_st)

    # Load metadata (feature names and penalty shot constant)
    meta <- readRDS(path_meta)
    .xg_env$xg_feature_names_5v5 <- meta$xg_feature_names_5v5
    .xg_env$xg_feature_names_st <- meta$xg_feature_names_st
    .xg_env$xg_model_ps <- meta$xg_model_ps

    invisible(NULL)
}

#' Force re-download of xG models
#'
#' @description Clears the cached xG model files and re-downloads them from
#'   the fastRhockey-nhl-data repository. Use this after models have been
#'   retrained and pushed to GitHub.
#'
#' @return Invisible NULL. Models are reloaded into the package namespace.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   refresh_xg_models()
#' }
refresh_xg_models <- function() {
    if (!requireNamespace("xgboost", quietly = TRUE)) {
        stop(
            "Package 'xgboost' is required. Install it with: install.packages('xgboost')",
            call. = FALSE
        )
    }
    cache_dir <- tools::R_user_dir("fastRhockey", which = "cache")
    if (dir.exists(cache_dir)) {
        existing <- list.files(cache_dir, full.names = TRUE)
        file.remove(existing)
        message("Cleared cached xG models.")
    }
    .load_xg_models("fastRhockey")
    message("xG models reloaded successfully.")
    invisible(NULL)
}
