# zzz.R -- the NHL expected-goals (xG) models, loaded on first use
#
# library(fastRhockey) makes no network call (CRAN policy): the models are
# fetched by helper_nhl_calculate_xg() the first time it runs, from the
# sportsdataverse-data `nhl_xg_models` release that sportsdataverse-py reads,
# so both libraries score with the same bytes.

# Package environment for xG model objects (avoids locked-binding issues)
.xg_env <- new.env(parent = emptyenv())
.xg_env$xg_model_5v5 <- NULL
.xg_env$xg_model_st <- NULL
.xg_env$xg_model_ps <- NULL
.xg_env$xg_feature_names_5v5 <- NULL
.xg_env$xg_feature_names_st <- NULL

.xg_base_url <- paste0(
    "https://github.com/sportsdataverse/sportsdataverse-data/",
    "releases/download/nhl_xg_models/"
)

#' The xG model files this version scores with, pinned by sha256
#'
#' The release asset digests of `nhl_xg_models` (the 2026-04-06 training,
#' sha256-identical to `fastRhockey-nhl-data/models/`). A cached file whose
#' hash differs -- a copy from another training -- is evicted and fetched
#' again, so a retrain cannot leave users scoring with the old boosters, and a
#' valid cached copy is used offline. A retrain ships by updating these pins.
#'
#' @keywords internal
#' @noRd
.xg_pins <- c(
    xg_model_5v5.json = "2e5017a4a8ef07708df758d5105d41f45ad83e85fe158358e707988a7d50c80c",
    xg_model_st.json = "8d6dfa95d9a8574b81eff4981e2bdbf69832238f8c383513de651712a3e3c678",
    xg_model_meta.json = "0188937cf169d597d9d378195f4056cd9ee3094e6c96f6244c8d86ae6ca9cfc6",
    feats_5v5.txt = "ecf01864d64aa825b76296fa79834f68c71bac4acc2b94f081976f413b838ee8",
    feats_st.txt = "c1e50527eb320021a222ee266f0584270ff083e13a142d659a43dce37f5c08d6"
)

#' Directory the xG models are cached in
#'
#' The user cache dir, except under `R CMD check` (which sets
#' `_R_CHECK_PACKAGE_NAME_`), where it is `tempdir()` so a check run leaves
#' nothing in the user's file space.
#'
#' @keywords internal
#' @noRd
.xg_cache_dir <- function() {
    if (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
        return(file.path(tempdir(), "fastRhockey"))
    }
    tools::R_user_dir("fastRhockey", which = "cache")
}

#' A pinned xG file: the cached copy if its sha256 matches, else downloaded
#'
#' @return The local path. Errors when the file cannot be fetched or the
#'   release no longer carries the pinned bytes.
#' @keywords internal
#' @noRd
.xg_file <- function(fname, cache_dir = .xg_cache_dir()) {
    dest <- file.path(cache_dir, fname)
    pin <- .xg_pins[[fname]]
    if (.xg_hash_is(dest, pin)) return(dest)
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    # Its own temp file per download, so two sessions loading at once cannot
    # hash or rename each other's partial file.
    tmp <- tempfile(paste0(fname, "-"), tmpdir = cache_dir)
    on.exit(unlink(tmp), add = TRUE)
    utils::download.file(paste0(.xg_base_url, fname), tmp,
                         mode = "wb", quiet = TRUE)
    if (!.xg_hash_is(tmp, pin)) {
        cli::cli_abort(c(
            "The {.val nhl_xg_models} release no longer carries the {.file {fname}} this fastRhockey version scores with.",
            "i" = "Update fastRhockey to use the retrained xG models."
        ))
    }
    # Replaces a stale cached copy too; a copy that still fails the pin is refused.
    unlink(dest, recursive = TRUE)
    if (!isTRUE(file.rename(tmp, dest))) file.copy(tmp, dest, overwrite = TRUE)
    if (!.xg_hash_is(dest, pin)) {
        cli::cli_abort("Could not write the xG model file {.file {dest}}.")
    }
    dest
}

#' Does `path` hash to `pin`? An unreadable or missing path is a miss.
#' @keywords internal
#' @noRd
.xg_hash_is <- function(path, pin) {
    file.exists(path) && !dir.exists(path) &&
        identical(tryCatch(cli::hash_file_sha256(path), error = function(e) NA), pin)
}

#' Load the xG models into `.xg_env`, once per session
#'
#' @return `TRUE` when loaded. Errors (with the reason) when a file cannot
#'   be fetched; the next call tries again.
#' @keywords internal
#' @noRd
.load_xg_models <- function() {
    if (!is.null(.xg_env$xg_model_5v5) && !is.null(.xg_env$xg_model_st)) {
        return(invisible(TRUE))
    }
    cache_dir <- .xg_cache_dir()
    # The pre-1.1.1 sidecar, superseded by xg_model_meta.json + feats_*.txt.
    unlink(file.path(cache_dir, "xg_model_meta.rds"))
    paths <- vapply(names(.xg_pins), .xg_file, character(1),
                    cache_dir = cache_dir)
    meta <- jsonlite::fromJSON(paths[["xg_model_meta.json"]])
    .xg_env$xg_feature_names_5v5 <- readLines(paths[["feats_5v5.txt"]], warn = FALSE)
    .xg_env$xg_feature_names_st <- readLines(paths[["feats_st.txt"]], warn = FALSE)
    .xg_env$xg_model_ps <- as.numeric(meta$xg_model_ps)
    .xg_env$xg_model_5v5 <- xgboost::xgb.load(paths[["xg_model_5v5.json"]])
    .xg_env$xg_model_st <- xgboost::xgb.load(paths[["xg_model_st.json"]])
    invisible(TRUE)
}

#' Force re-download of xG models
#'
#' @description Clears the cached xG model files, downloads them again from
#'   the sportsdataverse-data `nhl_xg_models` release and reloads them. The
#'   models normally load on first use and a stale cached copy is replaced
#'   automatically, so this is only needed to repair a cache by hand.
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
    cache_dir <- .xg_cache_dir()
    cached <- file.path(cache_dir, c(names(.xg_pins), "xg_model_meta.rds"))
    unlink(cached)
    if (any(file.exists(cached))) {
        stop("Could not delete the cached xG models in ", cache_dir, call. = FALSE)
    }
    message("Cleared cached xG models.")
    for (nm in c("xg_model_5v5", "xg_model_st")) assign(nm, NULL, envir = .xg_env)
    .load_xg_models()
    message("xG models reloaded successfully.")
    invisible(NULL)
}
