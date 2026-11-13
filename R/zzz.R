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
.xg_env$xg_model_version <- NULL
.xg_env$xg_feature_names_5v5 <- NULL
.xg_env$xg_feature_names_st <- NULL

.xg_base_url <- paste0(
    "https://github.com/sportsdataverse/sportsdataverse-data/",
    "releases/download/nhl_xg_models/"
)

#' The xG model files the release manifest must name
#'
#' The release's `xg_model_meta.json` is its manifest: the penalty-shot
#' constant, a `version`, and the sha256 of each of these files. It is written
#' by `fastRhockey-nhl-data`'s publisher (`nhl_data_build.xg_publish`) from the
#' promoted models, so a retrain ships by publishing -- no fastRhockey release
#' is needed, and a cached file from another training is replaced.
#'
#' @keywords internal
#' @noRd
.xg_files <- c("xg_model_5v5.json", "xg_model_st.json", "feats_5v5.txt", "feats_st.txt")
.xg_manifest_name <- "xg_model_meta.json"

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

#' Read and check a release manifest
#'
#' @return The manifest list with `files` as a named sha256 vector in
#'   `.xg_files` order. Errors on a manifest that does not name every file.
#' @keywords internal
#' @noRd
.xg_read_manifest <- function(path) {
    m <- jsonlite::fromJSON(path, simplifyVector = TRUE)
    pins <- unlist(m$files)
    ok <- !is.null(m$xg_model_ps) && setequal(names(pins), .xg_files) &&
        all(grepl("^[0-9a-f]{64}$", pins))
    if (!isTRUE(ok)) {
        manifest <- .xg_manifest_name  # cli reads a leading "." in {} as a style, not a name
        expected <- .xg_files
        cli::cli_abort(c(
            "The {.val nhl_xg_models} release manifest {.file {manifest}} is not in the expected format.",
            "i" = "It must carry {.field xg_model_ps} and a sha256 for each of {.file {expected}}."
        ))
    }
    m$files <- pins[.xg_files]
    m
}

#' The release manifest: fetched once per load, the cached copy when offline
#'
#' @return The checked manifest (see `.xg_read_manifest()`).
#' @keywords internal
#' @noRd
.xg_manifest <- function(cache_dir = .xg_cache_dir()) {
    dest <- file.path(cache_dir, .xg_manifest_name)
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile("xg_model_meta-", tmpdir = cache_dir)
    on.exit(unlink(tmp), add = TRUE)
    # A failed download.file() warns and then errors; the error is what we act on,
    # and its warning would only repeat it.
    fetched <- tryCatch({
        suppressWarnings(utils::download.file(paste0(.xg_base_url, .xg_manifest_name), tmp,
                                              mode = "wb", quiet = TRUE))
        .xg_read_manifest(tmp)
    }, error = function(e) e)
    if (!inherits(fetched, "error")) {
        unlink(dest, recursive = TRUE)
        if (!isTRUE(file.rename(tmp, dest))) file.copy(tmp, dest, overwrite = TRUE)
        return(fetched)
    }
    if (file.exists(dest) && !dir.exists(dest)) {
        return(.xg_read_manifest(dest))  # offline: the last manifest seen, with the files it named
    }
    cli::cli_abort(c(
        "Could not fetch the {.val nhl_xg_models} release manifest.",
        "x" = conditionMessage(fetched)
    ))
}

#' An xG file: the cached copy if it matches the manifest, else downloaded
#'
#' @return The local path. Errors when the file cannot be fetched or the
#'   downloaded bytes do not match the manifest.
#' @keywords internal
#' @noRd
.xg_file <- function(fname, pin, cache_dir = .xg_cache_dir()) {
    dest <- file.path(cache_dir, fname)
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
            "The downloaded {.file {fname}} does not match the {.val nhl_xg_models} release manifest.",
            "i" = "The release may be mid-update; try again shortly, or run {.fn refresh_xg_models}."
        ))
    }
    # Replaces a stale cached copy too; a copy that still fails the hash is refused.
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
    manifest <- .xg_manifest(cache_dir)
    paths <- vapply(.xg_files, function(f) .xg_file(f, manifest$files[[f]], cache_dir),
                    character(1))
    .xg_env$xg_feature_names_5v5 <- readLines(paths[["feats_5v5.txt"]], warn = FALSE)
    .xg_env$xg_feature_names_st <- readLines(paths[["feats_st.txt"]], warn = FALSE)
    .xg_env$xg_model_ps <- as.numeric(manifest$xg_model_ps)
    .xg_env$xg_model_version <- manifest$version
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
    cached <- file.path(cache_dir, c(.xg_files, .xg_manifest_name, "xg_model_meta.rds"))
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
