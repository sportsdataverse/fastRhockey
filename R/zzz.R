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
#'   `.xg_files` order. Errors on a manifest that does not name every file or
#'   whose penalty-shot constant is not one probability.
#' @keywords internal
#' @noRd
.xg_read_manifest <- function(path) {
    m <- jsonlite::fromJSON(path, simplifyVector = TRUE)
    pins <- unlist(m$files)
    ps <- m$xg_model_ps
    ok <- is.numeric(ps) && length(ps) == 1L && is.finite(ps) && ps >= 0 && ps <= 1 &&
        setequal(names(pins), .xg_files) && all(grepl("^[0-9a-f]{64}$", pins))
    if (!isTRUE(ok)) {
        manifest <- .xg_manifest_name  # cli reads a leading "." in {} as a style, not a name
        expected <- .xg_files
        cli::cli_abort(c(
            "The {.val nhl_xg_models} release manifest {.file {manifest}} is not in the expected format.",
            "i" = "It must carry {.field xg_model_ps} (a probability) and a sha256 for each of {.file {expected}}."
        ))
    }
    m$files <- pins[.xg_files]
    m
}

#' The release manifest, fetched but NOT yet written to the cache
#'
#' @return `list(manifest, tmp)`: `tmp` is the fetched copy, committed to the
#'   cache by `.load_xg_models()` only after every file it names has verified;
#'   `NULL` when offline, where the cached manifest is used instead.
#' @keywords internal
#' @noRd
.xg_fetch_manifest <- function(cache_dir = .xg_cache_dir()) {
    dest <- file.path(cache_dir, .xg_manifest_name)
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile("xg_model_meta-", tmpdir = cache_dir)
    # A failed download.file() warns and then errors; the error is what we act on,
    # and its warning would only repeat it.
    fetched <- tryCatch({
        suppressWarnings(utils::download.file(paste0(.xg_base_url, .xg_manifest_name), tmp,
                                              mode = "wb", quiet = TRUE))
        .xg_read_manifest(tmp)
    }, error = function(e) e)
    if (!inherits(fetched, "error")) {
        return(list(manifest = fetched, tmp = tmp))
    }
    unlink(tmp)
    if (file.exists(dest) && !dir.exists(dest)) {
        # offline: the last manifest seen, with the files it named
        return(list(manifest = .xg_read_manifest(dest), tmp = NULL))
    }
    cli::cli_abort(c(
        "Could not fetch the {.val nhl_xg_models} release manifest.",
        "x" = conditionMessage(fetched)
    ))
}

#' Stage one xG file: `NULL` if the cached copy matches, else a verified download
#'
#' Downloads to a temp file in the cache dir and checks its sha256 against the
#' manifest, without touching the cached copy; `.load_xg_models()` renames the
#' staged files into place only when every one has verified.
#'
#' @return `NULL` (cache hit) or the staged temp path. Errors when the file
#'   cannot be fetched or does not match the manifest.
#' @keywords internal
#' @noRd
.xg_stage_file <- function(fname, pin, cache_dir = .xg_cache_dir()) {
    if (.xg_hash_is(file.path(cache_dir, fname), pin)) return(NULL)
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile(paste0(fname, "-"), tmpdir = cache_dir)
    ok <- FALSE
    on.exit(if (!ok) unlink(tmp), add = TRUE)
    utils::download.file(paste0(.xg_base_url, fname), tmp, mode = "wb", quiet = TRUE)
    if (!.xg_hash_is(tmp, pin)) {
        cli::cli_abort(c(
            "The downloaded {.file {fname}} does not match the {.val nhl_xg_models} release manifest.",
            "i" = "The release may be mid-update; try again shortly, or run {.fn refresh_xg_models}."
        ))
    }
    ok <- TRUE
    tmp
}

#' Move a staged temp file over `dest`
#'
#' A rename replaces `dest` in one step (it never deletes `dest` first); when
#' the rename fails, a copy is tried, and the result must hash to `pin` or the
#' load stops with an error rather than continue from an incomplete cache.
#' @keywords internal
#' @noRd
.xg_commit <- function(tmp, dest, pin) {
    if (!isTRUE(file.rename(tmp, dest))) {
        suppressWarnings(file.copy(tmp, dest, overwrite = TRUE))
        unlink(tmp)
    }
    if (!.xg_hash_is(dest, pin)) {
        cli::cli_abort("Could not write the xG model file {.file {dest}}.")
    }
    invisible(dest)
}

#' Commit staged files as one set, rolling back on any failure
#'
#' `staged` maps cache file names to verified temp files, in commit order (the
#' manifest last); `pins` gives each one's sha256. Every file about to be
#' replaced is first moved to its own uniquely named `<name>.bak-*` (an earlier
#' operation's backup is never overwritten); if any commit fails, every file
#' already written is put back from its backup (or removed, if it is new), so the
#' cache keeps the previous complete set. On success the backups are deleted.
#' @keywords internal
#' @noRd
.xg_commit_set <- function(staged, pins, cache_dir) {
    backups <- character()
    written <- character()
    res <- tryCatch({
        for (f in names(staged)) {
            dest <- file.path(cache_dir, f)
            if (file.exists(dest)) {
                bak <- tempfile(paste0(f, ".bak-"), tmpdir = cache_dir)
                if (!isTRUE(file.rename(dest, bak))) {
                    cli::cli_abort("Could not set aside the cached {.file {dest}}.")
                }
                backups[[f]] <- bak
            }
            written <- c(written, f)
            .xg_commit(staged[[f]], dest, pins[[f]])
        }
        NULL
    }, error = function(e) e)
    if (is.null(res)) {
        unlink(backups)
        return(invisible(TRUE))
    }
    for (f in written) {
        dest <- file.path(cache_dir, f)
        unlink(dest)
        if (!is.na(backups[f])) file.rename(backups[[f]], dest)
    }
    lost <- names(backups)[!file.exists(file.path(cache_dir, names(backups)))]
    if (length(lost)) {
        cli::cli_abort(c(
            "Could not update the xG models, and could not restore {.file {lost}}.",
            "i" = "The previous copies are kept as {.file .bak-*} files in {.path {cache_dir}}.",
            "x" = conditionMessage(res)
        ))
    }
    stop(res)
}

#' Build a booster from verified bytes (a seam the offline tests replace)
#' @keywords internal
#' @noRd
.xg_load_booster <- function(bytes) xgboost::xgb.load.raw(bytes)

#' Does `path` hash to `pin`? An unreadable or missing path is a miss.
#' @keywords internal
#' @noRd
.xg_hash_is <- function(path, pin) {
    file.exists(path) && !dir.exists(path) &&
        identical(tryCatch(cli::hash_file_sha256(path), error = function(e) NA), pin)
}

#' The bytes of a cached xG file, checked against the manifest as they are read
#'
#' The models are built from these exact bytes, so another session replacing
#' the file between verification and loading surfaces as an error, never as a
#' booster from one release loaded beside another release's constant.
#' @keywords internal
#' @noRd
.xg_verified_bytes <- function(fname, pin, cache_dir) {
    path <- file.path(cache_dir, fname)
    bytes <- readBin(path, "raw", n = file.size(path))
    if (!identical(cli::hash_raw_sha256(bytes), pin)) {
        cli::cli_abort(c(
            "The cached {.file {fname}} changed while it was being loaded.",
            "i" = "Another session may be updating the xG models; try again."
        ))
    }
    bytes
}

#' Load the xG models into `.xg_env`, once per session
#'
#' Fetches the manifest, stages every file it names that the cache lacks, and
#' commits only a complete, verified set -- the files first, the manifest last
#' -- so a refresh that fails part-way leaves the previous set intact for
#' offline use.
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
    fetched <- .xg_fetch_manifest(cache_dir)
    manifest <- fetched$manifest
    staged <- list()
    on.exit(unlink(c(unlist(staged), fetched$tmp)), add = TRUE)
    for (f in .xg_files) {
        tmp <- .xg_stage_file(f, manifest$files[[f]], cache_dir)
        if (!is.null(tmp)) staged[[f]] <- tmp
    }
    pins <- as.list(manifest$files)
    if (!is.null(fetched$tmp)) {
        staged[[.xg_manifest_name]] <- fetched$tmp  # committed last
        pins[[.xg_manifest_name]] <- cli::hash_file_sha256(fetched$tmp)
    }
    if (length(staged)) .xg_commit_set(staged, pins, cache_dir)
    bytes <- lapply(stats::setNames(.xg_files, .xg_files), function(f) {
        .xg_verified_bytes(f, manifest$files[[f]], cache_dir)
    })
    feats <- function(b) strsplit(gsub("\r", "", rawToChar(b)), "\n", fixed = TRUE)[[1]]
    .xg_env$xg_feature_names_5v5 <- feats(bytes[["feats_5v5.txt"]])
    .xg_env$xg_feature_names_st <- feats(bytes[["feats_st.txt"]])
    .xg_env$xg_model_ps <- as.numeric(manifest$xg_model_ps)
    .xg_env$xg_model_version <- manifest$version
    .xg_env$xg_model_5v5 <- .xg_load_booster(bytes[["xg_model_5v5.json"]])
    .xg_env$xg_model_st <- .xg_load_booster(bytes[["xg_model_st.json"]])
    invisible(TRUE)
}

#' Force re-download of xG models
#'
#' @description Clears the cached xG model files, downloads them again from
#'   the sportsdataverse-data `nhl_xg_models` release and reloads them. The
#'   models normally load on first use and a stale cached copy is replaced
#'   automatically, so this is only needed to repair a cache by hand. If the
#'   download fails, the previous cache is restored.
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
    names <- c(.xg_files, .xg_manifest_name)
    cached <- file.path(cache_dir, names)
    unlink(file.path(cache_dir, "xg_model_meta.rds"))
    # Move the current set aside rather than delete it: a refresh that cannot
    # download (offline, release mid-update) puts it back instead of leaving an
    # empty cache. Each refresh gets its own backup dir, so one kept by an
    # earlier failed restore is never removed here.
    backup <- tempfile(".refresh-backup-", tmpdir = cache_dir)
    dir.create(backup, recursive = TRUE, showWarnings = FALSE)
    present <- file.exists(cached)
    moved <- file.rename(cached[present], file.path(backup, names[present]))
    if (!all(moved)) {
        back <- file.rename(file.path(backup, names[present][moved]), cached[present][moved])
        if (!all(back)) {
            stop("Could not clear the cached xG models in ", cache_dir, ", and could not put ",
                 "every file back; the rest are kept in ", backup, ".", call. = FALSE)
        }
        unlink(backup, recursive = TRUE)
        stop("Could not clear the cached xG models in ", cache_dir, call. = FALSE)
    }
    message("Cleared cached xG models.")
    for (nm in c("xg_model_5v5", "xg_model_st")) assign(nm, NULL, envir = .xg_env)
    tryCatch(.load_xg_models(), error = function(e) {
        unlink(cached)
        restored <- file.rename(file.path(backup, names[present]), cached[present])
        if (!all(restored)) {
            stop("Could not reload the xG models, and could not restore the previous cache; ",
                 "its files are kept in ", backup, ". ", conditionMessage(e), call. = FALSE)
        }
        unlink(backup, recursive = TRUE)
        stop("Could not reload the xG models; the previous cache was restored. ",
             conditionMessage(e), call. = FALSE)
    })
    unlink(backup, recursive = TRUE)
    message("xG models reloaded successfully.")
    invisible(NULL)
}
