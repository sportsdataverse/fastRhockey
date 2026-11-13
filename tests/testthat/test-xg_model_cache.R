# The xG models load on first use and are checked against the release MANIFEST
# (xg_model_meta.json: penalty-shot constant, version, sha256 per file), not hashes
# pinned in this package, so a retrain ships by publishing (fastRhockey#96 follow-up).
# Offline: download.file is mocked; the manifest fixture is the real published one.

manifest_fixture <- test_path("fixtures", "nhl", "xg_model_meta.json")

# A fake release: `files` maps asset name -> local source path; NULL = no network.
mock_release <- function(env) {
    function(url, destfile, ...) {
        env$calls <- c(env$calls, url)
        if (is.null(env$files)) stop("cannot open URL '", url, "'")
        src <- env$files[[basename(url)]]
        if (is.null(src)) stop("HTTP status was '404 Not Found'")
        file.copy(src, destfile, overwrite = TRUE)
        0L
    }
}

# A release in `dir`: four asset files (one line of text each) and a manifest naming them.
fake_assets <- function(dir, tag = "a", ps = 0.31) {
    paths <- file.path(dir, .xg_files)
    # exact bytes (writeLines would write CRLF on Windows)
    for (p in paths) writeBin(charToRaw(paste0(tag, " ", basename(p), "
")), p)
    m <- list(xg_model_ps = ps, version = tag,
              files = as.list(stats::setNames(vapply(paths, cli::hash_file_sha256, ""), .xg_files)))
    mp <- file.path(dir, .xg_manifest_name)
    jsonlite::write_json(m, mp, auto_unbox = TRUE, digits = NA)
    c(stats::setNames(as.list(paths), .xg_files), stats::setNames(list(mp), .xg_manifest_name))
}

# Every file of `rel` copied into `cache` (a complete cached set).
cache_set <- function(rel, cache) {
    for (p in rel) file.copy(p, cache, overwrite = TRUE)
}

# .load_xg_models() without xgboost (a Suggests): the booster loader returns its bytes.
local_fake_xgboost <- function(env = parent.frame()) {
    local_mocked_bindings(.xg_load_booster = function(bytes) bytes, .env = env)
    old <- as.list(.xg_env)
    withr::defer(for (n in names(old)) assign(n, old[[n]], envir = .xg_env), envir = env)
    for (n in c("xg_model_5v5", "xg_model_st")) assign(n, NULL, envir = .xg_env)
}

# refresh_xg_models() checks for xgboost (a Suggests) before it touches the cache; the
# coverage job does not install it, so these tests report it present (the loader is faked).
local_xgboost_present <- function(env = parent.frame()) {
    real <- requireNamespace
    local_mocked_bindings(
        requireNamespace = function(package, ...) if (identical(package, "xgboost")) TRUE else real(package, ...),
        .package = "base", .env = env
    )
}

test_that("loading the package does not fetch the xG models", {
    expect_false(exists(".onLoad", envir = asNamespace("fastRhockey"), inherits = FALSE))
})

test_that("the published manifest names every file with a sha256 and the PS constant", {
    m <- .xg_read_manifest(manifest_fixture)
    expect_identical(names(m$files), .xg_files)
    expect_true(all(grepl("^[0-9a-f]{64}$", m$files)))
    expect_true(is.numeric(m$xg_model_ps) && m$xg_model_ps > 0 && m$xg_model_ps < 1)
})

test_that("a manifest without file hashes (the pre-manifest release) is refused", {
    old <- withr::local_tempfile(fileext = ".json")
    writeLines('{"xg_model_ps": 0.3202197}', old)
    expect_error(.xg_read_manifest(old), "not in the expected format")
})

test_that("a penalty-shot constant that is not one probability is refused", {
    dir <- withr::local_tempdir()
    for (ps in list(2, -0.1, "0.3", c(0.3, 0.4))) {
        rel <- fake_assets(dir, ps = ps)
        expect_error(.xg_read_manifest(rel[[.xg_manifest_name]]), "not in the expected format")
    }
})

test_that("a cached file matching the manifest is not downloaded again", {
    src <- withr::local_tempdir(); cache <- withr::local_tempdir()
    rel <- fake_assets(src)
    file.copy(rel[["feats_st.txt"]], cache)
    env <- new.env(); env$files <- rel
    local_mocked_bindings(download.file = mock_release(env), .package = "utils")
    expect_null(.xg_stage_file("feats_st.txt", cli::hash_file_sha256(rel[["feats_st.txt"]]), cache))
    expect_null(env$calls)
})

test_that("a retrain reaches the cache: the new manifest replaces every stale file", {
    old <- withr::local_tempdir(); new <- withr::local_tempdir(); cache <- withr::local_tempdir()
    cache_set(fake_assets(old, "old"), cache)
    rel <- fake_assets(new, "new")
    env <- new.env(); env$files <- rel
    local_mocked_bindings(download.file = mock_release(env), .package = "utils")
    local_mocked_bindings(.xg_cache_dir = function() cache)
    local_fake_xgboost()
    expect_true(.load_xg_models())
    expect_identical(.xg_env$xg_model_version, "new")
    expect_identical(.xg_env$xg_feature_names_st, "new feats_st.txt")
    expect_identical(rawToChar(.xg_env$xg_model_5v5), "new xg_model_5v5.json\n")
    for (f in c(.xg_files, .xg_manifest_name)) {
        expect_identical(cli::hash_file_sha256(file.path(cache, f)), cli::hash_file_sha256(rel[[f]]))
    }
    expect_length(env$calls, 1L + length(.xg_files))
    expect_match(env$calls[[1]], "releases/download/nhl_xg_models/xg_model_meta.json$")
    expect_setequal(list.files(cache), c(.xg_files, .xg_manifest_name))  # no temp files left
})

test_that("a refresh that fails part-way leaves the previous set usable offline", {
    a <- withr::local_tempdir(); b <- withr::local_tempdir(); cache <- withr::local_tempdir()
    cache_set(fake_assets(a, "A"), cache)
    before <- vapply(file.path(cache, c(.xg_files, .xg_manifest_name)), cli::hash_file_sha256, "")
    rel <- fake_assets(b, "B")
    writeLines("truncated", rel[["xg_model_st.json"]])  # B's second booster fails its hash
    env <- new.env(); env$files <- rel
    local_mocked_bindings(download.file = mock_release(env), .package = "utils")
    local_mocked_bindings(.xg_cache_dir = function() cache)
    local_fake_xgboost()
    expect_error(.load_xg_models(), "does not match")
    after <- vapply(file.path(cache, c(.xg_files, .xg_manifest_name)), cli::hash_file_sha256, "")
    expect_identical(unname(after), unname(before))  # nothing committed
    expect_setequal(list.files(cache), c(.xg_files, .xg_manifest_name))
    env$files <- NULL  # now offline: set A still loads
    expect_true(.load_xg_models())
    expect_identical(.xg_env$xg_model_version, "A")
})

test_that("offline, the cached manifest is used; with none, the failure is reported", {
    src <- withr::local_tempdir(); cache <- withr::local_tempdir()
    rel <- fake_assets(src, "cached")
    env <- new.env(); env$files <- NULL
    local_mocked_bindings(download.file = mock_release(env), .package = "utils")
    expect_error(.xg_fetch_manifest(cache), "Could not fetch")
    file.copy(rel[[.xg_manifest_name]], cache)
    got <- .xg_fetch_manifest(cache)
    expect_identical(got$manifest$version, "cached")
    expect_null(got$tmp)
})

test_that("a file that changes after verification is refused, not loaded", {
    src <- withr::local_tempdir(); cache <- withr::local_tempdir()
    rel <- fake_assets(src)
    cache_set(rel, cache)
    pin <- cli::hash_file_sha256(rel[["xg_model_st.json"]])
    expect_identical(rawToChar(.xg_verified_bytes("xg_model_st.json", pin, cache)), "a xg_model_st.json\n")
    writeLines("another release", file.path(cache, "xg_model_st.json"))
    expect_error(.xg_verified_bytes("xg_model_st.json", pin, cache), "changed while it was being loaded")
})

test_that("bytes that do not match the manifest are refused, not cached", {
    src <- withr::local_tempdir(); cache <- withr::local_tempdir()
    rel <- fake_assets(src)
    env <- new.env(); env$files <- rel
    local_mocked_bindings(download.file = mock_release(env), .package = "utils")
    expect_error(.xg_stage_file("xg_model_st.json", strrep("0", 64), cache), "does not match")
    expect_length(list.files(cache), 0L)
})

test_that("an unreadable cache entry is a miss, not an error", {
    src <- withr::local_tempdir(); cache <- withr::local_tempdir()
    rel <- fake_assets(src)
    dir.create(file.path(cache, "feats_5v5.txt"))
    env <- new.env(); env$files <- rel
    local_mocked_bindings(download.file = mock_release(env), .package = "utils")
    pin <- cli::hash_file_sha256(rel[["feats_5v5.txt"]])
    tmp <- .xg_stage_file("feats_5v5.txt", pin, cache)
    expect_length(env$calls, 1L)
    expect_identical(cli::hash_file_sha256(tmp), pin)
})

test_that("a commit that cannot write the file stops the load", {
    src <- withr::local_tempdir(); cache <- withr::local_tempdir()
    rel <- fake_assets(src)
    tmp <- file.path(cache, "staged")
    file.copy(rel[["feats_st.txt"]], tmp)
    local_mocked_bindings(file.rename = function(...) FALSE, file.copy = function(...) FALSE, .package = "base")
    expect_error(
        .xg_commit(tmp, file.path(cache, "feats_st.txt"), cli::hash_file_sha256(rel[["feats_st.txt"]])),
        "Could not write"
    )
})

test_that("refresh_xg_models() restores the previous cache when it cannot download", {
    local_xgboost_present()
    src <- withr::local_tempdir(); cache <- withr::local_tempdir()
    rel <- fake_assets(src, "kept")
    cache_set(rel, cache)
    before <- vapply(file.path(cache, c(.xg_files, .xg_manifest_name)), cli::hash_file_sha256, "")
    env <- new.env(); env$files <- NULL  # offline
    local_mocked_bindings(download.file = mock_release(env), .package = "utils")
    local_mocked_bindings(.xg_cache_dir = function() cache)
    local_fake_xgboost()
    expect_error(suppressMessages(refresh_xg_models()), "previous cache was restored")
    after <- vapply(file.path(cache, c(.xg_files, .xg_manifest_name)), cli::hash_file_sha256, "")
    expect_identical(unname(after), unname(before))
    expect_length(list.files(cache, pattern = "^\\.refresh-backup", all.files = TRUE), 0L)
})

test_that("a commit that fails part-way rolls back to the previous complete set", {
    a <- withr::local_tempdir(); b <- withr::local_tempdir(); cache <- withr::local_tempdir()
    cache_set(fake_assets(a, "A"), cache)
    before <- vapply(file.path(cache, c(.xg_files, .xg_manifest_name)), cli::hash_file_sha256, "")
    rel <- fake_assets(b, "B")
    env <- new.env(); env$files <- rel
    local_mocked_bindings(download.file = mock_release(env), .package = "utils")
    local_mocked_bindings(.xg_cache_dir = function() cache)
    local_fake_xgboost()
    real_commit <- .xg_commit
    n <- 0L
    local_mocked_bindings(.xg_commit = function(tmp, dest, pin) {
        n <<- n + 1L
        if (n == 3L) cli::cli_abort("Could not write the xG model file {.file {dest}}.")
        real_commit(tmp, dest, pin)
    })
    expect_error(.load_xg_models(), "Could not write")
    after <- vapply(file.path(cache, c(.xg_files, .xg_manifest_name)), cli::hash_file_sha256, "")
    expect_identical(unname(after), unname(before))
    expect_setequal(list.files(cache), c(.xg_files, .xg_manifest_name))  # no .bak or temp left
    env$files <- NULL  # offline: set A still loads
    local_mocked_bindings(.xg_commit = real_commit)
    expect_true(.load_xg_models())
    expect_identical(.xg_env$xg_model_version, "A")
})

test_that("refresh_xg_models() keeps its backup when it cannot restore it", {
    local_xgboost_present()
    src <- withr::local_tempdir(); cache <- withr::local_tempdir()
    cache_set(fake_assets(src, "kept"), cache)
    env <- new.env(); env$files <- NULL  # offline
    local_mocked_bindings(download.file = mock_release(env), .package = "utils")
    local_mocked_bindings(.xg_cache_dir = function() cache)
    local_fake_xgboost()
    real_rename <- file.rename
    local_mocked_bindings(file.rename = function(from, to) {
        if (any(grepl(".refresh-backup", from, fixed = TRUE))) return(rep(FALSE, length(from)))
        real_rename(from, to)
    }, .package = "base")
    expect_error(suppressMessages(refresh_xg_models()), "files are kept in")
    kept <- list.files(cache, pattern = "^\\.refresh-backup", all.files = TRUE, full.names = TRUE)
    expect_length(kept, 1L)
    expect_setequal(list.files(kept), c(.xg_files, .xg_manifest_name))
    # a second failed refresh must not delete the backup the first one kept
    expect_error(suppressMessages(refresh_xg_models()))
    expect_true(dir.exists(kept))
    expect_setequal(list.files(kept), c(.xg_files, .xg_manifest_name))
})

test_that("refresh_xg_models() keeps the files it cannot move back when clearing fails", {
    local_xgboost_present()
    src <- withr::local_tempdir(); cache <- withr::local_tempdir()
    cache_set(fake_assets(src, "kept"), cache)
    local_mocked_bindings(.xg_cache_dir = function() cache)
    real_rename <- file.rename
    local_mocked_bindings(file.rename = function(from, to) {
        into_backup <- grepl(".refresh-backup", to, fixed = TRUE)
        out_of_backup <- grepl(".refresh-backup", from, fixed = TRUE)
        if (any(into_backup)) {  # the move aside fails for the manifest only
            ok <- !grepl("xg_model_meta.json", from, fixed = TRUE)
            real_rename(from[ok], to[ok])
            return(ok)
        }
        if (any(out_of_backup)) return(rep(FALSE, length(from)))  # and moving back fails too
        real_rename(from, to)
    }, .package = "base")
    expect_error(suppressMessages(refresh_xg_models()), "the rest are kept in")
    kept <- list.files(cache, pattern = "^\\.refresh-backup", all.files = TRUE, full.names = TRUE)
    expect_length(kept, 1L)
    expect_setequal(list.files(kept), .xg_files)  # nothing deleted
})

test_that("a model commit never overwrites an earlier operation's backup", {
    a <- withr::local_tempdir(); b <- withr::local_tempdir(); cache <- withr::local_tempdir()
    cache_set(fake_assets(a, "A"), cache)
    old_bak <- file.path(cache, "xg_model_st.json.bak")
    writeLines("kept by an earlier failed restore", old_bak)
    rel <- fake_assets(b, "B")
    env <- new.env(); env$files <- rel
    local_mocked_bindings(download.file = mock_release(env), .package = "utils")
    local_mocked_bindings(.xg_cache_dir = function() cache)
    local_fake_xgboost()
    expect_true(.load_xg_models())
    expect_identical(readLines(old_bak), "kept by an earlier failed restore")
    expect_identical(.xg_env$xg_model_version, "B")
})

test_that("a rollback that cannot restore a file keeps its backup and says so", {
    a <- withr::local_tempdir(); b <- withr::local_tempdir(); cache <- withr::local_tempdir()
    cache_set(fake_assets(a, "A"), cache)
    rel <- fake_assets(b, "B")
    env <- new.env(); env$files <- rel
    local_mocked_bindings(download.file = mock_release(env), .package = "utils")
    local_mocked_bindings(.xg_cache_dir = function() cache)
    local_fake_xgboost()
    real_commit <- .xg_commit
    n <- 0L
    local_mocked_bindings(.xg_commit = function(tmp, dest, pin) {
        n <<- n + 1L
        if (n == 2L) cli::cli_abort("Could not write the xG model file {.file {dest}}.")
        real_commit(tmp, dest, pin)
    })
    real_rename <- file.rename
    local_mocked_bindings(file.rename = function(from, to) {
        if (any(grepl(".bak-", from, fixed = TRUE))) return(rep(FALSE, length(from)))  # restore fails
        real_rename(from, to)
    }, .package = "base")
    expect_error(.load_xg_models(), "could not restore")
    expect_gte(length(list.files(cache, pattern = "[.]bak-")), 1L)  # the previous copies are kept
})
