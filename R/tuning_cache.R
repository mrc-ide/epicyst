## =============================================================================
## R/tuning_cache.R
##
## The layer between set_up(tune = TRUE) and tune_transmission().
##
## Tuning costs tens of model integrations -- minutes. Without a cache, every
## call repeats them, which makes tune = TRUE unusable as a default and
## unusable inside anything that calls set_up() more than once. This file adds
## the memoisation set_up() already expects: tuned_coefficients() plus the
## user-facing cache management.
##
## Keys are built from the FULL resolved argument set, not the call as written,
## so set_up(PCPrev = 0.25, tune = TRUE) and set_up(0.25, tune = TRUE) resolve
## to the same key, and so do two calls that differ only in an argument left at
## its default.
##
## Depends: nothing beyond base R and tools.
## =============================================================================


#' @title
#' Cache directory for tuned transmission coefficients
#'
#' @description
#' Where [tuned_coefficients()] stores its results.
#'
#' @details
#' Defaults to `tools::R_user_dir("epicyst", "cache")`, overridable with
#' `options(epicyst.cache_dir = )` or the `EPICYST_CACHE_DIR` environment
#' variable -- the latter matters on a cluster, where a shared cache lets
#' parallel workers reuse one another's results instead of each tuning the same
#' configuration.
#'
#' During `R CMD check` the location is redirected to the session temporary
#' directory, since a package's examples and tests must not write to a user
#' cache.
#'
#' @param create Create the directory if absent.
#' @return The path, invisibly if created.
#' @export
epicyst_cache_dir <- function(create = FALSE) {

  checking <- nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) ||
    !identical(Sys.getenv("NOT_CRAN"), "true") &&
      nzchar(Sys.getenv("_R_CHECK_TIMINGS_"))

  d <- if (checking) {
    file.path(tempdir(), "epicyst-cache")
  } else {
    getOption("epicyst.cache_dir",
              Sys.getenv("EPICYST_CACHE_DIR",
                         tools::R_user_dir("epicyst", "cache")))
  }
  if (create) dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}


#' @title
#' Fingerprint of the installed package
#'
#' @description
#' Included in every cache key so that a change to the model invalidates stored
#' coefficients.
#'
#' @details
#' The package version alone is not enough during development: editing the odin
#' model does not bump it, so a cache keyed on version alone would serve
#' coefficients fitted to a model that no longer exists -- a failure that is
#' silent and produces plausible numbers. The fingerprint therefore also
#' incorporates the most recent modification time of the package's source
#' files where those are visible, which they are under `devtools::load_all()`.
#'
#' @return A short character fingerprint.
#' @keywords internal
#' @noRd
epicyst_fingerprint <- function() {

  ver <- tryCatch(as.character(utils::packageVersion("epicyst")),
                  error = function(e) "unknown")

  src <- tryCatch(find.package("epicyst"), error = function(e) NA_character_)
  mt  <- NA_character_
  if (!is.na(src)) {
    f <- list.files(file.path(src, c("R", "src", "inst", "odin")),
                    full.names = TRUE, recursive = TRUE)
    if (length(f)) {
      m <- suppressWarnings(max(file.info(f)$mtime, na.rm = TRUE))
      if (is.finite(m)) mt <- format(m, "%Y%m%d%H%M%S")
    }
  }
  paste(ver, mt, sep = "-")
}


#' @title
#' Resolve a partial argument list against set_up_analytic()'s defaults
#'
#' @description
#' Fills in every argument the caller did not supply, so that two calls
#' differing only in an unstated default hash to the same key.
#'
#' @param named Named list of supplied arguments, already evaluated.
#' @return A named list covering every non-dots formal that has a usable value.
#' @keywords internal
#' @noRd
resolve_setup_args <- function(named) {

  fm <- formals(set_up_analytic)
  fm <- fm[setdiff(names(fm), "...")]

  out <- list()
  for (nm in names(fm)) {
    if (nm %in% names(named)) { out[[nm]] <- named[[nm]]; next }
    d <- fm[[nm]]
    if (identical(d, quote(expr = ))) next          # no default: skip
    v <- tryCatch(eval(d, envir = out, enclos = baseenv()),
                  error = function(e) NULL)
    if (!is.null(v)) out[[nm]] <- v
  }

  ## Anything the caller passed that is not a formal (possible, since
  ## set_up_analytic() may take `...`) is kept: it can still change the model.
  extra <- setdiff(names(named), names(out))
  for (nm in extra) out[[nm]] <- named[[nm]]

  out[order(names(out))]
}


#' @title
#' Cache key for one tuning configuration
#'
#' @description
#' An MD5 digest of the resolved arguments, the porcine prevalence basis and
#' the package fingerprint.
#'
#' @details
#' Hashing is done by serialising to a temporary file and calling
#' `tools::md5sum()`, which keeps the package free of a hashing dependency.
#' Names are sorted before serialisation so argument order cannot change the
#' key.
#'
#' @keywords internal
#' @noRd
tuning_key <- function(args, pc_basis) {
  payload <- list(args = args[order(names(args))],
                  pc_basis = pc_basis,
                  fingerprint = epicyst_fingerprint())
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(payload, tf, version = 3, compress = FALSE)
  unname(tools::md5sum(tf))
}


#' @title
#' Tuned transmission coefficients, memoised on disk
#'
#' @description
#' Returns `tau`, `beta` and `theta` for a given `set_up_analytic()`
#' configuration, tuning with [tune_transmission()] on a cache miss and
#' storing the result.
#'
#' @details
#' Called by [set_up()] when `tune = TRUE`; not normally called directly.
#'
#' Writes are atomic -- to a temporary file in the cache directory, then
#' renamed -- because `file.rename()` within a directory is atomic on every
#' supported platform, whereas a partially written RDS would be indistinguishable
#' from a valid one on the next read.
#'
#' Concurrency is deliberately not locked. Several parallel workers missing the
#' same key will each tune, wasting minutes per worker but producing identical
#' results and a correct cache. A lock would remove that duplication at the
#' cost of a stale lock blocking every future call, which is a worse failure.
#' Where it matters, tune once before dispatching work -- a single `set_up(...,
#' tune = TRUE)` call in the parent populates the cache for every worker that
#' shares the directory.
#'
#' @param named Named list of arguments for `set_up_analytic()`.
#' @param cache Use the on-disk cache. `FALSE` forces a fresh tune and still
#'   stores the result.
#' @param pc_basis Passed to [tune_transmission()]. Defaults to
#'   `getOption("epicyst.pc_basis", "all_age")`.
#' @param verbose Passed to [tune_transmission()]. Defaults to
#'   `getOption("epicyst.tune_verbose", TRUE)`.
#' @param ... Further arguments for [tune_transmission()], such as `tol` or
#'   `burn_in`. These form part of the cache key.
#'
#' @return Named numeric vector `c(tau, beta, theta)`, with the full
#'   `"epicyst_tuned"` diagnostics attached as attribute `"tuned"`.
#' @export
tuned_coefficients <- function(named, cache = TRUE,
                               pc_basis = getOption("epicyst.pc_basis",
                                                    "all_age"),
                               verbose = getOption("epicyst.tune_verbose",
                                                   TRUE),
                               ...) {

  args <- resolve_setup_args(named)

  need <- c("PCPrev", "TPrev", "CPrev")
  miss <- need[!need %in% names(args)]
  if (length(miss))
    stop("Cannot tune without ", paste(miss, collapse = ", "),
         ": no value supplied and no default found in set_up_analytic().",
         call. = FALSE)

  ## Tuning arguments are part of the configuration, so they belong in the key.
  tune_args <- list(...)
  key <- tuning_key(c(args, tune_args), pc_basis)
  dir <- epicyst_cache_dir(create = TRUE)
  f   <- file.path(dir, paste0(key, ".rds"))

  if (isTRUE(cache) && file.exists(f)) {
    hit <- tryCatch(readRDS(f), error = function(e) NULL)
    if (!is.null(hit) && is.numeric(hit$coefficients) &&
        length(hit$coefficients) == 3L) {
      if (isTRUE(verbose))
        message("epicyst: tuned coefficients from cache (", substr(key, 1, 8),
                ")")
      out <- hit$coefficients
      attr(out, "tuned") <- hit$tuned
      return(out)
    }
    ## Unreadable or malformed: treat as a miss rather than propagating it.
    warning("Discarding unreadable tuning cache entry ", basename(f),
            call. = FALSE)
  }

  ## ---- miss: tune ----------------------------------------------------------
  dots <- args[setdiff(names(args), need)]

  tuned <- do.call(tune_transmission,
                   c(list(PCPrev = args$PCPrev,
                          TPrev  = args$TPrev,
                          CPrev  = args$CPrev),
                     dots,
                     list(pc_basis = pc_basis, verbose = verbose),
                     tune_args))

  co <- tuned$coefficients
  stopifnot(is.numeric(co), length(co) == 3L)

  ## Atomic write: rename within the directory, so a reader never sees a
  ## half-written file.
  tmp <- tempfile(tmpdir = dir, fileext = ".tmp")
  saveRDS(list(coefficients = co,
               tuned        = tuned,
               key          = key,
               pc_basis     = pc_basis,
               fingerprint  = epicyst_fingerprint(),
               stored       = Sys.time()),
          tmp, version = 3)
  ok <- file.rename(tmp, f)
  if (!ok) {
    unlink(tmp)
    warning("Could not write the tuning cache to ", dir,
            "; the coefficients are returned but will be recomputed next time.",
            call. = FALSE)
  }

  attr(co, "tuned") <- tuned
  co
}


#' @title
#' Inspect the tuning cache
#'
#' @description
#' Lists stored tuning results with their targets, achieved prevalences and
#' the fingerprint they were computed under.
#'
#' @return A data frame, invisibly if empty.
#' @export
epicyst_cache_list <- function() {
  d <- epicyst_cache_dir()
  f <- list.files(d, pattern = "\\.rds$", full.names = TRUE)
  if (!length(f)) {
    message("epicyst: tuning cache is empty (", d, ")")
    return(invisible(data.frame()))
  }
  rows <- lapply(f, function(p) {
    x <- tryCatch(readRDS(p), error = function(e) NULL)
    if (is.null(x)) return(NULL)
    data.frame(key = substr(basename(p), 1, 8),
               PCPrev = unname(x$tuned$targets[["PC"]]),
               TPrev  = unname(x$tuned$targets[["T"]]),
               CPrev  = unname(x$tuned$targets[["C"]]),
               pc_basis = x$pc_basis,
               max_rel_err = max(abs(x$tuned$rel_err)),
               burn_in = x$tuned$burn_in,
               fingerprint = x$fingerprint,
               stored = format(x$stored, "%Y-%m-%d %H:%M"),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  print(out, row.names = FALSE, digits = 4)
  invisible(out)
}


#' @title
#' Clear the tuning cache
#'
#' @description
#' Removes stored tuning results, forcing the next `set_up(tune = TRUE)` to
#' re-solve.
#'
#' @details
#' Rarely necessary: a change to the package or its model alters the
#' fingerprint and invalidates entries automatically. Use it after changing
#' something the fingerprint cannot see -- a solver tolerance, or a dependency
#' that alters the integration.
#'
#' @param stale_only Remove only entries whose fingerprint differs from the
#'   installed package's.
#' @return Number of files removed, invisibly.
#' @export
epicyst_cache_clear <- function(stale_only = FALSE) {
  d <- epicyst_cache_dir()
  f <- list.files(d, pattern = "\\.rds$", full.names = TRUE)
  if (!length(f)) {
    message("epicyst: tuning cache already empty (", d, ")")
    return(invisible(0L))
  }
  if (isTRUE(stale_only)) {
    fp <- epicyst_fingerprint()
    keep <- vapply(f, function(p) {
      x <- tryCatch(readRDS(p), error = function(e) NULL)
      !is.null(x) && identical(x$fingerprint, fp)
    }, logical(1))
    f <- f[!keep]
    if (!length(f)) {
      message("epicyst: no stale entries")
      return(invisible(0L))
    }
  }
  unlink(f)
  message("epicyst: removed ", length(f), " cached tuning result(s) from ", d)
  invisible(length(f))
}
