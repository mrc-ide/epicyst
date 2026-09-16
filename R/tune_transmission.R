## =============================================================================
## R/tune_transmission.R
##
## Solve EPICYST's three transmission coefficients (tau, beta, theta) so that
## the integrated, age-structured system sits at a specified set of endemic
## equilibrium prevalences.
##
## This replaces the practice of inflating the nominal prevalence arguments to
## set_up() until the burn-in drift happens to land on the wanted values.
## set_up()'s PCPrev/TPrev/CPrev arguments control BOTH the initial state
## vector and the analytically derived coefficients, so a single argument fixes
## where the trajectory starts and, through an approximate inversion of a
## reduced form of the system, where it ends. The two do not coincide. Here the
## coefficients are supplied directly via tau_input/beta_input/theta_input,
## which decouples them from the prevalence arguments, so the start and the
## equilibrium can be made to agree.
##
## Depends: nleqslv
## =============================================================================


#' @title
#' Console progress reporter
#'
#' @description
#' Returns a closure that prints a single-line progress bar, overwriting itself
#' with a carriage return. Used by the tuning stages, where each tick is one
#' integration of the model and the total is not known in advance.
#'
#' @details
#' `budget` is an expected number of evaluations, not a limit. The bar
#' saturates at full width once `n` reaches it but the counter keeps rising, so
#' an iteration that takes longer than expected is visible rather than
#' appearing to stall. The `max|res|` field shows the smallest maximum absolute
#' relative residual seen so far, which falls monotonically as the solver
#' converges and is the most useful single indicator of progress.
#'
#' Overwriting with `\r` works in an interactive console but not in knitr or a
#' logged batch run, where each tick prints on its own line. Pass
#' `quiet = TRUE` in those contexts.
#'
#' @param label Short stage name, printed before the bar. Pad to a fixed width
#'   if several stages should align.
#' @param budget Expected number of ticks, used to scale the bar.
#' @param quiet If `TRUE`, suppress all output; `tick()` and `done()` still
#'   accumulate counts and timings.
#'
#' @return A list of three functions: `tick(resid)` advances the bar and
#'   optionally records a residual vector; `done()` clears the bar, prints a
#'   summary line, and returns `c(n, secs, best)` invisibly; `n()` returns the
#'   current count.
#'
#' @keywords internal
#' @noRd
make_progress <- function(label, budget, quiet = FALSE) {
  t0 <- Sys.time(); n <- 0L; best <- Inf
  fmt <- function(s) sprintf("%d:%02d", as.integer(s) %/% 60L, as.integer(s) %% 60L)
  
  list(
    tick = function(resid = NULL) {
      n <<- n + 1L
      if (!is.null(resid) && all(is.finite(resid)))
        best <<- min(best, max(abs(resid)))
      if (quiet) return(invisible(NULL))
      
      el   <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      frac <- min(n / budget, 1)
      w    <- 24L; filled <- as.integer(round(frac * w))
      bar  <- paste0("[", strrep("=", max(filled - 1L, 0)),
                     if (filled > 0 && filled < w) ">" else if (filled >= w) "=" else "",
                     strrep(" ", w - filled), "]")
      
      cat(sprintf("\r  %s %s %3d/~%d | %s elapsed | max|res| %s    ",
                  label, bar, n, budget, fmt(el),
                  if (is.finite(best)) formatC(best, format = "e", digits = 2)
                  else "   --   "))
      utils::flush.console()
      invisible(NULL)
    },
    done = function() {
      el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      if (!quiet) cat(sprintf("\r  %s done: %d evaluations in %s%s\n",
                              label, n, fmt(el), strrep(" ", 30)))
      invisible(c(n = n, secs = el, best = best))
    },
    n = function() n)
}

#' Equilibrium prevalences for a given set of transmission coefficients
#'
#' Forward map used by the tuner. Integrates the model for \code{burn_in} and
#' returns the prevalences at the end of a short trailing horizon, together
#' with a drift diagnostic.
#'
#' @param tau,beta,theta Transmission coefficients.
#' @param base_args Named list of fixed \code{set_up()} arguments.
#' @param burn_in Burn-in length passed to \code{run_model()}.
#' @param eq_time Post-burn-in horizon; only needs to be long enough to expose
#'   residual movement.
#'
#' @return Named numeric vector: \code{PC}, \code{PC_slgt}, \code{T}, \code{C},
#'   \code{E}, \code{drift}, \code{ok}. \code{ok = 0} marks an inadmissible
#'   parameter set (\code{set_up()} errored), with all other entries \code{NA}.
#' @export
epicyst_equilibrium <- function(tau, beta, theta, base_args,
                                burn_in = 200, eq_time = 5) {

  s <- tryCatch(
    
    do.call(set_up_analytic, c(base_args, list(tau_input   = tau,
                                      beta_input  = beta,
                                      theta_input = theta))),
    error = function(e) e)

  # An inadmissible point is a return value, not a stop(): set_up() errors
  # where implied pi_L/pi_H exceed one, which a trust-region step can provoke.
  # The caller converts ok = 0 into a large finite residual and steps away.
  if (inherits(s, "error"))
    return(c(PC = NA_real_, PC_slgt = NA_real_, T = NA_real_, C = NA_real_,
             E = NA_real_, drift = NA_real_, ok = 0))

  out <- run_model(time = eq_time, burn_in = burn_in,
                   params = s[[1]], initial_states = s[[2]],
                   num_intervention_rounds = 0)

  df   <- if (is.data.frame(out)) out else out$Output
  last <- df[nrow(df), , drop = FALSE]

  drift <- NA_real_
  if (nrow(df) > 1) {
    prv <- df[nrow(df) - 1, , drop = FALSE]
    drift <- max(
      abs(last$Pig_Cysticercosis_prev - prv$Pig_Cysticercosis_prev) /
        max(last$Pig_Cysticercosis_prev, 1e-12),
      abs(last$Human_Taeniasis_prev - prv$Human_Taeniasis_prev) /
        max(last$Human_Taeniasis_prev, 1e-12))
  }

  # Slaughter-age pig prevalence -- the quantity the human FOI sees. Columns
  # are reordered NUMERICALLY: "IPL[10]" precedes "IPL[2]" under character
  # ordering, which with 150 classes would silently aggregate the wrong ones.
  pull <- function(stem) {
    cols <- grep(paste0("^", stem, "\\[[0-9]+\\]$"), names(last), value = TRUE)
    idx  <- as.integer(sub(".*\\[([0-9]+)\\]$", "\\1", cols))
    as.numeric(last[1, cols[order(idx)]])
  }
  fi  <- s[[1]]$slgage_foi:s[[1]]$na_pig
  num <- sum(pull("IPL")[fi]) + sum(pull("IPH")[fi])
  den <- num + sum(pull("SP")[fi]) + sum(pull("PP")[fi]) +
    sum(pull("RP")[fi]) + sum(pull("VP")[fi])

  c(PC = last$Pig_Cysticercosis_prev, PC_slgt = num / den,
    T = last$Human_Taeniasis_prev, C = last$Human_Cysticercosis_prev,
    E = last$E, drift = drift, ok = 1)
}


#' Choose a burn-in length by level agreement
#'
#' Selects the shortest candidate whose equilibrium prevalences agree with
#' those of the longest candidate to within \code{level_tol} in relative terms.
#'
#' A one-step drift criterion is not used. Drift decays at the rate of the
#' slowest eigenvalue and becomes small well before the trajectory reaches its
#' asymptote: a burn-in passing a 1e-6 drift test has been observed to sit
#' 0.12\% off in level, two orders of magnitude above the tolerance asked of
#' the solver.
#'
#' @param base_args Fixed \code{set_up()} arguments.
#' @param tau,beta,theta Coefficients at which to profile (the analytic
#'   starting values are the natural choice).
#' @param candidates Burn-in lengths to try, ascending.
#' @param level_tol Relative agreement required against the longest candidate.
#' @param eq_time Trailing horizon.
#' @param verbose Print the candidate table and a progress bar over candidates.
#'
#' @return The chosen burn-in, with two attributes: `"table"`, a data frame of
#'   every candidate with its elapsed time, the three prevalences,
#'   slaughter-age porcine prevalence, drift, and relative error against the
#'   longest candidate; and `"analytic_prev"`, the prevalences reached at the
#'   chosen burn-in on the coefficients supplied. The second is what
#'   [tune_transmission()] reports as the analytic column of its summary, so
#'   the cost of measuring the pre-tuning equilibrium is absorbed into
#'   profiling rather than paid separately.
#' @export
profile_burn_in <- function(base_args, tau, beta, theta,
                            candidates = c(50, 100, 200, 400),
                            level_tol = 1e-5, eq_time = 5, verbose = TRUE) {

  candidates <- sort(unique(candidates))
  rows <- vector("list", length(candidates))

  if (verbose) cat("  Stage 1/3  burn-in profiling\n")
  pb <- if (verbose) utils::txtProgressBar(0, length(candidates), style = 3) else NULL
  
  for (k in seq_along(candidates)) {
    tk <- system.time(
      r <- epicyst_equilibrium(tau, beta, theta, base_args,
                               burn_in = candidates[k], eq_time = eq_time)
    )[["elapsed"]]
    rows[[k]] <- data.frame(burn_in = candidates[k], secs = tk,
                            PC = r[["PC"]], PC_slgt = r[["PC_slgt"]],
                            T = r[["T"]], C = r[["C"]], drift = r[["drift"]])
    if (verbose) utils::setTxtProgressBar(pb, k)
  }
  if (verbose) { close(pb); cat("\n") }

  tab <- do.call(rbind, rows)
  ref <- tab[nrow(tab), ]
  tab$rel_err <- pmax(abs(tab$PC / ref$PC - 1),
                      abs(tab$T  / ref$T  - 1),
                      abs(tab$C  / ref$C  - 1))
  if (verbose) print(tab, row.names = FALSE)

  # The reference is the longest candidate, not a proven asymptote. This test
  # establishes that shorter burn-ins agree with it, not that it has itself
  # converged.
  if (is.finite(ref$drift) && ref$drift > 1e-9)
    warning("Longest burn-in candidate is still drifting (",
            signif(ref$drift, 3), "); extend `candidates`.", call. = FALSE)

  ok     <- tab$burn_in[is.finite(tab$rel_err) & tab$rel_err < level_tol]
  chosen <- if (length(ok)) min(ok) else max(candidates)
  structure(chosen, table = tab,
            analytic_prev = c(PC = tab$PC[tab$burn_in == chosen][1],
                              PC_slgt = tab$PC_slgt[tab$burn_in == chosen][1],
                              T = tab$T[tab$burn_in == chosen][1],
                              C = tab$C[tab$burn_in == chosen][1]))
}


#' Tune EPICYST transmission coefficients to target equilibrium prevalences
#'
#' Solves jointly for \code{tau}, \code{beta} and \code{theta} such that the
#' integrated age-structured model sits at the requested endemic equilibrium.
#'
#' All three coefficients are solved simultaneously. A sequential scheme
#' (\code{(tau, beta)} against pig and taeniasis prevalence, then \code{theta}
#' against human cysticercosis) converges only if the Jacobian is
#' block-triangular, which it is not: \code{theta} shifts human compartment
#' occupancy, which shifts taeniasis, egg output and hence pig prevalence.
#' 
#' #' Progress is reported in three stages: burn-in profiling, the joint solve,
#' and verification. The solve stage shows a bar against an expected budget of
#' evaluations rather than a known total, since the iteration count of a
#' quasi-Newton solve is not determined in advance. Evaluations served from the
#' internal cache do not tick the bar, so the count reflects integrations
#' actually performed. Set `verbose = FALSE` in knitr or batch contexts, where
#' carriage-return overwriting produces one line per tick.
#'
#' On completion the returned object is printed, giving analytic and tuned
#' coefficients side by side with their ratio, and analytic and tuned
#' equilibrium prevalences against target with relative errors for each. The
#' analytic error column quantifies the gap that tuning closes.
#'
#' @param PCPrev,TPrev,CPrev Target equilibrium prevalences for porcine
#'   cysticercosis, human taeniasis and human cysticercosis. All three are
#'   required: leaving \code{TPrev} at its default while targeting a different
#'   value silently derives the analytic starting point against the wrong
#'   prevalence.
#' @param ... Further fixed arguments passed to \code{set_up()} (demography,
#'   population sizes, age classes). Names are checked against
#'   \code{formals(set_up)} and a mismatch is an error.
#' @param pc_basis Whether \code{PCPrev} refers to all-age pig prevalence or to
#'   prevalence among pigs at or above the minimum slaughter age. Most field
#'   estimates come from slaughter-slab or market-age surveys; the two differ
#'   substantially and the choice materially changes \code{tau} and
#'   \code{beta}.
#' @param burn_in Burn-in length. If \code{NULL} (default), chosen by
#'   \code{profile_burn_in()}.
#' @param burn_in_candidates Candidates passed to the profiler.
#' @param eq_time Trailing horizon used to expose residual drift.
#' @param tol Acceptance criterion: every prevalence within this relative
#'   distance of its target. A warning, not an error, if missed.
#' @param jac_step Finite-difference step for the Jacobian, in log space.
#'   Deliberately larger than the solver's internal default of
#'   \code{sqrt(.Machine$double.eps) * |x|}, which at \code{log(tau) ~ -22} is a
#'   relative parameter perturbation comparable to the integrator's own noise
#'   floor, so the difference quotient would measure integration error rather
#'   than sensitivity.
#' @param start Optional starting coefficients \code{c(tau, beta, theta)}.
#'   Defaults to the analytic values from \code{set_up()}.
#' @param verbose Report progress.
#'
#' @return Object of class \code{"epicyst_tuned"}. Elements \code{[[1]]} and
#'   \code{[[2]]} are the parameter list and initial states, so the object can
#'   be used wherever a \code{set_up()} result is expected. 
#'   `analytic_prev` holds the equilibrium prevalences reached on the analytic
#'   coefficients, for the comparison printed by [print.epicyst_tuned()].
#' @export
tune_transmission <- function(PCPrev, TPrev, CPrev, ...,
                              pc_basis = c("all_age", "slaughter_age"),
                              burn_in = NULL,
                              burn_in_candidates = c(50, 100, 200, 400),
                              eq_time = 5,
                              tol = 1e-6,
                              jac_step = 1e-3,
                              start = NULL,
                              verbose = TRUE) {

  if (!requireNamespace("nleqslv", quietly = TRUE))
    stop("tune_transmission() requires the nleqslv package.", call. = FALSE)

  pc_basis <- match.arg(pc_basis)
  pc_field <- if (pc_basis == "slaughter_age") "PC_slgt" else "PC"

  targets <- c(PC = PCPrev, T = TPrev, C = CPrev)
  if (any(!is.finite(targets)) || any(targets <= 0) || any(targets >= 1))
    stop("Targets must be finite prevalences strictly between 0 and 1.",
         call. = FALSE)

  ## ---- assemble and validate the fixed set_up() arguments -----------------
  dots <- list(...)
  if (any(c("tau_input", "beta_input", "theta_input") %in% names(dots)))
    stop("tau_input/beta_input/theta_input are solved for, not supplied.",
         call. = FALSE)

  base_args <- c(list(PCPrev = PCPrev, TPrev = TPrev, CPrev = CPrev), dots)

  # R matches argument names case-sensitively. A function with `...` swallows a
  # misspelling silently and uses the default, which is the failure mode
  # hardest to detect downstream, so check explicitly.
  fm  <- names(formals(set_up_analytic))
  bad <- setdiff(names(base_args), fm)
  if (length(bad))
    stop("set_up() has no argument(s): ", paste(bad, collapse = ", "),
         "\n  Available: ", paste(fm, collapse = ", "), call. = FALSE)
  if ("..." %in% fm)
    warning("set_up() accepts `...`; misspelled arguments will be ignored ",
            "silently rather than erroring.", call. = FALSE)

  ## ---- analytic starting point --------------------------------------------
  base   <- do.call(set_up_analytic, base_args)
  p_base <- base[[1]]
  beta0  <- p_base$chi * (p_base$pil * p_base$phi +
                          p_base$pih * (1 - p_base$phi))

  if (is.null(start)) start <- c(p_base$tau, beta0, p_base$theta)
  if (length(start) != 3L || any(!is.finite(start)) || any(start <= 0))
    stop("`start` must be three positive, finite coefficients.", call. = FALSE)

  ## ---- memoised forward map ------------------------------------------------
  # The solver requests the residual and the Jacobian at the same point, and
  # the Jacobian evaluates its own base point, so without a cache each is a
  # fresh integration. The key includes burn_in and eq_time: caching on the
  # coefficients alone would serve a short-burn-in result to a caller that
  # asked for a long one. emptyenv() as parent so a miss is a miss rather than
  # a lexical lookup into the calling frame.
  cache <- new.env(parent = emptyenv())
  n_eval <- 0L
  
  prog <- NULL

  run_eq <- function(tau, beta, theta, burn, cache_ok = TRUE) {
    key <- paste(sprintf("%.14g", c(tau, beta, theta, burn, eq_time)),
                 collapse = "|")
    if (cache_ok && !is.null(cache[[key]])) return(cache[[key]])
    r <- epicyst_equilibrium(tau, beta, theta, base_args,
                             burn_in = burn, eq_time = eq_time)
    n_eval <<- n_eval + 1L
    if (cache_ok) assign(key, r, envir = cache)
    if (!is.null(prog))
      prog$tick(if (r[["ok"]] == 1)
        c(r[[pc_field]] / targets[["PC"]] - 1,
          r[["T"]] / targets[["T"]] - 1,
          r[["C"]] / targets[["C"]] - 1) else NULL)
    r
  }

  ## ---- burn-in -------------------------------------------------------------
  burn_tab <- NULL; analytic_prev <- NULL
  if (is.null(burn_in)) {
    bi            <- profile_burn_in(base_args, start[1], start[2], start[3],
                                     candidates = burn_in_candidates,
                                     eq_time = eq_time, verbose = verbose)
    burn_tab      <- attr(bi, "table")
    analytic_prev <- attr(bi, "analytic_prev")
    burn_in       <- as.numeric(bi)
    if (verbose) message(sprintf("  burn_in = %g", burn_in))
  } else {
    r0 <- run_eq(start[1], start[2], start[3], burn_in, cache_ok = FALSE)
    analytic_prev <- c(PC = r0[["PC"]], PC_slgt = r0[["PC_slgt"]],
                       T = r0[["T"]], C = r0[["C"]])
  }

  ## ---- residual and Jacobian, in log space ---------------------------------
  # Log scale: the coefficients are positive and span ~8 orders of magnitude,
  # so a single step length is meaningful for all three; positivity is
  # automatic; and prevalence responds roughly log-linearly, which conditions
  # the problem for a Newton-type method.
  # Relative residuals: the targets differ by an order of magnitude, so
  # absolute residuals would silently weight the pig target ~8x the taeniasis
  # target.
  resid <- function(lp) {
    r <- run_eq(exp(lp[1]), exp(lp[2]), exp(lp[3]), burn_in)
    if (r[["ok"]] == 0 || !all(is.finite(r[c(pc_field, "T", "C")])))
      return(rep(1e3, 3))       # large but FINITE; NA/Inf breaks the update
    c(r[[pc_field]] / targets[["PC"]] - 1,
      r[["T"]]      / targets[["T"]]  - 1,
      r[["C"]]      / targets[["C"]]  - 1)
  }

  jac <- function(lp, ...) {
    f0 <- resid(lp)             # free: the cache almost always holds it
    J  <- matrix(NA_real_, 3L, 3L)
    for (j in 1:3) {
      lp_p <- lp; lp_p[j] <- lp_p[j] + jac_step
      J[, j] <- (resid(lp_p) - f0) / jac_step
    }
    J
  }

  ## ---- solve ---------------------------------------------------------------
  # Broyden: one Jacobian plus rank-one updates. Each residual evaluation is a
  # full integration, so the binding cost is evaluations, and Newton would
  # rebuild the Jacobian (3 integrations) every iteration.
  # Double-dogleg: the admissible region is bounded by set_up()'s pi_L/pi_H
  # check, and an unglobalised full step from a poor start can leave it.
  if (verbose) cat("  Stage 2/3  solving for tau, beta, theta\n")
  prog <- make_progress("solve   ", budget = 40, quiet = !verbose)
  t0   <- Sys.time()
  fit  <- nleqslv::nleqslv(
    log(start), resid, jac = jac, method = "Broyden", global = "dbldog",
    control = list(ftol = 1e-12, xtol = 1e-10, btol = 1e-4, maxit = 200))
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (verbose) prog$done()
  prog <- NULL

  coefs <- exp(fit$x)
  names(coefs) <- c("tau", "beta", "theta")

  ## ---- verification --------------------------------------------------------
  # Evaluated at a longer burn-in than was used for fitting. Separates two
  # failure modes that look identical in the final numbers: on target at
  # burn_in but moving at the longer one means the profiler was too permissive;
  # off at both means the solver did not converge.
  if (verbose) cat("  Stage 3/3  verification at two burn-in lengths\n")
  long_burn <- max(burn_in, max(burn_in_candidates))
  fin  <- run_eq(coefs[1], coefs[2], coefs[3], burn_in,   cache_ok = FALSE)
  finL <- if (long_burn > burn_in)
    run_eq(coefs[1], coefs[2], coefs[3], long_burn, cache_ok = FALSE) else fin

  achieved <- c(PC = fin[[pc_field]], T = fin[["T"]], C = fin[["C"]])
  rel_err  <- achieved / targets - 1

  if (max(abs(rel_err)) > tol)
    warning("Tuning did not reach tol = ", format(tol), " (max |rel err| ",
            signif(max(abs(rel_err)), 3), "). Inspect $diagnostics$fit.",
            call. = FALSE)

  ## ---- calibrated model object ---------------------------------------------
  s_cal <- do.call(set_up_analytic, c(base_args, list(tau_input   = coefs[["tau"]],
                                             beta_input  = coefs[["beta"]],
                                             theta_input = coefs[["theta"]])))

  out <- list(
    params         = s_cal[[1]],
    initial_states = s_cal[[2]],
    coefficients   = coefs,
    analytic       = c(tau = p_base$tau, beta = beta0, theta = p_base$theta),
    analytic_prev  = analytic_prev,
    targets        = targets,
    achieved       = achieved,
    rel_err        = rel_err,
    pc_basis       = pc_basis,
    burn_in        = burn_in,
    diagnostics    = list(
      fit            = fit,
      evaluations    = n_eval,
      elapsed_secs   = elapsed,
      drift          = fin[["drift"]],
      burn_in_table  = burn_tab,
      long_burn_in   = long_burn,
      long_burn_prev = c(PC = finL[["PC"]], T = finL[["T"]], C = finL[["C"]]),
      slaughter_PC   = fin[["PC_slgt"]],
      all_age_PC     = fin[["PC"]]))

  class(out) <- "epicyst_tuned"
  if (verbose) print(out)
  invisible(out)
}


#' @title
#' Print a tuned EPICYST object
#'
#' @description
#' Summarises a [tune_transmission()] result: transmission coefficients and
#' equilibrium prevalences, analytic against tuned, with relative errors.
#'
#' @param x An object of class `"epicyst_tuned"`.
#' @param ... Ignored, for consistency with the generic.
#'
#' @return `x`, invisibly.
#'
#' @export
print.epicyst_tuned <- function(x, ...) {
  rule <- function(s) cat(strrep("=", 62), "\n", s, "\n", strrep("=", 62), "\n", sep = "")
  rule(sprintf(" TUNING COMPLETE  |  %d evaluations, %.1f min, burn-in %g",
               x$diagnostics$evaluations, x$diagnostics$elapsed_secs / 60, x$burn_in))
  
  co <- data.frame(parameter = c("tau", "beta", "theta"),
                   analytic  = as.numeric(x$analytic),
                   tuned     = as.numeric(x$coefficients))
  co$ratio <- co$tuned / co$analytic
  cat("\nTransmission coefficients\n")
  print(co, row.names = FALSE, digits = 6)
  
  ap <- x$analytic_prev
  pv <- data.frame(
    prevalence = c("PC", "T", "C"),
    target     = as.numeric(x$targets),
    analytic   = if (is.null(ap)) NA_real_ else
      as.numeric(ap[c(if (x$pc_basis == "slaughter_age")
        "PC_slgt" else "PC", "T", "C")]),
    tuned      = as.numeric(x$achieved))
  pv$analytic_err <- pv$analytic / pv$target - 1
  pv$tuned_err    <- pv$tuned    / pv$target - 1
  cat(sprintf("\nEquilibrium prevalences (%s basis)\n", x$pc_basis))
  print(pv, row.names = FALSE, digits = 6)
  
  cat(sprintf("\nall-age PC %.5f | slaughter-age PC %.5f | final drift %.1e\n",
              x$diagnostics$all_age_PC, x$diagnostics$slaughter_PC,
              x$diagnostics$drift))
  d <- x$diagnostics$long_burn_prev /
    c(x$diagnostics$all_age_PC, x$achieved[["T"]], x$achieved[["C"]]) - 1
  cat(sprintf("shift at burn-in %g: PC %+.1e  T %+.1e  C %+.1e\n",
              x$diagnostics$long_burn_in, d[1], d[2], d[3]))
  invisible(x)
}


#' Decompose a tuned beta into its burden-specific components
#'
#' \code{beta} is the phi-weighted mean of the low- and high-burden
#' pig-to-human coefficients (Winskill et al. 2017, eqn 11):
#' \code{beta = chi*pi_L*phi + chi*pi_H*(1 - phi)}.
#'
#' One coefficient suffices at equilibrium because \code{I_PL : I_PH} is
#' exactly \code{phi : (1 - phi)} -- both compartments draw on the same inflow
#' term and are depleted at the same rate -- so
#' \code{beta_L*I_PL + beta_H*I_PH} collapses to the weighted mean times
#' \code{I_P}. That identity survives interventions which move both classes
#' proportionally (pig MDA, vaccination) but NOT meat inspection, which cuts
#' the two by different fractions. Its post-implementation coefficient must be
#' rebuilt from the components, not scaled.
#'
#' @param x An \code{"epicyst_tuned"} object.
#' @param inspect_L,inspect_H Proportional reductions in \code{beta_L} and
#'   \code{beta_H} under improved meat inspection.
#' @param verbose Print the decomposition and the identity checks.
#'
#' @return Invisible list of components and their post-inspection values.
#' @export
decompose_beta <- function(x, inspect_L = 0.20, inspect_H = 0.40,
                           verbose = TRUE) {

  stopifnot(inherits(x, "epicyst_tuned"))
  p     <- x$params
  scale <- x$coefficients[["beta"]] / x$analytic[["beta"]]
  wmean <- function(l, h) l * p$phi + h * (1 - p$phi)

  # pi_L and pi_H as set_up() derived them, scaled by the solver's correction.
  # chi and phi are fixed and pi_H is pinned to pi_L by a fixed ratio, so the
  # analytic beta is a function of pi_L alone and the correction to beta IS a
  # correction to pi_L. Note chi and pi_L enter only as a product and are not
  # separately identifiable; attributing the scaling to pi_L is presentational.
  pil <- p$pil * scale; pih <- p$pih * scale
  bL  <- p$chi * pil;   bH  <- p$chi * pih
  bLa <- p$chi * p$pil; bHa <- p$chi * p$pih

  if (verbose) {
    cat(sprintf("chi %.4f | phi %.4f | pi_H/pi_L %.4f | scale %.6f\n",
                p$chi, p$phi, p$pih / p$pil, scale))
    tab <- data.frame(
      quantity   = c("pi_L", "pi_H", "beta_L", "beta_H", "beta"),
      analytic   = c(p$pil, p$pih, bLa, bHa, wmean(bLa, bHa)),
      calibrated = c(pil,   pih,   bL,  bH,  wmean(bL, bH)))
    tab$ratio <- tab$calibrated / tab$analytic
    print(tab, row.names = FALSE, digits = 6)

    # If either identity fails, the package's decomposition differs from
    # eqn 11 and everything below is wrong.
    cat(sprintf("check: composite(analytic) vs beta0 %+.2e | composite(tuned) vs beta %+.2e\n",
                wmean(bLa, bHa) / x$analytic[["beta"]] - 1,
                wmean(bL, bH) / x$coefficients[["beta"]] - 1))

    post  <- wmean(bL * (1 - inspect_L), bH * (1 - inspect_H))
    naive <- x$coefficients[["beta"]] * (1 - mean(c(inspect_L, inspect_H)))
    cat(sprintf("meat inspection: via components %.6e (%.1f%%) | naive average %.6e (%.1f%%, wrong)\n",
                post,  100 * (1 - post  / x$coefficients[["beta"]]),
                naive, 100 * (1 - naive / x$coefficients[["beta"]])))
  }

  invisible(list(chi = p$chi, phi = p$phi, scale = scale,
                 pi_L = pil, pi_H = pih, beta_L = bL, beta_H = bH,
                 beta_L_inspect = bL * (1 - inspect_L),
                 beta_H_inspect = bH * (1 - inspect_H)))
}


## =============================================================================
## Usage
##
##   d_nat <- 1 / (15 * 12)
##   d_slg <- 1 / 12 - d_nat          # dPslg = 0.0777778
##   slgep <- (1 / d_slg) / 12        # 1.0714 years
##
##   tuned <- tune_transmission(
##     PCPrev = 0.25, TPrev = 0.03, CPrev = 0.06,
##     psi = 1 / 1.5, slaughter_age_min = 6,
##     LEP = 15, slgEP = slgep,
##     number_age_classes_pig = 150, PPS = 1000, HPS = 12000)
##
##   tuned                                   # print method
##   decompose_beta(tuned)
##
##   hp1 <- run_model(time = 4, burn_in = tuned$burn_in,
##                    params = tuned[[1]], initial_states = tuned[[2]])
## =============================================================================
