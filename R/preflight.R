# =============================================================================
# preflight.R
# Option 9: preflight_check()
#
# Pre-flight diagnostic tool for probability-linked data. Checks for issues
# specific to record linkage error correction BEFORE running adjMixture(),
# adjELE(), or adjMixBayes().
#
# Usage:
#   preflight_check(linked_data, outcome, predictors, match_prob, method)
#   preflight_check(..., provider = "anthropic")
# =============================================================================


#' Pre-Flight Data Quality Check for Probability-Linked Data
#'
#' Evaluates data quality before running linkage error correction models.
#' Unlike general regression diagnostics, all checks are tailored to the
#' record linkage setting: match probability quality, implied mismatch rate,
#' differential error risk, and method-specific sample size requirements.
#' Returns a structured diagnostic report with severity levels and optional
#' AI-generated recommendations.
#'
#' @param linked_data A data frame of linked records (the analysis dataset).
#' @param outcome Character string. Name of the outcome variable.
#' @param predictors Character vector. Names of predictor variables to check.
#'   If \code{NULL}, all columns except \code{outcome} and \code{match_prob_col}
#'   are used.
#' @param match_prob Numeric vector of posterior correct-match probabilities
#'   (e.g. from the linkage model). Length must equal \code{nrow(linked_data)}.
#'   Required for all linkage-specific checks.
#' @param method Character string. The adjustment method the user intends to
#'   run: \code{"adjMixture"} (default), \code{"adjELE"}, or
#'   \code{"adjMixBayes"}. Enables method-specific checks.
#' @param family Character string. Outcome model family:
#'   \code{"gaussian"} (default), \code{"binomial"}, \code{"poisson"},
#'   or \code{"cox"}.
#' @param min_n Integer. Absolute minimum sample size. Default \code{100}.
#' @param max_missing Numeric in \code{[0,1]}. Max acceptable missing fraction
#'   per variable. Default \code{0.20}.
#' @param provider Character string. LLM provider for AI recommendations.
#'   \code{NULL} (default) skips the AI call. Options: \code{"anthropic"},
#'   \code{"openai"}, \code{"gemini"}, \code{"ollama"}.
#' @param api_key Character string. API key (or \code{NULL} to use env var).
#' @param model Character string. LLM model name. Provider defaults apply.
#' @param verbose Logical. Print the diagnostic report. Default \code{TRUE}.
#'
#' @return Invisibly returns a list of class \code{"postlink_preflight"}:
#'   \itemize{
#'     \item \code{checks} — data frame with columns \code{check},
#'       \code{status} (\code{"OK"/"WARNING"/"ERROR"}), \code{detail},
#'       \code{recommendation}.
#'     \item \code{n_errors}, \code{n_warnings} — integer counts.
#'     \item \code{pass} — \code{TRUE} if no ERRORs.
#'     \item \code{ai_recommendations} — AI suggestions or \code{NULL}.
#'     \item \code{mismatch_rate} — implied mismatch rate from \code{match_prob}.
#'   }
#'
#' @details
#' \strong{Linkage-specific checks performed:}
#' \itemize{
#'   \item \strong{Match probability distribution}: distinguishes records with
#'     high confidence (prob > 0.9) from ambiguous records (0.3–0.7). A large
#'     ambiguous zone indicates poor linkage blocking strategy.
#'   \item \strong{Implied mismatch rate}: \eqn{1 - \bar{p}} where \eqn{\bar{p}}
#'     is mean match probability. Flags when mismatch is very high (> 30\%)
#'     or negligibly low (< 1\%), where adjustment adds no value.
#'   \item \strong{Non-differential linkage error}: tests whether outcome mean
#'     differs substantially between high- and low-confidence linked records.
#'     A large difference suggests error is differential w.r.t. the outcome —
#'     a violation of the \code{adjELE} model assumption.
#'   \item \strong{Safe matches}: warns if no \code{safe.matches} index is
#'     likely available, which is recommended for \code{adjMixture} to anchor
#'     the EM algorithm.
#'   \item \strong{Method-specific sample size}: \code{adjMixBayes} requires
#'     larger samples (>= 300 recommended) due to Stan MCMC; \code{adjMixture}
#'     EM convergence degrades below n = 150 at high mismatch.
#'   \item \strong{Missing values}: flags variables with missing fraction above
#'     \code{max_missing}. Missingness in match variables is especially risky.
#'   \item \strong{Outcome distribution}: family-specific range/balance checks.
#' }
#'
#' @examples
#' \dontrun{
#' data(lifem)
#' # Compute match probabilities first (e.g. from fastLink or your linkage step)
#' # match_prob <- your_linkage_model$posterior
#'
#' result <- preflight_check(
#'   linked_data = lifem,
#'   outcome     = "age_at_death",
#'   predictors  = c("unit_yob", "commf", "comml"),
#'   match_prob  = match_prob,
#'   method      = "adjMixture",
#'   family      = "gaussian"
#' )
#'
#' # With AI recommendations
#' result <- preflight_check(
#'   linked_data = lifem,
#'   outcome     = "age_at_death",
#'   match_prob  = match_prob,
#'   method      = "adjMixture",
#'   provider    = "anthropic"
#' )
#' }
#'
#' @export
preflight_check <- function(linked_data,
                             outcome,
                             predictors   = NULL,
                             match_prob   = NULL,
                             method       = c("adjMixture", "adjELE", "adjMixBayes"),
                             family       = c("gaussian", "binomial", "poisson", "cox"),
                             min_n        = 100L,
                             max_missing  = 0.20,
                             provider     = NULL,
                             api_key      = NULL,
                             model        = NULL,
                             verbose      = TRUE) {

  method <- match.arg(method)
  family <- match.arg(family)

  if (!is.data.frame(linked_data))
    stop("'linked_data' must be a data frame.", call. = FALSE)
  if (!outcome %in% names(linked_data))
    stop("Outcome variable '", outcome, "' not found in 'linked_data'.", call. = FALSE)

  if (is.null(predictors))
    predictors <- setdiff(names(linked_data), outcome)
  else {
    bad <- setdiff(predictors, names(linked_data))
    if (length(bad) > 0)
      stop("Predictor(s) not found: ", paste(bad, collapse=", "), call. = FALSE)
  }

  n      <- nrow(linked_data)
  y      <- linked_data[[outcome]]
  checks <- list()

  # ── 1. Match probability distribution ─────────────────────────────────────
  # This is the most postlink-specific check: how well-separated are the probs?
  if (!is.null(match_prob)) {
    if (length(match_prob) != n) {
      checks[["mp_distribution"]] <- .pf_check(
        "Match probability distribution",
        "ERROR",
        sprintf("length(match_prob) = %d but nrow(linked_data) = %d.", length(match_prob), n),
        "match_prob must be a vector of length nrow(linked_data)."
      )
    } else {
      pct_hi   <- mean(match_prob > 0.9,  na.rm=TRUE)   # confident matches
      pct_ambig <- mean(match_prob > 0.3 & match_prob <= 0.7, na.rm=TRUE)  # grey zone
      pct_low  <- mean(match_prob < 0.1,  na.rm=TRUE)   # confident non-matches

      if (pct_ambig > 0.40) {
        checks[["mp_distribution"]] <- .pf_check(
          "Match probability distribution",
          "WARNING",
          sprintf("%.1f%% of records fall in the ambiguous zone (0.3–0.7). Only %.1f%% are high-confidence (>0.9).",
                  100*pct_ambig, 100*pct_hi),
          "A large ambiguous zone reduces adjustment precision. Review linkage blocking variables or add more discriminating matching fields."
        )
      } else if (pct_hi < 0.30) {
        checks[["mp_distribution"]] <- .pf_check(
          "Match probability distribution",
          "WARNING",
          sprintf("Only %.1f%% of records have match probability > 0.9. Linkage confidence is low overall.",
                  100*pct_hi),
          "Consider using safe.matches to anchor high-confidence pairs and restrict ambiguous records."
        )
      } else {
        checks[["mp_distribution"]] <- .pf_check(
          "Match probability distribution",
          "OK",
          sprintf("%.1f%% of records are high-confidence (>0.9). Ambiguous zone (0.3–0.7): %.1f%%.",
                  100*pct_hi, 100*pct_ambig),
          NULL
        )
      }
    }
  } else {
    checks[["mp_distribution"]] <- .pf_check(
      "Match probability distribution",
      "WARNING",
      "No match_prob supplied. All linkage-specific checks are skipped.",
      "Pass match_prob = your_linkage_model$posterior for a full diagnostic."
    )
  }

  # ── 2. Implied mismatch rate ───────────────────────────────────────────────
  mismatch_rate <- NA_real_
  if (!is.null(match_prob) && length(match_prob) == n) {
    mismatch_rate <- 1 - mean(match_prob, na.rm=TRUE)

    if (mismatch_rate > 0.30) {
      checks[["mismatch_rate"]] <- .pf_check(
        "Implied mismatch rate",
        "ERROR",
        sprintf("Implied mismatch rate = %.1f%%. Adjustment models are unreliable above ~30%%.",
                100*mismatch_rate),
        "Revisit linkage criteria to improve match quality before proceeding. Consider stricter blocking or threshold tuning."
      )
    } else if (mismatch_rate < 0.01) {
      checks[["mismatch_rate"]] <- .pf_check(
        "Implied mismatch rate",
        "WARNING",
        sprintf("Implied mismatch rate = %.2f%%. Linkage error correction adds negligible value at this rate.",
                100*mismatch_rate),
        sprintf("With near-perfect linkage, running a standard %s() without adjustment may be appropriate.",
                sub("adj", "", method))
      )
    } else {
      checks[["mismatch_rate"]] <- .pf_check(
        "Implied mismatch rate",
        "OK",
        sprintf("Implied mismatch rate = %.1f%%. Within the effective range for %s().",
                100*mismatch_rate, method),
        NULL
      )
    }
  }

  # ── 3. Non-differential linkage error (adjELE assumption) ─────────────────
  # Compare outcome distribution in high-confidence vs low-confidence records.
  # A large difference suggests differential error — problematic especially for adjELE.
  if (!is.null(match_prob) && length(match_prob) == n && !any(is.na(c(match_prob, y)))) {
    hi_mask  <- match_prob > 0.8
    lo_mask  <- match_prob < 0.5

    if (sum(hi_mask) >= 10 && sum(lo_mask) >= 10) {
      y_hi <- y[hi_mask]
      y_lo <- y[lo_mask]

      if (family == "binomial") {
        diff_prop <- abs(mean(y_hi == 1) - mean(y_lo == 1))
        threshold <- 0.15
        detail_fn <- function(d) sprintf("Outcome prevalence: high-confidence = %.1f%%, low-confidence = %.1f%% (diff = %.1f pp).",
                                          100*mean(y_hi==1), 100*mean(y_lo==1), 100*d)
      } else {
        # gaussian / poisson: standardised mean difference
        pooled_sd <- sd(c(y_hi, y_lo))
        diff_prop <- if (pooled_sd > 0) abs(mean(y_hi) - mean(y_lo)) / pooled_sd else 0
        threshold <- 0.50  # Cohen's d
        detail_fn <- function(d) sprintf("Outcome mean: high-confidence = %.3g, low-confidence = %.3g (std diff = %.2f).",
                                          mean(y_hi), mean(y_lo), d)
      }

      if (diff_prop > threshold) {
        checks[["differential_error"]] <- .pf_check(
          "Non-differential linkage error",
          "WARNING",
          detail_fn(diff_prop),
          if (method == "adjELE")
            "adjELE assumes non-differential linkage error. This pattern suggests the assumption may be violated — consider adjMixture() instead, which is more robust to differential error."
          else
            "Outcome differs between high/low match-confidence records. Linkage error may be differential. Interpret results cautiously."
        )
      } else {
        checks[["differential_error"]] <- .pf_check(
          "Non-differential linkage error",
          "OK",
          detail_fn(diff_prop),
          NULL
        )
      }
    }
  }

  # ── 4. Method-specific sample size ────────────────────────────────────────
  min_n_method <- switch(method,
    adjMixBayes  = 300L,   # Stan MCMC needs more draws to converge
    adjMixture   = 150L,   # EM stable above ~150, especially at high mismatch
    adjELE       = 100L    # ELE closed-form; smaller n acceptable
  )
  min_n_use <- max(min_n, min_n_method)

  if (n < min_n_use) {
    checks[["sample_size"]] <- .pf_check(
      "Sample size",
      if (n < min_n) "ERROR" else "WARNING",
      sprintf("n = %d. %s() is recommended for n >= %d.", n, method, min_n_use),
      switch(method,
        adjMixBayes = "Stan MCMC may not converge reliably with small samples. Consider adjMixture() as a faster alternative.",
        adjMixture  = "EM convergence can be unstable with small n and high mismatch. Bootstrap CIs are strongly recommended.",
        "Consider collecting more linked records or using a simpler model."
      )
    )
  } else {
    checks[["sample_size"]] <- .pf_check(
      "Sample size",
      "OK",
      sprintf("n = %s — adequate for %s().", format(n, big.mark=","), method),
      NULL
    )
  }

  # ── 5. Missing values ─────────────────────────────────────────────────────
  all_vars  <- c(outcome, predictors)
  miss_frac <- sapply(linked_data[, all_vars, drop=FALSE], function(x) mean(is.na(x)))
  high_miss <- miss_frac[miss_frac > max_missing]

  if (length(high_miss) > 0) {
    detail <- paste(sprintf("%s (%.1f%%)", names(high_miss), 100*high_miss), collapse=", ")
    checks[["missing_values"]] <- .pf_check(
      "Missing values",
      "WARNING",
      paste0("High missingness in: ", detail, "."),
      "In linked datasets, missingness often correlates with linkage quality. Consider multiple imputation that accounts for linkage uncertainty."
    )
  } else {
    checks[["missing_values"]] <- .pf_check(
      "Missing values",
      "OK",
      sprintf("Maximum missingness: %.1f%%.", 100*max(miss_frac)),
      NULL
    )
  }

  # ── 6. Outcome distribution ───────────────────────────────────────────────
  if (family == "binomial") {
    if (!all(y %in% c(0L, 1L, NA))) {
      checks[["outcome_dist"]] <- .pf_check(
        "Outcome distribution",
        "ERROR",
        "Outcome has values outside {0, 1} but family = 'binomial'.",
        "Ensure outcome is coded 0/1 before fitting."
      )
    } else {
      p_pos <- mean(y == 1, na.rm=TRUE)
      p_min <- min(p_pos, 1-p_pos)
      status <- if (p_min < 0.05) "ERROR" else if (p_min < 0.10) "WARNING" else "OK"
      checks[["outcome_dist"]] <- .pf_check(
        "Outcome distribution",
        status,
        sprintf("%.1f%% positive cases (minority class = %.1f%%).", 100*p_pos, 100*p_min),
        if (status != "OK")
          "Severe class imbalance interacts poorly with high mismatch rates. The EM algorithm may misattribute rare outcomes to mismatch noise."
        else NULL
      )
    }
  } else if (family == "gaussian") {
    yv <- var(y, na.rm=TRUE)
    checks[["outcome_dist"]] <- .pf_check(
      "Outcome distribution",
      if (is.na(yv) || yv < .Machine$double.eps) "ERROR" else "OK",
      sprintf("Gaussian outcome: mean = %.3g, SD = %.3g.", mean(y, na.rm=TRUE), sqrt(max(yv, 0))),
      if (is.na(yv) || yv < .Machine$double.eps)
        "Outcome has near-zero variance — cannot be modelled." else NULL
    )
  } else {
    checks[["outcome_dist"]] <- .pf_check(
      "Outcome distribution",
      "OK",
      sprintf("%s family: basic range check passed.", family),
      NULL
    )
  }

  # ── Compile ────────────────────────────────────────────────────────────────
  checks_df  <- do.call(rbind, lapply(checks, as.data.frame, stringsAsFactors=FALSE))
  rownames(checks_df) <- NULL
  n_errors   <- sum(checks_df$status == "ERROR")
  n_warnings <- sum(checks_df$status == "WARNING")
  passed     <- n_errors == 0

  # ── Optional AI recommendations ───────────────────────────────────────────
  ai_recs <- NULL
  if (!is.null(provider)) {
    provider <- .detect_provider(provider)
    api_key  <- .resolve_key(api_key, provider)

    issues <- checks_df[checks_df$status %in% c("ERROR","WARNING"), , drop=FALSE]
    if (nrow(issues) > 0) {
      issue_lines <- paste(apply(issues, 1, function(r)
        sprintf("[%s] %s: %s", r["status"], r["check"], r["detail"])
      ), collapse="\n")

      sys_msg <- paste0(
        "You are a statistical consultant specialising in record linkage and ",
        "linkage error correction in R. The user is about to run ", method,
        "() from the postlink package. Given the data quality issues below, ",
        "provide concise, actionable recommendations specific to the linkage ",
        "setting. Reference postlink arguments (safe.matches, m.rate, etc.) ",
        "where relevant. 4–6 sentences max."
      )
      usr_msg <- paste0(
        "Dataset: n = ", format(n, big.mark=","),
        ", method = ", method,
        ", family = ", family, "\n",
        if (!is.na(mismatch_rate))
          paste0("Implied mismatch rate: ", round(100*mismatch_rate, 1), "%\n")
        else "",
        "Issues:\n", issue_lines
      )

      message("Querying ", provider, " for recommendations...")
      tryCatch({
        res     <- .call_llm(usr_msg, sys_msg, provider, api_key, model, max_tokens=400)
        ai_recs <- res$text
      }, error=function(e) {
        warning("AI call failed: ", conditionMessage(e), call.=FALSE)
      })
    } else {
      ai_recs <- "All checks passed — no issues to address before running the adjustment model."
    }
  }

  # ── Return ────────────────────────────────────────────────────────────────
  out <- structure(
    list(checks             = checks_df,
         n_errors           = n_errors,
         n_warnings         = n_warnings,
         pass               = passed,
         ai_recommendations = ai_recs,
         mismatch_rate      = mismatch_rate,
         method             = method,
         family             = family,
         n                  = n),
    class = "postlink_preflight"
  )

  if (verbose) print(out)
  invisible(out)
}


# ── print method ──────────────────────────────────────────────────────────────
#' @export
print.postlink_preflight <- function(x, ...) {
  cat("\n=== postlink: Pre-Flight Diagnostic ===\n")
  cat(sprintf("    Method: %s() | Family: %s | n = %s\n",
              x$method, x$family, format(x$n, big.mark=",")))
  if (!is.na(x$mismatch_rate))
    cat(sprintf("    Implied mismatch rate: %.1f%%\n", 100*x$mismatch_rate))
  cat("\n")

  icons <- c(OK="✓", WARNING="⚠", ERROR="✗")

  for (i in seq_len(nrow(x$checks))) {
    r   <- x$checks[i, ]
    ico <- icons[r$status]
    cat(sprintf("  %s [%-7s] %s\n", ico, r$status, r$check))
    cat(sprintf("           %s\n", r$detail))
    if (!is.na(r$recommendation) && nchar(r$recommendation) > 0)
      cat(sprintf("           ➜ %s\n", r$recommendation))
    cat("\n")
  }

  if (x$n_errors == 0 && x$n_warnings == 0) {
    cat("Result: PASS — ready to run adjustment models.\n")
  } else {
    cat(sprintf("Result: %d ERROR(s)  %d WARNING(s)\n", x$n_errors, x$n_warnings))
    if (!x$pass) cat("        Resolve ERROR(s) before proceeding.\n")
    else         cat("        Review WARNING(s) and proceed with caution.\n")
  }

  if (!is.null(x$ai_recommendations)) {
    cat("\n--- AI Recommendations ---\n")
    cat(x$ai_recommendations, "\n")
    cat("[AI-generated — verify before use]\n")
  }
  cat("\n")
  invisible(x)
}


# ── Internal helper ───────────────────────────────────────────────────────────
#' @noRd
.pf_check <- function(check, status, detail, recommendation) {
  list(check          = check,
       status         = status,
       detail         = detail,
       recommendation = if (is.null(recommendation)) NA_character_ else recommendation)
}
