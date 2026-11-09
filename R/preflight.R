# =============================================================================
# preflight.R
# Option 9: preflight_check()
#
# Pre-flight diagnostic tool that evaluates data quality BEFORE running any
# linkage adjustment models. Checks for common issues that would make results
# unreliable or models fail to converge.
#
# Usage:
#   preflight_check(linked_data, outcome, predictors)
#   preflight_check(linked_data, outcome, predictors, provider = "anthropic")
# =============================================================================


#' Pre-Flight Data Quality Check Before Linkage Adjustment
#'
#' Evaluates data quality before running linkage error correction models.
#' Checks for missing values, class imbalance, sample size, distributional
#' issues, and potential collinearity. Returns a structured diagnostic report
#' with severity levels (OK, WARNING, ERROR) and optional AI-generated
#' recommendations.
#'
#' @param linked_data A data frame of linked records (the analysis dataset).
#' @param outcome Character string. Name of the outcome variable in
#'   \code{linked_data}.
#' @param predictors Character vector. Names of predictor variables to check.
#'   If \code{NULL}, all columns except \code{outcome} are used.
#' @param match_prob Optional numeric vector of posterior correct match
#'   probabilities (e.g. from \code{adjMixture()$match.prob}). When supplied,
#'   additional checks on match quality are performed.
#' @param family Character string. Outcome model family: one of
#'   \code{"gaussian"} (default), \code{"binomial"}, \code{"poisson"},
#'   or \code{"cox"}.
#' @param min_n Integer. Minimum acceptable sample size. Default \code{100}.
#' @param max_missing Numeric in \code{[0, 1]}. Maximum acceptable missing
#'   value fraction per variable. Default \code{0.20} (20\%).
#' @param provider Character string. LLM provider for AI recommendations.
#'   If \code{NULL} (default), no AI call is made and only the deterministic
#'   checks are run. Set to \code{"anthropic"}, \code{"openai"},
#'   \code{"gemini"}, or \code{"ollama"} to enable AI suggestions.
#' @param api_key Character string. API key for the chosen provider. If
#'   \code{NULL}, read from the appropriate environment variable.
#' @param model Character string. LLM model name. Provider defaults apply.
#' @param verbose Logical. Print the diagnostic report to console.
#'   Default \code{TRUE}.
#'
#' @return Invisibly returns a list of class \code{"postlink_preflight"} with
#'   elements:
#'   \itemize{
#'     \item \code{checks} — data frame of all checks with columns
#'       \code{check}, \code{status} (\code{"OK"}, \code{"WARNING"},
#'       \code{"ERROR"}), \code{detail}, and \code{recommendation}.
#'     \item \code{n_errors} — number of ERROR-level issues.
#'     \item \code{n_warnings} — number of WARNING-level issues.
#'     \item \code{pass} — logical; \code{TRUE} if no ERRORs.
#'     \item \code{ai_recommendations} — character string of AI suggestions,
#'       or \code{NULL} if \code{provider} is \code{NULL}.
#'   }
#'
#' @details
#' Checks performed:
#' \itemize{
#'   \item \strong{Sample size}: flags if \code{n < min_n} (ERROR) or
#'     \code{n < 200} (WARNING).
#'   \item \strong{Missing values}: flags variables with missing fraction above
#'     \code{max_missing} threshold.
#'   \item \strong{Outcome distribution}: for binomial, checks class balance
#'     (ERROR if minority class < 5\%, WARNING if < 10\%); for Gaussian,
#'     checks for near-zero variance.
#'   \item \strong{Predictor variance}: flags near-constant predictors.
#'   \item \strong{Collinearity}: flags predictor pairs with \eqn{|r| > 0.95}.
#'   \item \strong{Match probability quality} (if supplied): flags if mean
#'     match probability is below 0.5 or if many records have probability < 0.3.
#' }
#'
#' @examples
#' \dontrun{
#' data(lifem)
#' adj <- adjMixture(linked.data = lifem, m.formula = ~ commf + comml,
#'                   m.rate = 0.05, safe.matches = hndlnk)
#'
#' # Deterministic checks only
#' result <- preflight_check(
#'   linked_data = lifem,
#'   outcome     = "age_at_death",
#'   predictors  = c("unit_yob", "commf", "comml"),
#'   match_prob  = adj$match.prob,
#'   family      = "gaussian"
#' )
#'
#' # With AI recommendations
#' result <- preflight_check(
#'   linked_data = lifem,
#'   outcome     = "age_at_death",
#'   predictors  = c("unit_yob", "commf", "comml"),
#'   match_prob  = adj$match.prob,
#'   family      = "gaussian",
#'   provider    = "anthropic"
#' )
#' }
#'
#' @export
preflight_check <- function(linked_data,
                             outcome,
                             predictors  = NULL,
                             match_prob  = NULL,
                             family      = c("gaussian", "binomial", "poisson", "cox"),
                             min_n       = 100L,
                             max_missing = 0.20,
                             provider    = NULL,
                             api_key     = NULL,
                             model       = NULL,
                             verbose     = TRUE) {

  family <- match.arg(family)

  if (!is.data.frame(linked_data))
    stop("'linked_data' must be a data frame.", call. = FALSE)
  if (!outcome %in% names(linked_data))
    stop("Outcome variable '", outcome, "' not found in 'linked_data'.", call. = FALSE)

  if (is.null(predictors)) {
    predictors <- setdiff(names(linked_data), outcome)
  } else {
    missing_preds <- setdiff(predictors, names(linked_data))
    if (length(missing_preds) > 0)
      stop("Predictor(s) not found in 'linked_data': ",
           paste(missing_preds, collapse=", "), call. = FALSE)
  }

  checks <- list()

  # ── 1. Sample size ────────────────────────────────────────────────────────
  n <- nrow(linked_data)
  if (n < min_n) {
    checks[["sample_size"]] <- .pf_check(
      "Sample size",
      "ERROR",
      sprintf("n = %d, below minimum threshold of %d.", n, min_n),
      "Linkage error correction models require sufficient sample sizes. Consider expanding the linked dataset or relaxing linkage criteria."
    )
  } else if (n < 200) {
    checks[["sample_size"]] <- .pf_check(
      "Sample size",
      "WARNING",
      sprintf("n = %d. Linkage error correction models perform best with n >= 200.", n),
      "Results may be unstable. Interpret with caution and consider bootstrapped confidence intervals."
    )
  } else {
    checks[["sample_size"]] <- .pf_check(
      "Sample size",
      "OK",
      sprintf("n = %s.", format(n, big.mark=",")),
      NULL
    )
  }

  # ── 2. Missing values ─────────────────────────────────────────────────────
  all_vars  <- c(outcome, predictors)
  miss_frac <- sapply(linked_data[, all_vars, drop=FALSE],
                      function(x) mean(is.na(x)))
  high_miss <- miss_frac[miss_frac > max_missing]

  if (length(high_miss) > 0) {
    detail <- paste(
      sprintf("%s (%.1f%%)", names(high_miss), 100 * high_miss),
      collapse = ", "
    )
    checks[["missing_values"]] <- .pf_check(
      "Missing values",
      "WARNING",
      paste0("High missingness in: ", detail, "."),
      "Consider multiple imputation or complete-case analysis. Document missingness assumptions."
    )
  } else {
    max_miss_pct <- round(100 * max(miss_frac), 1)
    checks[["missing_values"]] <- .pf_check(
      "Missing values",
      "OK",
      sprintf("Maximum missingness across variables: %.1f%%.", max_miss_pct),
      NULL
    )
  }

  # ── 3. Outcome distribution ───────────────────────────────────────────────
  y <- linked_data[[outcome]]

  if (family == "binomial") {
    if (!all(y %in% c(0, 1, NA))) {
      checks[["outcome_dist"]] <- .pf_check(
        "Outcome distribution",
        "ERROR",
        "Outcome variable has values outside {0, 1} but family = 'binomial'.",
        "Ensure the outcome is binary (0/1) before fitting a logistic regression."
      )
    } else {
      prop_minority <- min(mean(y == 1, na.rm=TRUE), mean(y == 0, na.rm=TRUE))
      if (prop_minority < 0.05) {
        checks[["outcome_dist"]] <- .pf_check(
          "Outcome distribution",
          "ERROR",
          sprintf("Extreme class imbalance: minority class = %.1f%%.", 100 * prop_minority),
          "Severe imbalance can destabilize EM convergence and inflate mismatch sensitivity. Consider oversampling, undersampling, or a penalized model."
        )
      } else if (prop_minority < 0.10) {
        checks[["outcome_dist"]] <- .pf_check(
          "Outcome distribution",
          "WARNING",
          sprintf("Moderate class imbalance: minority class = %.1f%%.", 100 * prop_minority),
          "Monitor EM convergence carefully. Report sensitivity analyses with different mismatch rate assumptions."
        )
      } else {
        checks[["outcome_dist"]] <- .pf_check(
          "Outcome distribution",
          "OK",
          sprintf("Class balance OK: %.1f%% positive.", 100 * mean(y == 1, na.rm=TRUE)),
          NULL
        )
      }
    }
  } else if (family == "gaussian") {
    y_var <- var(y, na.rm=TRUE)
    if (is.na(y_var) || y_var < .Machine$double.eps) {
      checks[["outcome_dist"]] <- .pf_check(
        "Outcome distribution",
        "ERROR",
        "Outcome variable has near-zero variance (essentially constant).",
        "A constant outcome cannot be modelled. Check data extraction or recoding."
      )
    } else {
      checks[["outcome_dist"]] <- .pf_check(
        "Outcome distribution",
        "OK",
        sprintf("Gaussian outcome: mean = %.2f, SD = %.2f.", mean(y, na.rm=TRUE), sqrt(y_var)),
        NULL
      )
    }
  } else if (family == "poisson") {
    if (any(y < 0, na.rm=TRUE)) {
      checks[["outcome_dist"]] <- .pf_check(
        "Outcome distribution",
        "ERROR",
        "Poisson outcome has negative values.",
        "Poisson regression requires non-negative integer counts."
      )
    } else {
      checks[["outcome_dist"]] <- .pf_check(
        "Outcome distribution",
        "OK",
        sprintf("Poisson outcome: mean = %.2f, range [%g, %g].",
                mean(y, na.rm=TRUE), min(y, na.rm=TRUE), max(y, na.rm=TRUE)),
        NULL
      )
    }
  } else {
    # cox — just check for non-negative times
    checks[["outcome_dist"]] <- .pf_check(
      "Outcome distribution",
      "OK",
      "Cox model: survival time checks skipped (pass event vector separately).",
      NULL
    )
  }

  # ── 4. Predictor variance ──────────────────────────────────────────────────
  num_preds <- predictors[sapply(linked_data[, predictors, drop=FALSE], is.numeric)]
  low_var   <- character(0)

  for (pv in num_preds) {
    v <- var(linked_data[[pv]], na.rm=TRUE)
    if (!is.na(v) && v < .Machine$double.eps)
      low_var <- c(low_var, pv)
  }

  if (length(low_var) > 0) {
    checks[["predictor_variance"]] <- .pf_check(
      "Predictor variance",
      "ERROR",
      paste0("Near-zero variance predictors: ", paste(low_var, collapse=", "), "."),
      "Remove or recode these variables before fitting. Constant predictors cause matrix singularity."
    )
  } else {
    checks[["predictor_variance"]] <- .pf_check(
      "Predictor variance",
      "OK",
      sprintf("%d numeric predictor(s) checked — all have acceptable variance.", length(num_preds)),
      NULL
    )
  }

  # ── 5. Collinearity ───────────────────────────────────────────────────────
  if (length(num_preds) >= 2) {
    pred_mat    <- linked_data[, num_preds, drop=FALSE]
    pred_mat    <- pred_mat[complete.cases(pred_mat), , drop=FALSE]
    high_corr_pairs <- character(0)

    if (nrow(pred_mat) > 1) {
      cor_mat <- tryCatch(cor(pred_mat), error=function(e) NULL)
      if (!is.null(cor_mat)) {
        for (i in seq_len(nrow(cor_mat)-1)) {
          for (j in seq(i+1, ncol(cor_mat))) {
            if (!is.na(cor_mat[i,j]) && abs(cor_mat[i,j]) > 0.95)
              high_corr_pairs <- c(high_corr_pairs,
                sprintf("%s & %s (r = %.3f)", rownames(cor_mat)[i],
                        colnames(cor_mat)[j], cor_mat[i,j]))
          }
        }
      }
    }

    if (length(high_corr_pairs) > 0) {
      checks[["collinearity"]] <- .pf_check(
        "Collinearity",
        "WARNING",
        paste0("Near-perfect collinearity (|r| > 0.95): ", paste(high_corr_pairs, collapse="; "), "."),
        "Consider removing one variable from each collinear pair, or using a regularized model."
      )
    } else {
      checks[["collinearity"]] <- .pf_check(
        "Collinearity",
        "OK",
        "No predictor pairs with |r| > 0.95 detected.",
        NULL
      )
    }
  } else {
    checks[["collinearity"]] <- .pf_check(
      "Collinearity",
      "OK",
      "Fewer than 2 numeric predictors — collinearity check skipped.",
      NULL
    )
  }

  # ── 6. Match probability quality ──────────────────────────────────────────
  if (!is.null(match_prob)) {
    if (length(match_prob) != n) {
      checks[["match_quality"]] <- .pf_check(
        "Match probability quality",
        "WARNING",
        sprintf("Length of match_prob (%d) != nrow(linked_data) (%d). Skipping match quality checks.",
                length(match_prob), n),
        "Ensure match.prob corresponds to the same linked dataset."
      )
    } else {
      mean_mp    <- mean(match_prob, na.rm=TRUE)
      pct_low    <- mean(match_prob < 0.3, na.rm=TRUE)
      pct_hi     <- mean(match_prob > 0.9, na.rm=TRUE)
      mismatch_r <- 1 - mean_mp

      if (mean_mp < 0.5) {
        checks[["match_quality"]] <- .pf_check(
          "Match probability quality",
          "ERROR",
          sprintf("Mean match probability = %.3f. Implies >50%% mismatch rate.", mean_mp),
          "Very low match quality. Revisit linkage criteria before running adjustment models."
        )
      } else if (pct_low > 0.20 || mismatch_r > 0.20) {
        checks[["match_quality"]] <- .pf_check(
          "Match probability quality",
          "WARNING",
          sprintf("Mean match prob = %.3f (implied mismatch ~%.1f%%). %.1f%% of records have prob < 0.3.",
                  mean_mp, 100*mismatch_r, 100*pct_low),
          "Consider reviewing linkage blocking strategy or using safe.matches for high-confidence pairs."
        )
      } else {
        checks[["match_quality"]] <- .pf_check(
          "Match probability quality",
          "OK",
          sprintf("Mean match prob = %.3f (implied mismatch ~%.1f%%). %.1f%% of records exceed 0.9.",
                  mean_mp, 100*mismatch_r, 100*pct_hi),
          NULL
        )
      }
    }
  }

  # ── Compile results ────────────────────────────────────────────────────────
  checks_df <- do.call(rbind, lapply(checks, as.data.frame, stringsAsFactors=FALSE))
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

      system_msg <- paste0(
        "You are a statistical consultant advising on record linkage analysis in R. ",
        "The user is about to run a linkage error correction model using the postlink package. ",
        "Given the data quality issues below, provide concise, actionable recommendations. ",
        "Be specific about R functions or arguments where relevant. 3-5 sentences max."
      )
      user_msg <- paste0(
        "Dataset: n = ", format(n, big.mark=","), ", outcome family = '", family, "'\n",
        "Issues detected:\n", issue_lines, "\n\n",
        "What should the analyst do before running adjMixture() / adjELE() / adjMixBayes()?"
      )

      message("Querying ", provider, " API for recommendations...")
      tryCatch({
        result  <- .call_llm(user_msg, system_msg, provider, api_key, model, max_tokens=400)
        ai_recs <- result$text
      }, error = function(e) {
        warning("AI recommendations failed: ", conditionMessage(e), call. = FALSE)
      })
    } else {
      ai_recs <- "All checks passed. No issues to address before proceeding."
    }
  }

  # ── Output object ─────────────────────────────────────────────────────────
  out <- structure(
    list(checks            = checks_df,
         n_errors          = n_errors,
         n_warnings        = n_warnings,
         pass              = passed,
         ai_recommendations = ai_recs),
    class = "postlink_preflight"
  )

  if (verbose) print(out)

  invisible(out)
}


# ── print method ──────────────────────────────────────────────────────────────
#' @export
print.postlink_preflight <- function(x, ...) {
  cat("\n=== postlink: Pre-Flight Diagnostic ===\n\n")

  icons <- c(OK = "✓", WARNING = "⚠", ERROR = "✗")
  colors_avail <- tryCatch({ crayon::green; TRUE }, error=function(e) FALSE)

  for (i in seq_len(nrow(x$checks))) {
    r   <- x$checks[i, ]
    ico <- icons[r$status]
    cat(sprintf("  %s [%s] %s\n", ico, r$status, r$check))
    cat(sprintf("     %s\n", r$detail))
    if (!is.na(r$recommendation) && nchar(r$recommendation) > 0)
      cat(sprintf("     ➜ %s\n", r$recommendation))
    cat("\n")
  }

  if (x$n_errors == 0 && x$n_warnings == 0) {
    cat("Result: PASS — no issues detected. Ready to run adjustment models.\n")
  } else {
    cat(sprintf("Result: %d ERROR(s), %d WARNING(s) detected.\n",
                x$n_errors, x$n_warnings))
    if (!x$pass)
      cat("Action required: resolve ERROR(s) before proceeding.\n")
    else
      cat("Review WARNING(s) and proceed with caution.\n")
  }

  if (!is.null(x$ai_recommendations)) {
    cat("\n--- AI Recommendations ---\n")
    cat(x$ai_recommendations)
    cat("\n[AI-generated. Verify before use.]\n")
  }

  cat("\n")
  invisible(x)
}


# ── Helper: build a single check row ─────────────────────────────────────────
#' @noRd
.pf_check <- function(check, status, detail, recommendation) {
  list(
    check          = check,
    status         = status,
    detail         = detail,
    recommendation = if (is.null(recommendation)) NA_character_ else recommendation
  )
}
