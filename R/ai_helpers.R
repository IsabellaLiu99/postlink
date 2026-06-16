# =============================================================================
# ai_helpers.R
# Option 3: explain_fit()
#
# Sends coefficients and standard errors from a fitted postlink model to
# an LLM and returns a plain-language interpretation.
#
# Requires: httr2 (optional dependency)
# API Key:  Set ANTHROPIC_API_KEY in your .Renviron
#           usethis::edit_r_environ()  ->  ANTHROPIC_API_KEY=sk-ant-...
# =============================================================================


# ── Internal helpers ──────────────────────────────────────────────────────────

#' Check httr2 is installed and API key exists
#' @noRd
.check_api_key <- function() {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      "Package 'httr2' is required for AI features.\n",
      "Install it with: install.packages('httr2')",
      call. = FALSE
    )
  }
  key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (nchar(key) == 0) {
    stop(
      "ANTHROPIC_API_KEY not found.\n",
      "Add it to your .Renviron: usethis::edit_r_environ()\n",
      "Then add this line:  ANTHROPIC_API_KEY=sk-ant-...\n",
      "Restart R after saving.",
      call. = FALSE
    )
  }
  invisible(key)
}


#' Extract coefficients and SEs as a tidy data frame
#' @noRd
.get_coef_df <- function(fit) {

 # ELE: coefficients is a matrix (rows = weighting methods, cols = terms)
 # Mixture/Bayes: coefficients is a named numeric vector
 if (is.matrix(fit$coefficients)) {
  est        <- fit$coefficients[1, ]   # take first row (e.g. "ratio")
  term_names <- colnames(fit$coefficients)
 } else {
  est        <- fit$coefficients
  term_names <- names(fit$coefficients)
 }

 # Handle different structures of fit$var:
 # - Mixture: single matrix, take first length(est) diagonal entries
 # - ELE: list of matrices, take first matrix
 if (!is.null(fit$var)) {
  var_mat <- if (is.list(fit$var)) fit$var[[1]] else fit$var
  n_coef  <- length(est)
  se      <- sqrt(diag(var_mat)[seq_len(n_coef)])
 } else {
  se <- rep(NA_real_, length(est))
 }

 data.frame(
  term      = term_names,
  estimate  = round(as.numeric(est), 4),
  se        = round(as.numeric(se),  4),
  row.names = NULL,
  stringsAsFactors = FALSE
 )
}


#' Send one message to the Anthropic API and return the response text
#' @noRd
.call_claude <- function(user_msg, system_msg, max_tokens = 600) {
  key <- .check_api_key()

  body <- list(
    model      = "claude-sonnet-4-6",
    max_tokens = max_tokens,
    system     = system_msg,
    messages   = list(list(role = "user", content = user_msg))
  )

  resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
    httr2::req_headers(
      "x-api-key"         = key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(60) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_status(resp) != 200L) {
    err <- tryCatch(httr2::resp_body_json(resp)$error$message, error = function(e) "")
    stop("Anthropic API error: ", err, call. = FALSE)
  }

  httr2::resp_body_json(resp)$content[[1]]$text
}


# ── Public function ───────────────────────────────────────────────────────────

#' Explain a Fitted postlink Model in Plain Language
#'
#' Sends the coefficients and standard errors from a fitted postlink object
#' to an LLM (Anthropic Claude) and returns a plain-language interpretation
#' of the results.
#'
#' @param fit A fitted postlink object (e.g. from \code{\link{plglm}},
#'   \code{\link{plcoxph}}).
#' @param context Optional character string. One or two sentences describing
#'   the study (e.g. \code{"This study examines the effect of year of birth
#'   on longevity using linked Ohio birth and death certificates."}).
#'   Providing context helps the LLM give a more relevant explanation.
#' @param verbose Logical. If \code{TRUE} (default), prints the explanation
#'   to the console. Set to \code{FALSE} to return the text silently (useful
#'   when passing the output to \code{generate_report()}).
#'
#' @return Invisibly returns the explanation as a single character string.
#'
#' @details
#' The LLM receives only the coefficient names, point estimates, and standard
#' errors extracted from \code{fit}. It is explicitly instructed not to
#' fabricate numbers or make causal claims beyond what the data support.
#'
#' Requires the \code{httr2} package and \code{ANTHROPIC_API_KEY} set in
#' your \code{.Renviron} file. Run \code{usethis::edit_r_environ()} to open
#' that file, add \code{ANTHROPIC_API_KEY=sk-ant-...}, and restart R.
#'
#' @examples
#' \dontrun{
#' # Set your key first (do this once):
#' # usethis::edit_r_environ()
#' # Add line: ANTHROPIC_API_KEY=sk-ant-...
#' # Restart R.
#'
#' data(lifem)
#' adj <- adjMixture(
#'   linked.data  = lifem,
#'   m.formula    = ~ commf + comml,
#'   m.rate       = 0.05,
#'   safe.matches = hndlnk
#' )
#' fit <- plglm(age_at_death ~ poly(unit_yob, 3, raw = TRUE),
#'              family = "gaussian", adjustment = adj)
#'
#' # Basic usage
#' explain_fit(fit)
#'
#' # With study context for a more relevant explanation
#' explain_fit(fit,
#'   context = "This study links Ohio birth and death certificates to examine
#'              how year of birth relates to longevity.")
#'
#' # Capture output silently (e.g. to pass to generate_report)
#' explanation <- explain_fit(fit, verbose = FALSE)
#' }
#'
#' @export
explain_fit <- function(fit, context = NULL, verbose = TRUE) {

  coef_df     <- .get_coef_df(fit)
  family_name <- tryCatch(fit$family$family, error = function(e) "unknown")

  # Format coefficients into a readable block for the prompt
  coef_lines <- paste(
    apply(coef_df, 1, function(r)
      sprintf("  %s:  estimate = %s,  SE = %s",
              r["term"], r["estimate"], r["se"])
    ),
    collapse = "\n"
  )

  context_line <- if (!is.null(context)) {
    paste0("Study context provided by the researcher: ", context, "\n\n")
  } else {
    ""
  }

  system_msg <- paste0(
    "You are a statistician explaining regression results to a researcher. ",
    "Your job is to interpret the provided coefficients in plain language. ",
    "Rules you must follow: ",
    "(1) Only reference the exact numbers given — never invent or round differently. ",
    "(2) Do not claim causation unless the researcher's study context supports it. ",
    "(3) Note which coefficients appear statistically significant using the ",
    "    rule of thumb |estimate / SE| > 2. ",
    "(4) Keep the response to 3-5 sentences. ",
    "(5) Write for a researcher audience — assume familiarity with regression."
  )

  user_msg <- paste0(
    context_line,
    "Model type: ", family_name, " regression\n\n",
    "Coefficients and standard errors:\n",
    coef_lines, "\n\n",
    "Please interpret these results in plain language."
  )

  message("Querying Anthropic API...")
  explanation <- .call_claude(user_msg, system_msg)

  if (verbose) {
    cat("\n--- postlink: explain_fit() ---\n\n")
    cat(explanation)
    cat("\n\n[AI-generated interpretation. Verify before use.]\n")
  }

  invisible(explanation)
}
