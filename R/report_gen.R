# =============================================================================
# report_gen.R
# Option 8: generate_report()
#
# Report sections:
#   1. Data          — from fit + optional user context
#   2. Methods       — adjustment method + mismatch model coefficient table
#   3. Results       — regression coefficient table (no AI) + AI summary
#   4. References    — auto-populated based on method used
#
# Output formats: "rmd" (default), "qmd", "html", "text"
# Requires: httr2 (optional dependency)
# API Key:  Set ANTHROPIC_API_KEY in your .Renviron
# =============================================================================


#' Generate an Analysis Report from a Fitted postlink Model
#'
#' Extracts numerical results from a fitted postlink object and produces a
#' structured analysis report. Sections: Data, Methods (with mismatch model
#' table), Results (with regression coefficient table), and References.
#'
#' An LLM (Anthropic Claude) is used only to write the plain-language
#' narrative in the Results section, grounded entirely in the actual numbers
#' from the model. All tables and statistics come directly from the model.
#'
#' @param fit A fitted postlink object (e.g. from \code{\link{plglm}},
#'   \code{\link{plcoxph}}).
#' @param output_file File path for the output including extension
#'   (e.g. \code{"report.Rmd"}, \code{"report.html"}).
#'   If \code{NULL}, defaults to \code{"postlink_report.Rmd"} in the
#'   working directory.
#' @param output_format One of \code{"rmd"} (default), \code{"qmd"},
#'   \code{"html"}, or \code{"text"} (prints to console, no file written).
#' @param context An optional named list providing study-level context.
#'   If \code{NULL} (default), the report uses only information from
#'   \code{fit}. Providing context produces a richer Data section and
#'   more relevant AI summary.
#'
#'   Recognised fields (all optional):
#'   \describe{
#'     \item{\code{study}}{One or two sentences describing the overall study.}
#'     \item{\code{dataset}}{Name or description of the dataset.}
#'     \item{\code{n_total}}{Total number of records (numeric).}
#'     \item{\code{outcome_label}}{Human-readable name for the outcome variable.}
#'     \item{\code{predictor_label}}{Human-readable name for the main predictor.}
#'     \item{\code{linkage_description}}{One sentence describing how data were linked.}
#'   }
#' @param render Logical. If \code{TRUE} and format is \code{"rmd"} or
#'   \code{"qmd"}, render after writing. Default \code{FALSE}.
#'
#' @return Invisibly returns the file path, or the report text if
#'   \code{output_format = "text"}.
#'
#' @details
#' The AI receives only coefficient names, estimates, and standard errors.
#' It is instructed not to invent numbers or make unsupported claims.
#' Always review the generated report before submitting or publishing.
#'
#' Requires \code{httr2} and \code{ANTHROPIC_API_KEY} in \code{.Renviron}.
#'
#' @examples
#' \dontrun{
#' data(lifem)
#' adj <- adjMixture(linked.data = lifem, m.formula = ~ commf + comml,
#'                   m.rate = 0.05, safe.matches = hndlnk)
#' fit <- plglm(age_at_death ~ poly(unit_yob, 3, raw = TRUE),
#'              family = "gaussian", adjustment = adj)
#'
#' # Minimal — no context needed
#' generate_report(fit)
#'
#' # With context for a richer report
#' generate_report(
#'   fit,
#'   output_file = "lifem_report.Rmd",
#'   context = list(
#'     study            = "This study examines year of birth and age at death.",
#'     dataset          = "LIFE-M Ohio birth and death certificates, 1883-1906",
#'     n_total          = 156453,
#'     outcome_label    = "age at death (years)",
#'     predictor_label  = "year of birth",
#'     linkage_description = "Records linked using name and date of birth;
#'                            ~5% mismatch rate expected."
#'   )
#' )
#'
#' # Render immediately
#' generate_report(fit, output_file = "report.Rmd", render = TRUE)
#'
#' # Other formats
#' generate_report(fit, output_format = "qmd",  output_file = "report.qmd")
#' generate_report(fit, output_format = "html", output_file = "report.html")
#' generate_report(fit, output_format = "text")
#' }
#'
#' @export
generate_report <- function(fit,
                             output_file   = NULL,
                             output_format = c("rmd", "qmd", "html", "text"),
                             context       = NULL,
                             render        = FALSE) {

  output_format <- match.arg(output_format)

  if (!is.null(context) && !is.list(context)) {
    stop("'context' must be a named list or NULL. See ?generate_report.", call. = FALSE)
  }

  info        <- .extract_fit_info(fit, context)
  message("Querying Anthropic API for plain-language summary...")
  ai_summary  <- .summarise_with_ai(info, context)
  report_text <- .build_report(info, ai_summary, output_format)

  if (output_format == "text") {
    cat(report_text)
    return(invisible(report_text))
  }

  if (is.null(output_file)) {
    output_file <- switch(output_format,
      rmd  = "postlink_report.Rmd",
      qmd  = "postlink_report.qmd",
      html = "postlink_report.html"
    )
  }

  out_dir <- dirname(output_file)
  if (!dir.exists(out_dir) && out_dir != ".") dir.create(out_dir, recursive = TRUE)
  writeLines(report_text, output_file)
  cat("\nReport written to:", normalizePath(output_file, mustWork = FALSE), "\n")

  if (render) {
    if (output_format == "rmd") {
      if (!requireNamespace("rmarkdown", quietly = TRUE)) {
        warning("'rmarkdown' not installed. Skipping render.", call. = FALSE)
      } else {
        message("Rendering...")
        rmarkdown::render(output_file, quiet = TRUE)
        cat("Rendered successfully.\n")
      }
    } else if (output_format == "qmd") {
      if (nchar(Sys.which("quarto")) == 0) {
        warning("Quarto CLI not found. Install from https://quarto.org", call. = FALSE)
      } else {
        message("Rendering...")
        system2("quarto", c("render", output_file))
      }
    }
  }

  cat("[Review all AI-generated text before submitting or publishing.]\n")
  invisible(output_file)
}


# ── Null-coalescing helper ────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a)) a else b


# ── Internal: extract info from fit ──────────────────────────────────────────
.extract_fit_info <- function(fit, context = NULL) {

  method_label <- if (inherits(fit, c("glmMixture","coxphMixture","ctableMixture"))) {
    "mixture modeling with the EM algorithm (Slawski et al., 2025)"
  } else if (inherits(fit, c("glmELE","coxphELE"))) {
    "weighting under the Exchangeable Linkage Error model (Chambers, 2009)"
  } else if (inherits(fit, c("glmMixBayes","survregMixBayes"))) {
    "Bayesian mixture modeling (Gutman et al., 2016)"
  } else "postlink adjustment"

  ref <- if (inherits(fit, c("glmMixture","coxphMixture","ctableMixture"))) {
    paste0("Slawski, M., West, B. T., Bukke, P., Wang, Z., Diao, G., & Ben-David, E. (2025). ",
           "A general framework for regression with mismatched data based on mixture modelling. ",
           "*Journal of the Royal Statistical Society Series A*, 188(3), 896-919.")
  } else if (inherits(fit, c("glmELE","coxphELE"))) {
    paste0("Chambers, R. (2009). Regression analysis of probability-linked data. ",
           "*Official Statistics Research Series*, 4, 1-15.")
  } else {
    paste0("Gutman, R., Sammartino, C., Green, T., & Montague, B. (2016). ",
           "Error adjustments for file linking methods. *Statistics in Medicine*, 35(1), 115-129.")
  }

  family_name  <- tryCatch(fit$family$family, error = function(e) "unknown")
  family_label <- switch(family_name,
    gaussian = "Gaussian (linear) regression",
    binomial = "logistic regression",
    poisson  = "Poisson regression",
    Gamma    = "Gamma regression",
    paste0(family_name, " regression")
  )

  outcome_label   <- context$outcome_label %||%
    tryCatch(as.character(fit$call$formula[[2]]), error = function(e) "outcome")
  predictor_label <- context$predictor_label %||% NULL
  formula_str     <- tryCatch(deparse(fit$call$formula), error = function(e) "")

  # ── Coefficient table: only "coef " rows ──────────────────────────────────
  coef_df <- .get_coef_df(fit)
  coef_df$z_stat  <- round(coef_df$estimate / coef_df$se, 3)
  coef_df$p_value <- round(2 * pnorm(-abs(coef_df$z_stat)), 4)

  ci <- tryCatch(confint(fit), error = function(e) NULL)
  mcoef_df <- NULL

  if (!is.null(ci)) {
    # Regression coefficients CI — rows starting with "coef "
    coef_rows <- grepl("^coef ", rownames(ci))
    if (any(coef_rows)) {
      ci_coef <- ci[coef_rows, , drop = FALSE]
    } else {
      ci_coef <- ci[seq_len(nrow(coef_df)), , drop = FALSE]
    }
    coef_df$ci_lower <- round(ci_coef[, 1], 4)
    coef_df$ci_upper <- round(ci_coef[, 2], 4)

    # Mismatch model coefficients CI — rows starting with "m.coef "
    mcoef_rows <- grepl("^m\\.coef ", rownames(ci))
    if (any(mcoef_rows)) {
      ci_m <- ci[mcoef_rows, , drop = FALSE]
      mcoef_df <- data.frame(
        term     = sub("^m\\.coef ", "", rownames(ci_m)),
        ci_lower = round(ci_m[, 1], 4),
        ci_upper = round(ci_m[, 2], 4),
        row.names = NULL,
        stringsAsFactors = FALSE
      )
    }
  }

  n_fit   <- tryCatch(nrow(fit$model), error = function(e) NA_integer_)
  n_total <- context$n_total %||% n_fit

  match_summary <- NULL
  if (!is.null(fit$match.prob)) {
    mp <- fit$match.prob
    match_summary <- list(
      mean          = round(mean(mp), 3),
      pct_above_90  = round(100 * mean(mp > 0.9), 1),
      implied_mrate = round(100 * (1 - mean(mp)), 1)
    )
  }

  list(
    method_label    = method_label,
    ref             = ref,
    family_name     = family_name,
    family_label    = family_label,
    outcome_label   = outcome_label,
    predictor_label = predictor_label,
    formula_str     = formula_str,
    coef_df         = coef_df,
    mcoef_df        = mcoef_df,
    n_fit           = n_fit,
    n_total         = n_total,
    converged       = fit$converged,
    match_summary   = match_summary,
    has_context     = !is.null(context)
  )
}


# ── Internal: AI plain-language summary ──────────────────────────────────────
.summarise_with_ai <- function(info, context = NULL) {

  coef_lines <- paste(apply(info$coef_df, 1, function(r) {
    ci_part <- if (all(c("ci_lower","ci_upper") %in% names(r)))
      sprintf(", 95%% CI [%s, %s]", r["ci_lower"], r["ci_upper"]) else ""
    sprintf("  %s:  estimate = %s,  SE = %s,  p = %s%s",
            r["term"], r["estimate"], r["se"], r["p_value"], ci_part)
  }), collapse = "\n")

  ctx_block <- ""
  if (info$has_context) {
    parts <- c(
      if (!is.null(context$study))               paste0("Study: ",    context$study),
      if (!is.null(context$dataset))             paste0("Dataset: ",  context$dataset),
      if (!is.null(context$linkage_description)) paste0("Linkage: ",  context$linkage_description)
    )
    if (length(parts) > 0) ctx_block <- paste0(paste(parts, collapse = "\n"), "\n\n")
  }

  system_msg <- paste0(
    "You are a statistician writing the Results section of an academic paper. ",
    "You are given exact numerical results from a post-linkage data analysis. ",
    "Rules: (1) Only reference the exact numbers provided — never invent values. ",
    "(2) Do not claim causation unless the study context explicitly supports it. ",
    "(3) Note statistical significance using p < 0.05. ",
    "(4) Write 3-5 sentences in formal academic style. ",
    "(5) Do not add a disclaimer — that is handled separately.",
    "(6) Do not include any headers or section titles — only write the paragraph text."
  )

  user_msg <- paste0(
    ctx_block,
    "Model: ", info$family_label, "\n",
    "Outcome: ", info$outcome_label, "\n",
    "Linkage error adjustment: ", info$method_label, "\n",
    if (!is.na(info$n_fit)) paste0("Sample size: ", format(info$n_fit, big.mark=","), "\n") else "",
    if (!is.null(info$match_summary))
      paste0("Mean posterior match probability: ", info$match_summary$mean,
             " (implied mismatch rate ~", info$match_summary$implied_mrate, "%)\n") else "",
    "\nCoefficients:\n", coef_lines, "\n\n",
    "Write a 3-5 sentence Results summary for an academic paper."
  )

  .call_claude(user_msg, system_msg, max_tokens = 400)
}


# ── Internal: build document ──────────────────────────────────────────────────
.build_report <- function(info, ai_summary, output_format) {

  date_str <- format(Sys.Date(), "%B %d, %Y")
  is_md    <- output_format %in% c("rmd", "qmd", "text")

  header <- switch(output_format,
    rmd = paste0(
      "---\ntitle: \"postlink Analysis Report\"\ndate: \"", date_str, "\"\n",
      "output:\n  html_document:\n    toc: true\n    toc_float: true\n",
      "  pdf_document:\n    toc: true\n---\n\n"
    ),
    qmd = paste0(
      "---\ntitle: \"postlink Analysis Report\"\ndate: \"", date_str, "\"\n",
      "format:\n  html:\n    toc: true\n  pdf:\n    toc: true\n---\n\n"
    ),
    html = paste0(
      "<!DOCTYPE html><html><head><title>postlink Analysis Report</title><style>",
      "body{font-family:Georgia,serif;max-width:820px;margin:2.5em auto;line-height:1.7;color:#222}",
      "h1{font-size:1.8em;border-bottom:2px solid #333;padding-bottom:.3em}",
      "h2{font-size:1.3em;color:#444;margin-top:2em}",
      "h3{font-size:1.1em;color:#555;margin-top:1.5em}",
      "table{border-collapse:collapse;width:100%;margin:1em 0}",
      "th,td{border:1px solid #ccc;padding:8px 12px;text-align:left}",
      "th{background:#f5f5f5}",
      "blockquote{border-left:4px solid #f0ad4e;padding:.5em 1em;background:#fffbe6;color:#555}",
      "code{background:#f4f4f4;padding:2px 5px;border-radius:3px}",
      "</style></head><body>\n<h1>postlink Analysis Report</h1>\n",
      "<p><em>", date_str, "</em></p>\n\n"
    ),
    text = paste0("postlink Analysis Report\n", strrep("=", 40), "\n", date_str, "\n\n")
  )

  # ── Format helpers ──
  sec    <- function(title, body) if (is_md) paste0("## ", title, "\n\n", body, "\n")
                                  else paste0("<h2>", title, "</h2>\n\n", body, "\n")
  subsec <- function(title)       if (is_md) paste0("### ", title, "\n\n")
                                  else paste0("<h3>", title, "</h3>\n")
  bold   <- function(x) if (is_md) paste0("**", x, "**") else paste0("<strong>", x, "</strong>")
  code   <- function(x) if (is_md) paste0("`",  x, "`")  else paste0("<code>",   x, "</code>")
  note   <- function(x) if (is_md) paste0("> ", x)
                        else paste0("<blockquote>", x, "</blockquote>")
  ref    <- function(x) if (is_md) paste0("- ", x) else paste0("<p>", x, "</p>")

  # ── Data section ──
  n_str <- if (!is.na(info$n_total))
    paste0(format(info$n_total, big.mark = ","), " records") else "N not available"

  if (info$has_context) {
    data_body <- paste0(
      if (!is.na(info$n_total))
        paste0("The linked dataset contains ", bold(n_str), ". ") else "",
      if (!is.null(info$predictor_label))
        paste0("The outcome variable is ", bold(info$outcome_label),
               " and the main predictor is ", bold(info$predictor_label), ". ")
      else
        paste0("The outcome variable is ", bold(info$outcome_label), ". ")
    )
  } else {
    data_body <- paste0(
      "The analysis used ", n_str, ". ",
      "The outcome variable is ", bold(info$outcome_label), ". ",
      "Model formula: ", code(info$formula_str), "."
    )
  }

  # ── Methods section ──
  match_note <- if (!is.null(info$match_summary))
    paste0(
      " The mean posterior correct match probability was ", info$match_summary$mean,
      " (", info$match_summary$pct_above_90, "% of records > 0.9),",
      " implying an estimated mismatch rate of approximately ",
      info$match_summary$implied_mrate, "%."
    ) else ""

  conv_note <- if (!is.null(info$converged))
    if (isTRUE(info$converged)) " The EM algorithm converged."
    else " Warning: the EM algorithm did not converge. Results may be unreliable."
  else ""

  # Mismatch model coefficient table (in Methods)
  mcoef_block <- ""
  if (!is.null(info$mcoef_df)) {
    mcoef_block <- paste0(
      "\n\n",
      subsec("Mismatch Model Coefficients"),
      "The table below shows the 95% confidence intervals for the mismatch model ",
      "coefficients. These describe how the linkage quality variables (e.g. name ",
      "commonness scores) predict the probability that a record is a correct match. ",
      "They are reported here as supplementary information about the adjustment ",
      "procedure and are not the primary focus of the analysis.\n\n",
      .make_mcoef_table(info$mcoef_df, output_format)
    )
  }

  methods_body <- paste0(
    "Linkage error was accounted for using ", bold("postlink"), " (R package, v0.1.0), ",
    "applying ", info$method_label, ". ",
    "The outcome (", info$outcome_label, ") was modelled using ", info$family_label,
    " with the formula ", code(info$formula_str), ".",
    match_note, conv_note,
    mcoef_block
  )

  # ── Results section ──
  disclaimer <- note(paste0(
    "The narrative below was drafted by an AI language model using the exact ",
    "numerical outputs from the model. Review and edit before submitting or publishing."
  ))
  coef_tbl <- .make_coef_table(info$coef_df, output_format)
  sig_note  <- if (is_md) "_\\* p < 0.05_\n" else "<p><em>* p &lt; 0.05</em></p>"

  results_body <- paste0(
    disclaimer, "\n\n",
    ai_summary, "\n\n",
    subsec("Regression Coefficient Table"),
    coef_tbl, "\n\n", sig_note
  )

  # ── References section ──
  refs_body <- paste0(
    ref(paste0(
      "postlink R package (v0.1.0). Bukke, P., Kamat, G., Cui, J., Gutman, R., & Slawski, M. ",
      "https://postlink-group.github.io/postlink/"
    )),
    "\n",
    ref(info$ref)
  )

  footer <- if (output_format == "html") "\n</body></html>" else ""

  paste0(
    header,
    sec("Data",       data_body),
    sec("Methods",    methods_body),
    sec("Results",    results_body),
    sec("References", refs_body),
    footer
  )
}


# ── Regression coefficient table ─────────────────────────────────────────────
.make_coef_table <- function(coef_df, output_format) {

  has_ci <- all(c("ci_lower","ci_upper") %in% names(coef_df))

  if (output_format == "html") {
    th <- function(x) paste0("<th>", x, "</th>")
    td <- function(x) paste0("<td>", x, "</td>")
    hcells <- paste0(
      th("Term"), th("Estimate"), th("SE"), th("z"), th("p-value"),
      if (has_ci) paste0(th("95% CI Lower"), th("95% CI Upper")) else ""
    )
    rows <- paste(apply(coef_df, 1, function(r) {
      sig <- if (!is.na(as.numeric(r["p_value"])) && as.numeric(r["p_value"]) < 0.05) " *" else ""
      paste0("<tr>",
        td(r["term"]), td(r["estimate"]), td(r["se"]), td(r["z_stat"]),
        td(paste0(r["p_value"], sig)),
        if (has_ci) paste0(td(r["ci_lower"]), td(r["ci_upper"])) else "",
        "</tr>")
    }), collapse = "\n")
    paste0("<table>\n<thead><tr>", hcells, "</tr></thead>\n<tbody>\n", rows, "\n</tbody>\n</table>")
  } else {
    if (has_ci) {
      hdr <- "| Term | Estimate | SE | z | p-value | 95% CI Lower | 95% CI Upper |"
      sep <- "|------|----------|----|---|---------|--------------|--------------|"
    } else {
      hdr <- "| Term | Estimate | SE | z | p-value |"
      sep <- "|------|----------|----|---|---------|"
    }
    rows <- apply(coef_df, 1, function(r) {
      sig <- if (!is.na(as.numeric(r["p_value"])) && as.numeric(r["p_value"]) < 0.05) " *" else ""
      if (has_ci)
        sprintf("| %s | %s | %s | %s | %s%s | %s | %s |",
                r["term"], r["estimate"], r["se"], r["z_stat"],
                r["p_value"], sig, r["ci_lower"], r["ci_upper"])
      else
        sprintf("| %s | %s | %s | %s | %s%s |",
                r["term"], r["estimate"], r["se"], r["z_stat"], r["p_value"], sig)
    })
    paste(c(hdr, sep, rows), collapse = "\n")
  }
}


# ── Mismatch model coefficient table ─────────────────────────────────────────
.make_mcoef_table <- function(mcoef_df, output_format) {

  if (output_format == "html") {
    th <- function(x) paste0("<th>", x, "</th>")
    td <- function(x) paste0("<td>", x, "</td>")
    hcells <- paste0(th("Term"), th("95% CI Lower"), th("95% CI Upper"))
    rows <- paste(apply(mcoef_df, 1, function(r) {
      paste0("<tr>", td(r["term"]), td(r["ci_lower"]), td(r["ci_upper"]), "</tr>")
    }), collapse = "\n")
    paste0("<table>\n<thead><tr>", hcells, "</tr></thead>\n<tbody>\n", rows, "\n</tbody>\n</table>")
  } else {
    hdr  <- "| Term | 95% CI Lower | 95% CI Upper |"
    sep  <- "|------|--------------|--------------|"
    rows <- apply(mcoef_df, 1, function(r)
      sprintf("| %s | %s | %s |", r["term"], r["ci_lower"], r["ci_upper"]))
    paste(c(hdr, sep, rows), collapse = "\n")
  }
}
