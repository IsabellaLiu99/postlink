# ── Option 1: RAG Documentation Agent ────────────────────────────────────────
#
# Pure-R implementation:
#   1. .build_postlink_index()  — parse .Rd + vignette → chunks → embed → .rds
#   2. .search_index()          — cosine similarity retrieval
#   3. ask_postlink()           — user-facing Q&A function
#
# Embeddings: OpenAI text-embedding-3-small (or any provider's embedding API)
# Storage:    inst/rag/postlink_index.rds  (shipped with package)
#             Falls back to ~/.postlink_rag_cache.rds for user-built indices
# -----------------------------------------------------------------------------


# ── Cosine similarity ─────────────────────────────────────────────────────────
.cosine_sim <- function(a, b) {
  sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)) + 1e-10)
}


# ── Strip Rd markup → plain text ──────────────────────────────────────────────
.rd_to_text <- function(rd_path) {
  lines <- readLines(rd_path, warn = FALSE, encoding = "UTF-8")

  # Drop comment lines
  lines <- lines[!grepl("^%", lines)]

  # Remove common Rd macros, keep content
  text <- paste(lines, collapse = " ")
  text <- gsub("\\\\[a-zA-Z]+\\{([^}]*)\\}", "\\1", text)  # \cmd{text} → text
  text <- gsub("\\\\[a-zA-Z]+", " ", text)                  # bare \cmd → space
  text <- gsub("[{}]", " ", text)
  text <- gsub("\\s+", " ", text)
  trimws(text)
}


# ── Split text into overlapping chunks ───────────────────────────────────────
.chunk_text <- function(text, chunk_size = 400, overlap = 80) {
  words <- strsplit(text, "\\s+")[[1]]
  if (length(words) == 0) return(character(0))

  chunks <- character(0)
  start  <- 1L
  while (start <= length(words)) {
    end <- min(start + chunk_size - 1L, length(words))
    chunks <- c(chunks, paste(words[start:end], collapse = " "))
    if (end == length(words)) break
    start <- start + chunk_size - overlap
  }
  chunks
}


# ── Embed a batch of texts via OpenAI ────────────────────────────────────────
.embed_texts <- function(texts, api_key,
                         model = "text-embedding-3-small") {
  if (!requireNamespace("httr2", quietly = TRUE))
    stop("httr2 is required for embedding. Install it with install.packages('httr2').")

  # OpenAI allows up to 2048 inputs per request; batch in 100s to be safe
  batch_size <- 100L
  all_embeddings <- vector("list", length(texts))

  for (i in seq(1, length(texts), by = batch_size)) {
    batch <- texts[i:min(i + batch_size - 1L, length(texts))]

    resp <- httr2::request("https://api.openai.com/v1/embeddings") |>
      httr2::req_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ) |>
      httr2::req_body_json(list(input = batch, model = model),
                           auto_unbox = FALSE) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) != 200L) {
      stop("Embedding API error (", httr2::resp_status(resp), "): ",
           httr2::resp_body_string(resp))
    }

    parsed <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    for (j in seq_along(parsed$data)) {
      all_embeddings[[i + j - 1L]] <- unlist(parsed$data[[j]]$embedding)
    }
  }

  all_embeddings
}


# ── Parse all .Rd files + vignette into a named list of text chunks ──────────
.collect_docs <- function(pkg_path = NULL) {
  if (is.null(pkg_path)) {
    # Try installed package location first, then development tree
    pkg_path <- tryCatch(
      system.file(package = "postlink"),
      error = function(e) NULL
    )
    if (is.null(pkg_path) || pkg_path == "")
      pkg_path <- file.path(getwd())
  }

  docs <- list()

  # ── .Rd man pages ──────────────────────────────────────────────────────────
  man_dir <- file.path(pkg_path, "man")
  if (!dir.exists(man_dir)) man_dir <- file.path(pkg_path, "help")

  if (dir.exists(man_dir)) {
    rd_files <- list.files(man_dir, pattern = "\\.Rd$",
                           full.names = TRUE, recursive = FALSE)
    for (f in rd_files) {
      nm   <- tools::file_path_sans_ext(basename(f))
      text <- tryCatch(.rd_to_text(f), error = function(e) "")
      if (nchar(trimws(text)) < 20) next
      chunks <- .chunk_text(text)
      for (k in seq_along(chunks)) {
        docs[[paste0("man:", nm, ":", k)]] <- list(
          source = paste0("?", nm),
          text   = chunks[[k]]
        )
      }
    }
  }

  # ── Vignette(s) ────────────────────────────────────────────────────────────
  vig_dirs <- c(
    file.path(pkg_path, "vignettes"),
    file.path(pkg_path, "doc")
  )
  for (vig_dir in vig_dirs) {
    if (!dir.exists(vig_dir)) next
    rmd_files <- list.files(vig_dir, pattern = "\\.(Rmd|rmd|qmd)$",
                            full.names = TRUE)
    for (f in rmd_files) {
      nm    <- tools::file_path_sans_ext(basename(f))
      lines <- tryCatch(readLines(f, warn = FALSE, encoding = "UTF-8"),
                        error = function(e) character(0))
      # Strip YAML front matter and code chunks
      lines <- lines[!grepl("^```", lines)]
      text  <- paste(lines, collapse = " ")
      text  <- gsub("---.*?---", " ", text)          # YAML
      text  <- gsub("`[^`]+`", " ", text)            # inline code
      text  <- gsub("\\s+", " ", trimws(text))
      chunks <- .chunk_text(text)
      for (k in seq_along(chunks)) {
        docs[[paste0("vignette:", nm, ":", k)]] <- list(
          source = paste0("vignette('", nm, "')"),
          text   = chunks[[k]]
        )
      }
    }
  }

  docs
}


# ── Build and save the RAG index ─────────────────────────────────────────────

#' Build the postlink RAG documentation index
#'
#' Parses all man pages and vignettes, embeds them using the OpenAI
#' \code{text-embedding-3-small} model, and saves the index to disk.
#' Run this once; the index is then reused by \code{\link{ask_postlink}}.
#'
#' @param api_key OpenAI API key. Defaults to \code{OPENAI_API_KEY} env var.
#' @param pkg_path Path to the postlink package source. Defaults to the
#'   installed package location.
#' @param out_file Path to save the index \code{.rds} file. Defaults to
#'   \code{~/.postlink_rag_index.rds}.
#' @param embed_model OpenAI embedding model name.
#'
#' @return Invisibly returns the path to the saved index.
#' @export
build_postlink_index <- function(
    api_key    = Sys.getenv("OPENAI_API_KEY"),
    pkg_path   = NULL,
    out_file   = path.expand("~/.postlink_rag_index.rds"),
    embed_model = "text-embedding-3-small") {

  if (nchar(api_key) == 0)
    stop("OpenAI API key required. Set OPENAI_API_KEY or pass api_key=.")

  message("Collecting documentation...")
  docs <- .collect_docs(pkg_path)
  message("  Found ", length(docs), " chunks from ",
          length(unique(sub(":.*", "", names(docs)))), " sources.")

  texts <- vapply(docs, `[[`, character(1), "text")

  message("Embedding ", length(texts), " chunks (this may take ~1 min)...")
  embeddings <- .embed_texts(texts, api_key, embed_model)

  index <- list(
    chunks     = docs,
    embeddings = embeddings,
    model      = embed_model,
    built_at   = Sys.time()
  )

  saveRDS(index, out_file)
  message("Index saved to: ", out_file)
  invisible(out_file)
}


# ── Load index (cached in session) ───────────────────────────────────────────
.rag_env <- new.env(parent = emptyenv())

.load_index <- function(index_file = NULL) {
  if (!is.null(.rag_env$index)) return(.rag_env$index)

  candidates <- c(
    index_file,
    path.expand("~/.postlink_rag_index.rds"),
    system.file("rag", "postlink_index.rds", package = "postlink")
  )
  candidates <- candidates[!is.null(candidates) & nchar(candidates) > 0]

  for (f in candidates) {
    if (file.exists(f)) {
      .rag_env$index <- readRDS(f)
      return(.rag_env$index)
    }
  }

  stop(
    "No RAG index found. Run build_postlink_index() first to create one.\n",
    "Example: build_postlink_index(api_key = Sys.getenv('OPENAI_API_KEY'))"
  )
}


# ── Retrieve top-k chunks ─────────────────────────────────────────────────────
.search_index <- function(query_embedding, index, top_k = 5L) {
  sims <- vapply(index$embeddings,
                 function(e) .cosine_sim(query_embedding, e),
                 numeric(1))
  top_idx <- order(sims, decreasing = TRUE)[seq_len(min(top_k, length(sims)))]
  list(
    chunks = index$chunks[top_idx],
    scores = sims[top_idx]
  )
}


# ── User-facing Q&A function ─────────────────────────────────────────────────

#' Ask a question about the postlink package
#'
#' Uses retrieval-augmented generation (RAG) to answer questions about
#' postlink's functions, methods, and statistical concepts. Retrieves the
#' most relevant documentation chunks and passes them to an LLM for a
#' grounded, citation-aware answer.
#'
#' @param question A character string with your question.
#' @param provider LLM provider: \code{"openai"} (default), \code{"anthropic"},
#'   \code{"gemini"}, or \code{"ollama"}.
#' @param api_key API key for the LLM provider. Defaults to the standard
#'   environment variable for the chosen provider.
#' @param model LLM model name. Defaults to provider's recommended model.
#' @param embed_api_key OpenAI API key for embedding the query (always uses
#'   OpenAI embeddings regardless of LLM provider). Defaults to
#'   \code{OPENAI_API_KEY}.
#' @param index_file Path to a pre-built RAG index \code{.rds} file. If
#'   \code{NULL}, looks in the default location
#'   (\code{~/.postlink_rag_index.rds}) or the package's bundled index.
#' @param top_k Number of documentation chunks to retrieve (default 5).
#' @param verbose If \code{TRUE}, prints retrieved sources before the answer.
#'
#' @return Invisibly returns the answer string; also prints it to the console.
#'
#' @examples
#' \dontrun{
#' # Build index once
#' build_postlink_index(api_key = Sys.getenv("OPENAI_API_KEY"))
#'
#' # Ask questions
#' ask_postlink("What does the m.rate argument do in adjMixture?")
#' ask_postlink("When should I use adjELE instead of adjMixture?")
#' ask_postlink("How do I interpret the output of explain()?",
#'              provider = "anthropic",
#'              api_key  = Sys.getenv("ANTHROPIC_API_KEY"))
#' }
#' @export
ask_postlink <- function(
    question,
    provider      = "openai",
    api_key       = NULL,
    model         = NULL,
    embed_api_key = Sys.getenv("OPENAI_API_KEY"),
    index_file    = NULL,
    top_k         = 5L,
    verbose       = FALSE) {

  if (!is.character(question) || length(question) != 1 || nchar(trimws(question)) == 0)
    stop("'question' must be a non-empty character string.")

  if (nchar(embed_api_key) == 0)
    stop("OpenAI API key required for query embedding. ",
         "Set OPENAI_API_KEY or pass embed_api_key=.")

  # Resolve LLM api key
  api_key <- .resolve_key(api_key, provider)

  # Load index
  index <- .load_index(index_file)

  # Embed the query
  q_emb <- .embed_texts(list(question), embed_api_key, index$model)[[1]]

  # Retrieve top-k chunks
  retrieved <- .search_index(q_emb, index, top_k)

  # Format context block
  context_parts <- mapply(function(chunk, score) {
    sprintf("[Source: %s (relevance: %.2f)]\n%s",
            chunk$source, score, chunk$text)
  }, retrieved$chunks, retrieved$scores, SIMPLIFY = TRUE)

  context_block <- paste(context_parts, collapse = "\n\n---\n\n")

  if (verbose) {
    cat("Retrieved sources:\n")
    for (ch in retrieved$chunks) cat(" -", ch$source, "\n")
    cat("\n")
  }

  # Build prompt
  system_prompt <- paste0(
    "You are an expert assistant for the postlink R package, which implements ",
    "linkage-error-corrected regression methods for probabilistically linked data. ",
    "Answer the user's question using ONLY the documentation excerpts provided. ",
    "If the excerpts do not contain enough information, say so clearly. ",
    "Be concise and practical. When referencing functions, use R syntax (e.g. adjMixture()). ",
    "If relevant, mention which function argument or vignette section to consult."
  )

  user_prompt <- sprintf(
    "Documentation excerpts:\n\n%s\n\n---\n\nQuestion: %s",
    context_block, question
  )

  # Call LLM
  result <- .call_llm(
    system_prompt = system_prompt,
    user_prompt   = user_prompt,
    provider      = provider,
    api_key       = api_key,
    model         = model
  )

  answer <- result$text

  cat("\n", answer, "\n\n", sep = "")

  if (length(retrieved$chunks) > 0) {
    sources <- unique(vapply(retrieved$chunks, `[[`, character(1), "source"))
    cat("Sources consulted: ", paste(sources, collapse = ", "), "\n", sep = "")
  }

  invisible(answer)
}
