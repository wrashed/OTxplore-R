# R/get_data.R
# =========================================================
# Open Targets GraphQL data access module for OTxplorer-R
# =========================================================
# Objective:
#   Implement functions to fetch, parse, and structure data
#   via the Open Targets GraphQL API for downstream visualization.
#
# API endpoint (v4 GraphQL):
#   https://api.platform.opentargets.org/api/v4/graphql
#
# Schema reference:
#   See "target", "associatedDiseases", "knownDrugs", "disease", "evidences"
#   objects/fields in Open Targets GraphQL schema.
#
# Dependencies:
#   - Feeds data into plot_graph.R visualization
#   - Triggered by Shiny server on user input
#
# =========================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(tidyverse)
})

BASE_URL <- "https://api.platform.opentargets.org/api/v4/graphql"

# -----------------------------
# Helpers: HTTP + JSON handling
# -----------------------------
# Robust GraphQL POST that:
# - auto-unboxes scalar variables
# - encodes NULL as proper JSON null (prevents cursor = {} errors)
# - surfaces HTTP errors verbosely

post_gql <- function(query, variables, log = TRUE) {
  # body_json is the query that will be sent to the API
  # jasonlite trun an R list into the JSON object
  body_json <- jsonlite::toJSON(
    list(query = query, variables = variables),
    auto_unbox = TRUE, # keep single strings as "text" instead of ["text"]
    null = "null" # turns missing values into null in JSON
  )
  if (isTRUE(log)) message("POST ", BASE_URL, "  [vars: ", paste(names(variables), collapse = ","), "]")
  
  # httr::POST sends the JSON text to the API's URL
  # res is the reply message from the Open Targets server
  res <- httr::POST(
    url = BASE_URL,
    body = body_json,
    httr::add_headers(`Content-Type` = "application/json")
  )
  
  # txt is the raw text content of the reply from the the server
  txt <- httr::content(res, as = "text", encoding = "UTF-8")
  if (httr::http_error(res)) {
    stop(sprintf("HTTP %s: %s", httr::status_code(res), txt), call. = FALSE)
  }
  
  # Make the txt into R data structure
  jsonlite::fromJSON(txt, flatten = TRUE)
}

# handy default-if-NULL operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# -----------------------------
# GraphQL queries
# -----------------------------

# Find the diseases linked to the gene
q_associations <- "
query targetAssociations($ensemblId: String!, $size: Int!, $index: Int!) {
  target(ensemblId: $ensemblId) {
    id
    approvedSymbol
    associatedDiseases(page: {size: $size, index: $index}) {
      count 
      rows {
        disease { id name }
        score
        datasourceScores { id score }
        datatypeScores   { id score }
      }
    }
  }
}
"
# count: the total number of associated diseases
# rows: the actual list of association entries

# Find the drugs targeting the gene
q_known_drugs <- "
query targetKnownDrugs($ensemblId: String!, $size: Int!, $cursor: String) {
  target(ensemblId: $ensemblId) {
    id
    approvedSymbol
    knownDrugs(size: $size, cursor: $cursor) {
      count
      uniqueDrugs
      uniqueDiseases
      uniqueTargets
      cursor
      rows {
        drug    { id name }
        disease { id name }
        target  { id approvedSymbol }
        mechanismOfAction
        phase
        status
        drugType
      }
    }
  }
}
"

# Find the evidence why the gene-disease link exists
q_evidence_td <- "
query diseaseEvidenceForTarget($efoId: String!, $ensemblId: String!, $size: Int, $cursor: String) {
  disease(efoId: $efoId) {
    id
    name
    evidences(ensemblIds: [$ensemblId], size: $size, cursor: $cursor) {
      count
      cursor
      rows {
        datasourceId
        datatypeId
        score
        target  { id approvedSymbol }
        disease { id name }
      }
    }
  }
}
"

# -----------------------------
# Pagers
# This part turn paginated GraphQL responses from Open Targets into single, tidy data frames.
# The outputs can be used for plotting/analysis.
# -----------------------------

# Build GraphQL variables and then call post_gpl
.fetch_associations_page <- function(ensembl_id, page_size, page_index, log = TRUE) {
  vars <- list(ensemblId = ensembl_id, size = page_size, index = page_index)
  post_gql(q_associations, vars, log = log)
}

# Loop through all pages and return one tidy data frame
fetch_all_associations <- function(ensembl_id, page_size = 200, log = TRUE) {
  
  # Create an empty list to collect pages, and the page index idx is 0 for now
  # target symbol is initially unknown, and the total is infinite as we don't know how many rows exist
  out_rows <- list(); idx <- 0
  target_symbol <- NA_character_; total <- Inf
  
  repeat {
    dat <- .fetch_associations_page(ensembl_id, page_size, idx, log = log)  # Fetch a page
    tgt <- dat$data$target
    if (is.null(tgt)) break  # Break if target is missing
    
    if (is.na(target_symbol)) target_symbol <- tgt$approvedSymbol %||% NA_character_
    block <- tgt$associatedDiseases
    total <- block$count %||% 0
    rows <- block$rows
    
    if (is.null(rows) || NROW(rows) == 0) break  # Break if the page is empty
    out_rows[[length(out_rows) + 1]] <- rows  # Otherwise, append the pages into out_rows list
    
    got <- sum(vapply(out_rows, nrow, 0))  # Count how many rows have been collected so far. Go the the next page until we reach the total.
    if (got >= total) break
    idx <- idx + 1
  }
  
  df <- if (length(out_rows)) bind_rows(out_rows) else tibble() # Combine all pages into one data frame (or empty tibble if none).
  if (nrow(df)) {
    df <- df %>%
      rename(
        disease_id   = `disease.id`,
        disease_name = `disease.name`
      ) %>%
      mutate(target_id = ensembl_id,
             target_symbol = target_symbol, .before = 1)
  }
  df
}

.fetch_known_drugs_page <- function(ensembl_id, size, cursor = NULL, log = TRUE) {
  vars <- list(ensemblId = ensembl_id, size = size)
  if (!is.null(cursor)) vars$cursor <- cursor  # Include cursor only when it exists
  post_gql(q_known_drugs, vars, log = log)
}

fetch_all_known_drugs <- function(ensembl_id, page_size = 200, log = TRUE) {
  cursor <- NULL; out_rows <- list(); target_symbol <- NA_character_
  
  repeat {
    dat <- .fetch_known_drugs_page(ensembl_id, page_size, cursor, log = log)
    tgt <- dat$data$target
    if (is.null(tgt)) break
    if (is.na(target_symbol)) target_symbol <- tgt$approvedSymbol %||% NA_character_
    
    kd <- tgt$knownDrugs
    rows <- kd$rows
    if (is.null(rows) || NROW(rows) == 0) break
    
    out_rows[[length(out_rows) + 1]] <- rows
    cursor <- kd$cursor
    if (is.null(cursor) || is.na(cursor) || identical(cursor, "")) break
  }
  
  df <- if (length(out_rows)) bind_rows(out_rows) else tibble()
  if (nrow(df)) {
    df <- df %>%
      rename(
        drug_id        = `drug.id`,
        drug_name      = `drug.name`,
        disease_id     = `disease.id`,
        disease_name   = `disease.name`,
        target_id      = `target.id`,
        target_symbol  = `target.approvedSymbol`
      ) %>%
      mutate(target_id = target_id %||% ensembl_id)
  }
  df
}

fetch_evidence_td <- function(efo_id, ensembl_id, page_size = 200, log = TRUE) {
  cursor <- NULL; out_rows <- list()
  
  repeat {
    vars <- list(efoId = efo_id, ensemblId = ensembl_id, size = page_size)
    if (!is.null(cursor)) vars$cursor <- cursor
    dat <- post_gql(q_evidence_td, vars, log = log)
    
    ev <- dat$data$disease$evidences
    rows <- ev$rows
    if (is.null(rows) || NROW(rows) == 0) break
    
    out_rows[[length(out_rows) + 1]] <- rows
    cursor <- ev$cursor
    if (is.null(cursor) || is.na(cursor) || identical(cursor, "")) break
  }
  
  df <- if (length(out_rows)) bind_rows(out_rows) else tibble()
  if (nrow(df)) {
    df <- df %>%
      mutate(
        target_id     = map_chr(target, ~.x$id %||% NA_character_),
        target_symbol = map_chr(target, ~.x$approvedSymbol %||% NA_character_),
        disease_id    = map_chr(disease, ~.x$id %||% NA_character_),
        disease_name  = map_chr(disease, ~.x$name %||% NA_character_)
      ) %>%
      select(target_id, target_symbol, disease_id, disease_name,
             datasourceId, datatypeId, score)
  }
  df
}

# -----------------------------
# Tidy graph builder (visNetwork)
# This section turns tables into nodes (targets, diseases, drugs) and edges
# -----------------------------
dedup_df <- function(df, cols) {
  if (is.null(df) || !nrow(df)) return(df)
  df %>% distinct(across(all_of(cols)), .keep_all = TRUE)
}

build_graph <- function(assoc_df, known_drugs_df, target_id, target_symbol,
                        include_drug_disease_edges = TRUE) {
  
  # Nodes
  target_node <- tibble(
    id = target_id, label = target_symbol %||% target_id, group = "target"
  )
  
  disease_nodes <- bind_rows(
    assoc_df %>% select(id = disease_id, label = disease_name) %>% mutate(group = "disease"),
    known_drugs_df %>% select(id = disease_id, label = disease_name) %>% mutate(group = "disease")
  ) %>% dedup_df(c("id", "label", "group"))
  
  drug_nodes <- known_drugs_df %>%
    select(id = drug_id, label = drug_name) %>%
    mutate(group = "drug") %>%
    dedup_df(c("id", "label", "group"))
  
  nodes <- bind_rows(target_node, disease_nodes, drug_nodes) %>%
    dedup_df(c("id", "label", "group"))
  
  # Edges
  edges_td <- assoc_df %>%
    transmute(
      from = target_id,
      to   = disease_id,
      value = score,
      title = paste0("Target–Disease score: ", signif(score, 3), " | ", disease_name),
      type  = "target-disease"
    )
  
  edges_tdrug <- known_drugs_df %>%
    transmute(
      from = target_id,
      to   = drug_id,
      value = 1,
      title = paste0("Drug: ", drug_name,
                     ifelse(is.na(mechanismOfAction), "", paste0(" | MoA: ", mechanismOfAction)),
                     ifelse(is.na(phase), "", paste0(" | Phase: ", phase)),
                     ifelse(is.na(status), "", paste0(" | Status: ", status))),
      type  = "target-drug"
    )
  
  edges_ddisease <- if (isTRUE(include_drug_disease_edges)) {
    known_drugs_df %>%
      transmute(
        from = drug_id,
        to   = disease_id,
        value = 1,
        title = paste0(drug_name, " → ", disease_name),
        type  = "drug-disease"
      )
  } else {
    tibble()
  }
  
  edges <- bind_rows(edges_td, edges_tdrug, edges_ddisease) %>%
    dedup_df(c("from", "to", "type"))
  
  list(nodes = nodes, edges = edges)
}


# -----------------------------
# Public API: fetch_ot_data()
# -----------------------------
#' This function fetch and structure Open Targets data for visualization
#'
#' @param gene_id Ensembl gene ID (e.g., "ENSG00000169083")
#' @param disease_efo_id Optional EFO disease ID (e.g., "EFO_0000616")
#' @param min_score Minimum association score to include (default 0)
#' @param evidence_types Optional character vector of datatypeId or datasourceId to keep
#' @param page_size Page size for API pagination (default 200)
#' @param log Logical; print progress messages (default TRUE)
#' @return A list with: assoc_df, known_drugs_df, evidence_df, graph (nodes/edges), meta
#' @export
fetch_ot_data <- function(gene_id,
                          disease_efo_id = NULL,
                          min_score = 0,
                          evidence_types = NULL,
                          page_size = 200,
                          log = TRUE) {
  t0 <- Sys.time()
  if (isTRUE(log)) message("Fetching Open Targets data for: ", gene_id,
                           if (!is.null(disease_efo_id)) paste0(" & ", disease_efo_id) else "")
  
  # Associations (target -> diseases)
  assoc_df <- tryCatch(
    fetch_all_associations(gene_id, page_size = page_size, log = log),
    error = function(e) { warning("Associations error: ", e$message); tibble() }
  ) %>%
    filter(is.finite(score), score >= min_score)
  
  target_symbol <- assoc_df$target_symbol[1] %||% NA_character_
  
  # Known drugs (target -> drugs, and drug -> disease)
  known_drugs_df <- tryCatch(
    fetch_all_known_drugs(gene_id, page_size = page_size, log = log),
    error = function(e) { warning("Known drugs error: ", e$message); tibble() }
  )
  
  # Evidence for the specified target–disease pair (optional)
  evidence_df <- tibble()
  if (!is.null(disease_efo_id)) {
    evidence_df <- tryCatch(
      fetch_evidence_td(disease_efo_id, gene_id, page_size = page_size, log = log),
      error = function(e) { warning("Evidence error: ", e$message); tibble() }
    )
    
    # Evidence filtering by type (datatypeId or datasourceId)
    if (!is.null(evidence_types) && nrow(evidence_df)) {
      evidence_df <- evidence_df %>%
        filter(datatypeId %in% evidence_types | datasourceId %in% evidence_types)
    }
  }
  
  # Build visNetwork-ready graph
  graph <- build_graph(
    assoc_df = assoc_df,
    known_drugs_df = known_drugs_df,
    target_id = gene_id,
    target_symbol = target_symbol
  )
  
  meta <- list(
    gene_id = gene_id,
    disease_efo_id = disease_efo_id,
    min_score = min_score,
    evidence_types = evidence_types,
    page_size = page_size,
    fetched_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    elapsed_sec = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  )
  
  list(
    assoc_df = assoc_df,
    known_drugs_df = known_drugs_df,
    evidence_df = evidence_df,
    graph = graph,
    meta = meta
  )
}

