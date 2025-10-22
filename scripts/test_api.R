suppressPackageStartupMessages({
  library(visNetwork)
  library(tidyverse)
})

source("./get_data.R")

USE_MOCK <- FALSE  # toggle

# -----------------------------
# Mock function for offline testing
# -----------------------------
# Returns a tiny synthetic graph if you’re offline or rate-limited.

mock_fetch_ot_data <- function() {
  assoc_df <- tibble(
    target_id = "ENSG00000000000",
    target_symbol = "MOCK1",
    disease_id = c("EFO_X1", "EFO_X2"),
    disease_name = c("Mock Disease A", "Mock Disease B"),
    score = c(0.9, 0.6)
  )
  
  known_drugs_df <- tibble(
    target_id = "ENSG00000000000",
    target_symbol = "MOCK1",
    drug_id = c("CHEMBL:DRUG1", "CHEMBL:DRUG2"),
    drug_name = c("Mockinib", "Placebex"),
    disease_id = c("EFO_X1", "EFO_X2"),
    disease_name = c("Mock Disease A", "Mock Disease B"),
    mechanismOfAction = c("Inhibitor", "Antagonist"),
    phase = c("4", "2"),
    status = c("Approved", "Investigational")
  )
  
  graph <- build_graph(
    assoc_df = assoc_df, known_drugs_df = known_drugs_df,
    target_id = "ENSG00000000000", target_symbol = "MOCK1"
  )
  
  list(
    assoc_df = assoc_df,
    known_drugs_df = known_drugs_df,
    evidence_df = tibble(),
    graph = graph,
    meta = list(gene_id = "MOCK", fetched_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  )
}

# Run the test
res <- if (USE_MOCK) {
  mock_fetch_ot_data()
} else {
  fetch_ot_data(
    gene_id = "ENSG00000169083",
    disease_efo_id = "EFO_0000616",
    min_score = 0.2,
    evidence_types = c("genetic_association","literature"),
    page_size = 200,
    log = TRUE
  )
}

# Create export folder
out_dir <- "test_exports"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Save CSVs and RData
readr::write_csv(res$graph$nodes, file.path(out_dir,"nodes.csv"))
readr::write_csv(res$graph$edges, file.path(out_dir,"edges.csv"))
saveRDS(res, file.path(out_dir, "res.rds"))   # full result object

# Confirmation message
message("Export complete: ", out_dir)
message("Files saved:\n  - nodes.csv\n  - edges.csv\n  - res.rds")

# Visualise with vizNetwork
visNetwork(res$graph$nodes, res$graph$edges) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLegend()
