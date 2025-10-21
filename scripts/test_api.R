suppressPackageStartupMessages({
  library(visNetwork)
  library(tidyverse)
})

source("./get_data.R")

USE_MOCK <- TRUE  # toggle

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

