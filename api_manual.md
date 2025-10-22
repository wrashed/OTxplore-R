
# OTxplore-R - API Data Access and Testing Guide

This repository provides an R module (`scripts/get_data.R`) that connects to the **Open Targets GraphQL API**, retrieves **Target–Disease–Drug** relationships, and converts them into tidy data frames and **visNetwork-ready** graphs.

A separate test script (`scripts/test_api.R`) allows collaborators to **run mock tests** (offline) or **live API queries**, automatically saving results for visualization or sharing.

---

## File Overview

| File | Purpose |
|------|----------|
| `R/get_data.R` | Core API module — fetches Open Targets data, handles pagination, builds visNetwork graph |
| `test_api.R` | Test runner — executes mock or live data fetching and exports results |

---

## Key Function

### Output Structure from `fetch_ot_data()`

Returns a list with:

- `assoc_df` — Target–disease associations  
- `known_drugs_df` — Target–drug–disease relationships  
- `evidence_df` — Evidence details (optional)  
- `graph` — `list(nodes, edges)` formatted for `visNetwork()`  
- `meta` — Metadata (gene/disease IDs, runtime, parameters)

## Running Tests

To run tests, run the following command

### Requirements

Install the required packages once:

```r
install.packages(c("httr", "jsonlite", "tidyverse", "visNetwork", "readr"))
```

Ensure this structure exists
```bash
scripts/get_data.R
scripts/test_api.R
```
### Option A - Run interactively in R or RStudio
- Steps:
  1. Open the project root in R or RStudio
  2. Load the scripts:
  ```r
  source("scripts/get_data.R")
  source("scripts/test_api.R")
  ```
  3. At the top of ```test_api.R```, choose between mock and live testing:
  ```r
  USE_MOCK <- TRUE   # Offline test using mock data
  # or
  USE_MOCK <- FALSE  # Live API query
  ```
  4. This script will:
  - Fetch data (mock or live)
  - Build a tidy graph
  - Export results into test_exports/
  - Show an interactive visNetwork plot

  5. Expected output files

  ```pgsql
  test_exports/
  ├─ nodes.csv   # Graph nodes (id, label, group)
  ├─ edges.csv   # Graph edges (from, to, type, title)
  └─ res.rds     # Full R object: assoc_df, known_drugs_df, evidence_df, graph, meta
  ```
  6. To reload and visualiza later
  ```r
  res <- readRDS("test_exports/res.rds")
  library(visNetwork)
  visNetwork(res$graph$nodes, res$graph$edges)
  ```

### Option B - Run from the Command Line
Run the full script directly without opening RStudio:
```bash
Rscript test_api.R
```

### Example: Running with Different Targets
You can modify the parameters in ```test_api.R``` to test other genes or diseases:

```r
res <- fetch_ot_data(
  gene_id = "ENSG00000155657",          # TP53
  disease_efo_id = "EFO_0000708",       # breast carcinoma
  min_score = 0.1,
  evidence_types = NULL,
  page_size = 200,
  log = TRUE
)
```

### For Collaborators
To run the test and visualize results, simply execute:

```r
source("scripts/get_data.R")
source("scripts/test_api.R")
```

After execution, open ```test_exports/nodes.csv```, ```test_exports/edges.csv```, or view the graph interactively in RStudio’s Viewer pane.
## Authors

- [@siwanartma](https://github.com/SiwanartMa)

