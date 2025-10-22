
# OTxplore-R - API Data Access and Testing Guide

This repository provides an R module (`scripts/get_data.R`) that connects to the **Open Targets GraphQL API**, retrieves **Target–Disease–Drug** relationships, and converts them into tidy data frames and **visNetwork-ready** graphs.

A separate test script (`scripts/test_api.R`) allows collaborators to **run mock tests** (offline) or **live API queries**, automatically saving results for visualization or sharing.

---

## File Overview

| File | Purpose |
|------|----------|
| `scripts/get_data.R` | Core API module — fetches Open Targets data, handles pagination, builds visNetwork graph |
| `scripts/test_api.R` | Test runner — executes mock or live data fetching and exports results |

---

## Key Function

### Using the `fetch_ot_data` function
If you want to use the Open Targets API directly — for analysis, data integration, or custom visualization —  
you can call the `fetch_ot_data()` function from the module `script/get_data.R`.

This function connects to the **Open Targets GraphQL API**, fetches all relevant target–disease–drug relationships, and returns a single structured list with data frames and a ready-to-plot network graph.

---

#### Step 1 — Load the module
```r
source("scripts/get_data.R")
```
#### Step 2 - Run a live API query
You can specify:
- gene_id — Ensembl ID for your target gene
- disease_efo_id (optional) — EFO ID for a disease
- min_score — Minimum association score to include
- evidence_types (optional) — Filter evidence by data source or data type
```r
res <- fetch_ot_data(
  gene_id = "ENSG00000169083",          # Example: Androgen Receptor (AR)
  disease_efo_id = "EFO_0000616",       # Example: Prostate Carcinoma
  min_score = 0.2,                      # Filter low-confidence associations
  evidence_types = c("genetic_association", "literature"),  # Optional
  page_size = 200,
  log = TRUE
)
```
#### Step 3 - Explore the results
`fetch_ot_data()` returns a structured list with the following elements:
| Element | Type | Description |
|------|----------|----------|
| `assoc_df` | data.frame | Target-disease associations |
| `known_drugs_df` | data.frame | Target-drug-disease relationships |
| `evidence_df` | data.frame | Evidence for specific target–disease pairs |
| `graph` | list | Contains `nodes` and `edges` ready for visNetwork |
| `meta` | list | Metadata (parameters, timestamps, runtime) |

Inspect the results in R:
```r
str(res$graph$nodes)
str(res$graph$edges)
head(res$assoc_df)
head(res$known_drugs_df)
head(res$evidence_df)
```

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

#### Step 1 — Open the project root in R or RStudio
#### Step 2 — Load the scripts:
```r
source("scripts/get_data.R")
source("scripts/test_api.R")
```
#### Step 3 — At the top of ```test_api.R```, choose between mock and live testing:
```r
USE_MOCK <- TRUE   # Offline test using mock data
# or
USE_MOCK <- FALSE  # Live API query
```
#### Step 4 — This script will:
- Fetch data (mock or live)
- Build a tidy graph
- Export results into test_exports/
- Show an interactive visNetwork plot

#### Step 5 — Expected output files

```pgsql
test_exports/
├─ nodes.csv   # Graph nodes (id, label, group)
├─ edges.csv   # Graph edges (from, to, type, title)
└─ res.rds     # Full R object: assoc_df, known_drugs_df, evidence_df, graph, meta
```
#### Step 6 — To reload and visualiza later
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

