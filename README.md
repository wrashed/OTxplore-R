# OTxplorer-R

**OTxplorer-R** is an interactive R Shiny app and package that helps researchers explore relationships between drug targets, diseases, and drugs using data from the [Open Targets Platform](https://platform.opentargets.org/).

## 🚀 Features
- Fetches target–disease–drug associations via Open Targets GraphQL API  
- Visualizes relationships as interactive networks (visNetwork, igraph)  
- Supports search, filtering, and custom gene list uploads  
- Evidence-based graph exploration for target discovery  

## 🧠 Built With
- R Shiny  
- visNetwork, igraph  
- tidyverse, jsonlite  
- Open Targets API  

## 🛠️ Setup
```r
# Install dependencies
install.packages(c("shiny", "visNetwork", "igraph", "tidyverse", "jsonlite"))

# Run the app
shiny::runApp("app.R")
