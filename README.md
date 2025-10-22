# OTxplorer-R

**OTxplorer-R** is an interactive R Shiny app and package that helps researchers explore relationships between drug targets, diseases, and drugs using data from the [Open Targets Platform](https://platform.opentargets.org/).

## 🚀 Features
- Fetches target–disease–drug associations via Open Targets GraphQL API  
- Visualizes relationships as interactive networks (visNetwork, igraph)  
- Supports search, filtering, and custom gene list uploads  
- Evidence-based graph exploration for target discovery  

## Installation
- To test and run OTxplorer-R, we recommend installing and activating a Conda environment using:

```
conda env create -f environment.yml
conda activate otxplorer-r
```

## Dependencies
| Package        | Version                                   |
|----------------|-------------------------------------------|
| **shiny**      | 1.11.1                                    |
| **visnetwork** | 2.1.4                                     |
| **igraph**     | 2.1.4                                     |
| **tidyverse**  | 2.0.0                                     |
| **jsonlite**   | 2.0.0                                     |
| **httr**       | 1.4.7                                     |
| **readr**      | 2.1.5                                     |

## Acknowledgment
OTxplorer-R was built during the OpenTargets hackathon, where collaboration and rapid prototyping inspired the project’s focus on interactive exploration of Open Targets data.  
Special thanks to the hackathon organizers and OTxplorer-R team members whose insights and contributions shaped the early design and functionality of this tool.

## Running the app
shiny::runApp("app.R")
