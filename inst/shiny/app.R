# Shiny
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinythemes))

# Packages
suppressPackageStartupMessages(library(R6))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(markdown))
suppressPackageStartupMessages(library(visNetwork))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(reactable))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(rjson))
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(hypeR))
suppressPackageStartupMessages(library(dnet))

# Loads the data oracle
source(file.path("utils", "resources.R"), local=TRUE)
source(file.path("utils", "oracle.R"), local=TRUE)

# Application
ui <- navbarPage(
    title="bieulergy",
    id="tabs",
    theme=shinytheme("simplex"),
    source(file.path("ui", "00_Data.R"), local=TRUE)$value,
    source(file.path("ui", "03_Visualize.R"), local=TRUE)$value,
    source(file.path("ui", "02_Centrality.R"), local=TRUE)$value,
    source(file.path("ui", "05_Documentation.R"), local=TRUE)$value,
    source(file.path("ui", "06_Versioning.R"), local=TRUE)$value
)

server <- function(input, output, session) {
    source(file.path("server", "00_Data.R"), local=TRUE)$value
    source(file.path("server", "03_Visualize.R"), local=TRUE)$value
    source(file.path("server", "02_Centrality.R"), local=TRUE)$value
    source(file.path("server", "06_Versioning.R"), local=TRUE)$value
}

shinyApp(ui=ui, server=server)
