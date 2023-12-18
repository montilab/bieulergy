
# Load R packages
library(shiny)
library(bieulergy)

# Set limitation for uploading files
options(shiny.maxRequestSize=30*1024^2)

# Create a shiny app object
bieulergy::run.shiny(host='0.0.0.0', port=3838)
