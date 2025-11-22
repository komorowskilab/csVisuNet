library(devtools)
library(RCy3)

load_all("/Users/ibraheem/Desktop/visu_to_cyto/VisuNet.v.1.1")
source("/Users/ibraheem/Desktop/visu_to_cyto/VisuNet.v.1.1/visu_to_cyto.R")

cytoscapePing()  # kolla att Cytoscape är igång

# Ladda VisuNet objektet från tidigare körning
if (!exists("vis")) {
  vis <- readRDS("visunet_headless.rds")
}

# Skicka VisuNet objektet till Cytoscape
visunetcyto(
  vis   = vis,
  title = "Autism_VisuNet_optA"
)

# Klart, nätverken ska nu finnas i Cytoscape
