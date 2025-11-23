library(devtools)
library(RCy3)
library(R.ROSETTA)

# Ladda VisuNet-paketet
load_all("/Users/ibraheem/Desktop/visu_to_cyto/VisuNet.v.1.1")

# Ladda funktionen visunetcyto
source("/Users/ibraheem/Desktop/visu_to_cyto/VisuNet.v.1.1/visu_to_cyto.R")

# Kolla att Cytoscape är igång
cytoscapePing()

# Skapa eller läs in VisuNet-objektet
if (!exists("vis")) {
  if (file.exists("visunet_headless.rds")) {
    # Läs in sparat VisuNet-objekt
    vis <- readRDS("visunet_headless.rds")
  } else {
    # Använd Cytoscapes egna GO-funktioner → ingen GO i VisuNet
    addGO <- FALSE
    
    # Kör ROSETTA på datan (autcon måste finnas i environment)
    ros <- rosetta(autcon)
    
    # Kör visunet i headlessläge (ingen popup)
    options(
      visunet.headless     = TRUE,
      shiny.launch.browser = FALSE,
      viewer               = NULL,
      browser              = function(...) {}
    )
    
    # Skapa VisuNet objekt
    vis <- visunet(
      ros$main,
      addGO       = addGO,
      GO_ontology = "MF",
      NodeSize    = "sum"
    )
    
    # Spara VisuNet-objektet för framtida körningar
    saveRDS(vis, "visunet_headless.rds")
  }
}

# Skicka VisuNet objektet till Cytoscape
visunetcyto(
  vis   = vis,
  title = "Autism_VisuNet_optA"
)

# Klart: Nätverken ska nu finnas i Cytoscape
