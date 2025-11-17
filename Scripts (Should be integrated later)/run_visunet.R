library(devtools)
library(R.ROSETTA)

load_all("/Users/ibraheem/Desktop/visu_to_cyto/VisuNet.v.1.1")

options(visunet.headless = TRUE,
        shiny.launch.browser = FALSE, viewer = NULL, browser = function(...) {})

ros <- rosetta(autcon)
vis <- visunet(ros$main, addGO = TRUE, GO_ontology = "MF", NodeSize = "sum")

saveRDS(vis, "visunet_headless.rds")
