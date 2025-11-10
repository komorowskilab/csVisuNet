library(R.ROSETTA)
library(devtools)

# load version of VisuNet
load_all("~/Documents/Forskningspraktik/VisuNet")

ros <- rosetta(autcon)
# VisuNet with GO annotations
vis <- visunet(ros$main, addGO = TRUE, GO_ontology = "MF", NodeSize = "sum")
