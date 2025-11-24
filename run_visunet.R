library(R.ROSETTA)
library(devtools)
library(RCy3)

# load version of VisuNet
load_all("~/Documents/GitHub/VisuNet.v.1.1")

ros <- rosetta(autcon)
# VisuNet with GO annotations
vis <- visunet(ros$main, addGO = TRUE, GO_ontology = "MF", NodeSize = "sum")

# VisuNet in cytoscaoe
# Have cytoscape app open on computer
visunetcyto(ros$main, title="VisuNet Networks")
