library(devtools)
if(!"RCy3" %in% installed.packages()){
    install.packages("BiocManager")
    BiocManager::install("RCy3")
}
library(RCy3)
load_all(path = "C://Users//tilda//OneDrive//Dokument//Tillämpad bioinformatik//VisuNet.v.1.1")

autconJohnson <- rosetta(autcon, roc = TRUE, clroc = "autism")
rules <- autconJohnson$main

title <- "Autism_Visunet"

Autism_Visunet <- visunet(rules)

cytoscapePing() #För eventuella problem med connection till Cytoscape

for (net_name in names(Autism_Visunet)) {
  try(deleteNetwork(net_name), silent=TRUE)
  network <- Autism_Visunet[[net_name]]
  colnames(network$edges)[colnames(network$edges) == "from"] <- "source"
  colnames(network$edges)[colnames(network$edges) == "to"] <- "target"
  createNetworkFromDataFrames(network$nodes,network$edges, title=net_name, collection=title)

  style.name = paste(net_name, '_style')
  defaults <- list(NODE_SHAPE="circle")

  # Node styles
  nodeLabels <- mapVisualProperty('node label','label','p')
  nodeSize <- mapVisualProperty('node size','meanSupp','c',c(min(network$nodes$meanSupp),max(network$nodes$meanSupp)), c("30","75"))
  nodeFills <- mapVisualProperty('node fill color', 'color.background', 'p')
  nodeBorderWidth <- mapVisualProperty('node border width', 'borderWidth', 'p')
  nodeBorderColor <- mapVisualProperty('node border paint', 'color.border', 'p')

  # Edge styles
  edgeColor <- mapVisualProperty('edge stroke unselected paint','color','p')
  edgeLabels <- mapVisualProperty('edge label','title','p')
  edgeVisibility <- mapVisualProperty('edge label transparency','selected','d', c('true', 'false'), c(255,0))
  edgeWidth <- mapVisualProperty('edge width', 'width', 'p')

  try(deleteVisualStyle(style.name), silent=TRUE)
  createVisualStyle(style.name, defaults, list(nodeSize, nodeFills, nodeLabels, nodeBorderWidth, nodeBorderColor, edgeColor, edgeLabels, edgeVisibility, edgeWidth))
  setVisualStyle(style.name)
  net_suid <- getNetworkSuid(title = net_name)
  makeMeanAccuracySlider(network = net_suid)
  makeMeanSupportSlider(network = net_suid)
  makeMeanDecCoverageSlider(network = net_suid)

  # Composite-filter som samlar alla tre i EN ruta
  createCompositeFilter(
    filter.name = "meanAcc + meanSupp + meanDecisionCoverage",
    filter.list = c(
      "meanAcc filter",
      "Mean support filter",
      "Mean decision coverage filter"
    ),
    type   = "ALL",    # ALL = AND, ANY = OR
    hide   = TRUE, # Vi vill att de som inte matchar filtret ska gömmas
    network = net_suid,
    apply  = FALSE     # bara skapa, inte köra direkt
  )
}
