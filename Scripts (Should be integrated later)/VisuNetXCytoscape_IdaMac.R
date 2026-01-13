library(devtools)
if(!"RCy3" %in% installed.packages()){
  install.packages("BiocManager")
  BiocManager::install("RCy3")
}
library(RCy3)
#load_all(path = "C://Users//tilda//OneDrive//Dokument//Tillämpad bioinformatik//VisuNet.v.1.1")
load_all(path = "/Users/idanordstrom/Documents/GitHub/VisuNet.v.1.1")

autconJohnson <- rosetta(autcon, roc = TRUE, clroc = "autism")
rules <- autconJohnson$main

title <- "Autism_Visunet"

Autism_Visunet <- visunet(rules)
##
cytoscapePing() #För eventuella problem med connection till Cytoscape
##

for (net_name in names(Autism_Visunet)) {
  try(deleteNetwork(net_name), silent=TRUE)
  network <- Autism_Visunet[[net_name]]
  # -- Början på nytt --
  edges <- network$edges #varför
  colnames(edges)[colnames(edges) == "from"] <- "source"
  colnames(edges)[colnames(edges) == "to"]   <- "target"
  
  all_ids <- unique(c(edges$source, edges$target))
  nodes_full <- data.frame(id = all_ids, stringsAsFactors = FALSE)
  nodes_full <- dplyr::left_join(nodes_full, network$nodes, by = "id")
  
  createNetworkFromDataFrames(nodes_full, edges,
                              title = net_name,
                              collection = title)
  
  # -- slut på nytt -- 
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
}

# Composite-filter som samlar alla tre i EN ruta
net_suid <- getNetworkSuid(title = "all") #hittar SUID för nätverket som innehåller alla noder
net_suid <- getNetworkSuid(title = "all")
createCompositeFilter(
  filter.name = "meanAcc + meanSupp + meanDecisionCoverage",
  filter.list = c(
    "meanAcc filter",
    "Mean support filter",
    "Mean decision coverage filter"
  ),
  type   = "ALL",    # ALL = AND, ANY = OR
  hide   = FALSE,
  network = net_suid,
  apply  = FALSE     # bara skapa, inte köra direkt
)
