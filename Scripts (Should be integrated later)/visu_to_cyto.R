library(RCy3)
library(devtools)

load_all("/Users/ibraheem/Desktop/visu_to_cyto/VisuNet.v.1.1")

if (!exists("vis")) vis <- readRDS("visunet_headless.rds")

title <- "Autism_VisuNet"

rename_edges <- function(df) {
  if (is.null(df) || !nrow(df)) return(df)
  names(df)[names(df) == "from"] <- "source"
  names(df)[names(df) == "to"]   <- "target"
  df
}
map_if <- function(ok, vp, col) if (isTRUE(ok)) mapVisualProperty(vp, col, 'p') else NULL

for (net_name in names(vis)) {
  net <- vis[[net_name]]
  if (is.null(net) || is.null(net$nodes)) next
  
  nodes <- net$nodes
  edges <- rename_edges(net$edges)
  
  try(deleteNetwork(net_name), silent = TRUE)
  if (!is.null(edges) && nrow(edges) > 0) {
    createNetworkFromDataFrames(nodes, edges, title = net_name, collection = title)
  } else {
    createNetworkFromDataFrames(nodes = nodes, title = net_name, collection = title)
  }
  
  style.name <- paste0(net_name, "_style")
  defaults   <- list(NODE_SHAPE = "circle", EDGE_LABEL = "")
  
  nodeLabels    <- mapVisualProperty('node label', 'label', 'p')
  nodeFills     <- mapVisualProperty('node fill color', 'color.background', 'p')
  nodeBorderW   <- mapVisualProperty('node border width', 'borderWidth', 'p')
  nodeBorderCol <- mapVisualProperty('node border paint', 'color.border', 'p')
  
  if ("meanSupp" %in% names(nodes)) {
    nodeSize <- mapVisualProperty('node size', 'meanSupp', 'c',
                                  range(nodes$meanSupp, finite = TRUE), c("30","75"))
  } else {
    nodeSize <- mapVisualProperty('node size', 'value', 'c',
                                  range(nodes$value, finite = TRUE), c("30","75"))
  }
  
  edgeColor <- map_if(!is.null(edges) && "color" %in% names(edges),
                      'edge stroke unselected paint', 'color')
  edgeWidth <- map_if(!is.null(edges) && "width" %in% names(edges),
                      'edge width', 'width')
  
  try(deleteVisualStyle(style.name), silent = TRUE)
  createVisualStyle(style.name, defaults,
                    Filter(Negate(is.null),
                           list(nodeLabels, nodeFills, nodeBorderW, nodeBorderCol, nodeSize,
                                edgeColor, edgeWidth)))
  setVisualStyle(style.name)
}
