createStyle = function(stylename, network, NodeSizeScale) {
  defaults <- list(NODE_SHAPE="circle",
                   NODE_LABEL_POSITION="S,N,c,0,5",
                   NODE_SELECTED_PAINT="#d2e5ff",
                   EDGE_STROKE_SELECTED_PAINT="yellow")

  # Node styles
  nodeTooltip <- mapVisualProperty('node tooltip','title','p')
  nodeSize <- mapVisualProperty('node size','value','c', c(min(network$nodes$value),max(network$nodes$value)), NodeSizeScale)
  nodeFills <- mapVisualProperty('node fill color', 'color.background', 'p')
  nodeBorderWidth <- mapVisualProperty('node border width', 'borderWidth', 'p')
  nodeBorderColor <- mapVisualProperty('node border paint', 'color.border', 'p')
  nodeLabels <- mapVisualProperty('node label', 'name', 'p')

  # Edge styles
  edgeColor <- mapVisualProperty('edge stroke unselected paint','color','p')
  edgeLabels <- mapVisualProperty('edge tooltip','title','p')
  edgeWidth <- mapVisualProperty('edge width', 'width', 'p')

  try(deleteVisualStyle(stylename), silent=TRUE)

  createVisualStyle(stylename, defaults, list(nodeSize, nodeFills, nodeLabels,
      nodeBorderWidth, nodeBorderColor, edgeColor, edgeLabels,
      edgeWidth, nodeTooltip))
}
