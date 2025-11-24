createStyle = function(stylename, network) {
  defaults <- list(NODE_SHAPE="circle",
                   NODE_LABEL_POSITION="S,NW,l,0.00,10.00",
                   NODE_LABEL_BACKGROUND_COLOR="gray",
                   NODE_LABEL_BACKGROUND_SHAPE="rectangle",
                   EDGE_LABEL_BACKGROUND_SHAPE="rectangle",
                   EDGE_STROKE_SELECTED_PAINT="yellow",
                   EDGE_LABEL_BACKGROUND_COLOR="gray",
                   EDGE_LABEL_POSITION="S,NW,l,0.00,10.00")
  # Node styles
  nodeTooltip <- mapVisualProperty('node label','title','p')
  nodeTooltipVisibility <- mapVisualProperty('node label transparency','selected','d', c('true', 'false'), c(255,0))
  nodeSize <- mapVisualProperty('node size','value','c', c(min(network$nodes$value),max(network$nodes$value)), c(20, 60))
  nodeFills <- mapVisualProperty('node fill color', 'color.background', 'p')
  nodeBorderWidth <- mapVisualProperty('node border width', 'borderWidth', 'p')
  nodeBorderColor <- mapVisualProperty('node border paint', 'color.border', 'p')
  nodeLabels <- mapVisualProperty('node customgraphics 1', 'popup', 'p')
  nodeBackground <- mapVisualProperty('node label background transparency','selected','d', c('true', 'false'), c(255,0))


  # Edge styles
  edgeColor <- mapVisualProperty('edge stroke unselected paint','color','p')
  edgeLabels <- mapVisualProperty('edge label','title','p')
  edgeVisibility <- mapVisualProperty('edge label transparency','selected','d', c('true', 'false'), c(255,0))
  edgeWidth <- mapVisualProperty('edge width', 'width', 'p')
  edgeBackground <- mapVisualProperty('edge label background transparency','selected','d', c('true', 'false'), c(255,0))

  try(deleteVisualStyle(stylename), silent=TRUE)

  createVisualStyle(stylename, defaults, list(nodeSize, nodeFills, nodeLabels,
      nodeBorderWidth, nodeBorderColor, nodeBackground, edgeColor, edgeLabels, edgeVisibility,
      edgeWidth, nodeTooltip, nodeTooltipVisibility, edgeBackground))

  setNodeCustomPosition(nodeAnchor = "S", graphicAnchor = "N", yOffset = 5, slot = 1, style.name=stylename)

}
