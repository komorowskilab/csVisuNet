createStyle = function(stylename) {
  defaults <- list(NODE_SHAPE="circle", NODE_LABEL_POSITION="S,C,c,0.00,10.00")

  # Node styles
  nodeLabels <- mapVisualProperty('node label','label','p')
  nodeSize <- mapVisualProperty('node size','value','c', c(min(network$nodes$value),max(network$nodes$value)), c(20, 60))
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
}
