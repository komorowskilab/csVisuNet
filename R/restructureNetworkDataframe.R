restructureNetworkDF <- function(network){
  colnames(network$edges)[colnames(network$edges) == "from"] <- "source"
  colnames(network$edges)[colnames(network$edges) == "to"] <- "target"
  colnames(network$nodes)[colnames(network$nodes) == "label"] <- "name"

  network$nodes$popup <- paste0(
    "label: attribute=NAME labelsize=12 color=black outline=false background=false"
  )

  network$edges$title <- paste0(
    "Edge: ", network$edges$source, ", ", network$edges$target,
    "\nConnection: ", round(network$edges$conn, 2)
  )
  network$nodes$title <- paste0(
  "Name: ", network$nodes$name, "\nEdges: ", network$nodes$NRules,
  "\nNode connection: ", round(network$nodes$NodeConnection, 2), "\nMean accuracy: ",
  round(network$nodes$meanAcc, 2), "\nMean support: ", round(network$nodes$meanSupp, 2),
  "\nMean decision coverage: ", round(network$nodes$meanDecisionCoverage, 2))

  node_columns_to_keep <- c("id", "name", "group", "color.background", "value",
                            "color.border", "borderWidth", "meanAcc",
                            "meanSupp", "meanDecisionCoverage", "title", "popup")

  network$nodes <- network$nodes[intersect(node_columns_to_keep, names(network$nodes))]

  edge_columns_to_keep <- c("source", "target", "color", "width", "title")

  network$edges <- network$edges[intersect(edge_columns_to_keep, names(network$edges))]

  return(network)
}
