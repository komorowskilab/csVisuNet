restructureNetworkDF <- function(network, NodeBorderScale, addGO){
  colnames(network$edges)[colnames(network$edges) == "from"] <- "source"
  colnames(network$edges)[colnames(network$edges) == "to"] <- "target"
  colnames(network$nodes)[colnames(network$nodes) == "label"] <- "name"

  network$nodes$popup <- paste0(
    "label: attribute=NAME labelsize=12 color=black outline=false background=false"
  )

  network$nodes$borderWidth <- network$nodes$borderWidth * NodeBorderScale

  network$edges$title <- paste0(
    "Edge: ", sub("^[^_]*_", "",network$edges$source), ", ", sub("^[^_]*_", "",network$edges$target),
    "\nConnection: ", round(network$edges$conn, 2)
  )
  network$nodes$title <- paste0(
  "Name: ", sub("^[^_]*_", "",network$nodes$id), "\nEdges: ", network$nodes$NRules,
  "\nNode connection: ", round(network$nodes$NodeConnection, 2), "\nMean accuracy: ",
  round(network$nodes$meanAcc, 2), "\nMean support: ", round(network$nodes$meanSupp, 2),
  "\nMean decision coverage: ", round(network$nodes$meanDecisionCoverage, 2))

  if (addGO){
    network$nodes$title <- ifelse(
      !is.na(network$nodes$GO_function),
      paste0(
        network$nodes$title,
        '\nGO: ',
        ifelse(network$nodes$GO_function == "", "NA", network$nodes$GO_function)
      ),
      network$nodes$title
    )
    network$nodes$title = gsub("<br/>", "\n", network$nodes$title)
  }

  node_columns_to_keep <- c("id", "name", "group", "color.background", "value",
                            "color.border", "borderWidth", "meanAcc",
                            "meanSupp", "meanDecisionCoverage", "title", "popup")

  network$nodes <- network$nodes[intersect(node_columns_to_keep, names(network$nodes))]

  edge_columns_to_keep <- c("id", "source", "target", "color", "width", "title")

  network$edges <- network$edges[intersect(edge_columns_to_keep, names(network$edges))]

  return(network)
}
