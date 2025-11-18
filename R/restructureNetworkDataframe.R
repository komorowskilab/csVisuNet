restructureNetworkDF <- function(network){
  colnames(network$edges)[colnames(network$edges) == "from"] <- "source"
  colnames(network$edges)[colnames(network$edges) == "to"] <- "target"
  network$nodes$popup <- paste0(
    "label: attribute=LABEL labelsize=12 color=black outline=false background=false"
  )

  network$edges$title <- paste0(
    "Edge: ", network$edges$source, ", ", network$edges$target,
    "\nConnection: ", network$edges$conn
  )
  network$nodes$title <- paste0(
  "Name: ", network$nodes$namem, "\nEdges: ", network$nodes$NRules,
  "\nNode connection: ", network$nodes$NodeConnection, "\nMean accuracy: ",
  network$nodes$meanAcc, "\nMean support: ", network$nodes$meanSupp,
  "\nMean decision coverage: ", network$nodes$meanDecisionCoverage)

  return(network)
}
