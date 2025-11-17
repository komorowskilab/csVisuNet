clearCollection = function(title) {
  nodes <- data.frame(id=c("node 0","node 1"),
                      group=c("A","A"), # categorical strings
                      score=as.integer(c(20,10)), # integers
                      stringsAsFactors=FALSE)
  edges <- data.frame(source=c("node 0"),
                      target=c("node 1"),
                      stringsAsFactors=FALSE)

  createNetworkFromDataFrames(nodes, edges, title="temp", collection=title)

  collectionSuid <- getCollectionSuid()
  nets_to_delete <- getCollectionNetworks(collectionSuid)
  for (net in nets_to_delete){
    deleteNetwork(network=net)
  }
}
