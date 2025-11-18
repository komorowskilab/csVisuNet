restructureNetworkDF <- function(network){
  colnames(network$edges)[colnames(network$edges) == "from"] <- "source"
  colnames(network$edges)[colnames(network$edges) == "to"] <- "target"
  return(network)
}
