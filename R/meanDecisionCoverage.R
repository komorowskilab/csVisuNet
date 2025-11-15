makeMeanDecCoverageSlider <- function(network = NULL) {
  
  if (is.null(network)) {
    network <- getNetworkSuid()  
  }
  
  meanCov_df <- getTableColumns(
    table   = "node",
    columns = "meanDecisionCoverage",
    network = network
  )
  
  meanCov <- meanCov_df$meanDecisionCoverage
  meanCov <- meanCov[!is.na(meanCov)]
  
  if (length(meanCov) == 0) {
    stop("Det finns inga värden i kolumnen 'meanDecisionCoverage'.")
  }
  
  rng <- range(meanCov)
  
  createColumnFilter(
    filter.name = "Mean decision coverage filter",
    column      = "meanDecisionCoverage",
    criterion   = rng,
    predicate   = "BETWEEN",
    type        = "nodes",
    hide        = FALSE,
    network     = network,
    apply       = FALSE
  )
  
  message("Filter 'Mean decision coverage filter' skapat!")
}
