makeMeanAccuracySlider <- function(network = NULL) {
  # Om du inte anger nätverk, ta det nuvarande
  if (is.null(network)) {
    network <- getNetworkSuid()  # current network
  }
  
  # Hämta meanAcc från NOD-tabellen i Cytoscape
  meanAcc_df <- getTableColumns(
    table   = "node",
    columns = "meanAcc",
    network = network
  )
  
  # Plocka ut vektorn med värden
  meanAcc <- meanAcc_df$meanAcc
  meanAcc <- meanAcc[!is.na(meanAcc)]
  
  if (length(meanAcc) == 0) {
    stop("Det finns inga värden i kolumnen 'meanAcc' i nodtabellen.")
  }
  
  # Sätt intervall (slider-range)
  rng <- range(meanAcc)
  
  # Skapa ett kolumnfilter; BETWEEN ger en range-slider i Cytoscape
  createColumnFilter(
    filter.name = "meanAcc filter",
    column      = "meanAcc",
    criterion   = rng,          # c(min, max)
    predicate   = "BETWEEN",
    type        = "nodes",      # <-- NODER
    hide        = FALSE,        # inget göms automatiskt
    network     = network,
    apply       = FALSE         # skapa filtret men kör det inte direkt
  )
  
  message("Filter 'meanAcc filter' skapat. Öppna Select → Filter i Cytoscape så ser du slidern.")
}