makeMeanSupportSlider <- function(network = NULL) {
  # Om du inte anger nätverk, ta det nuvarande
  if (is.null(network)) {
    network <- getNetworkSuid()  # current network
  }
  
  # Hämta meanAcc från NOD-tabellen i Cytoscape
  meanSupp_df <- getTableColumns(
    table   = "node",
    columns = "meanSupp",
    network = network
  )
  
  # Plocka ut vektorn med värden
  meanSupp <- meanSupp_df$meanSupp
  meanSupp <- meanSupp[!is.na(meanSupp)]
  
  if (length(meanSupp) == 0) {
    stop("Det finns inga värden i kolumnen 'meanSupp' i nodtabellen.")
  }
  
  # Sätt intervall (slider-range)
  rng <- range(meanSupp)
  
  # Skapa ett kolumnfilter; BETWEEN ger en range-slider i Cytoscape
  createColumnFilter(
    filter.name = "Mean support filter",
    column      = "meanSupp",
    criterion   = rng,          # c(min, max)
    predicate   = "BETWEEN",
    type        = "nodes",      # <-- NODER
    hide        = FALSE,        # inget göms automatiskt
    network     = network,
    apply       = FALSE         # skapa filtret men kör det inte direkt
  )
  
  message("Filter 'Mean support filter' skapat. Öppna Select → Filter i Cytoscape så ser du slidern.")
}