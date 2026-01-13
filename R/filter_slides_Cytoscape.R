filter_slides <- function(network = NULL) {
  if (is.null(network)) {
    network <- getNetworkSuid()
  }

  df <- getTableColumns(
    table   = "node",
    columns = c("meanAcc", "meanDecisionCoverage", "meanSupp"),
    network = network
  )

  # Helper to suppress messages
  quiet_createColumnFilter <- function(...) {
    suppressMessages({
      createColumnFilter(...)
    })
  }

  # --- meanAcc ---
  vals_acc <- na.omit(df$meanAcc)
  if (!length(vals_acc)) stop("No values for mean accuracy")

  quiet_createColumnFilter(
    filter.name = "Mean accuracy filter",
    column      = "meanAcc",
    criterion   = range(vals_acc),
    predicate   = "BETWEEN",
    type        = "nodes",
    network     = network,
    apply       = FALSE
  )

  # --- meanDecisionCoverage ---
  vals_deccov <- na.omit(df$meanDecisionCoverage)
  if (!length(vals_deccov)) stop("No values for Mean decision coverage")

  quiet_createColumnFilter(
    filter.name = "Mean decision coverage filter",
    column      = "meanDecisionCoverage",
    criterion   = range(vals_deccov),
    predicate   = "BETWEEN",
    type        = "nodes",
    network     = network,
    apply       = FALSE
  )

  # --- meanSupp ---
  vals_supp <- na.omit(df$meanSupp)
  if (!length(vals_supp)) stop("No values for mean support")

  quiet_createColumnFilter(
    filter.name = "Mean support filter",
    column      = "meanSupp",
    criterion   = range(vals_supp),
    predicate   = "BETWEEN",
    type        = "nodes",
    network     = network,
    apply       = FALSE
  )

  # --- Composite filter ---
  createCompositeFilter(
    filter.name = "meanAcc + meanDecisionCoverage + meanSupp",
    filter.list = c(
      "Mean accuracy filter",
      "Mean support filter",
      "Mean decision coverage filter"
    ),
    type    = "ALL",
    hide    = FALSE,
    network = network,
    apply   = FALSE
  )

  invisible(TRUE)
}
