#' VisuNet: an interactive tool for network visualization of rule-based models in R
#'

#' VisuNet is an interactive tool for network visualization of complex rule-based classifiers. See the \href{https://komorowskilab.github.io/VisuNet/}{documentation}.
#' @import visNetwork shiny shinythemes R.ROSETTA
#' @param ruleSet the appropriately formatted set of rules:
#'\itemize{
#' \item R.ROSETTA data frame - the rules data frame that is the output of R.ROSETTA can be directly imported in VisuNet.
#' See \code{\link[R.ROSETTA]{rosetta}} for details.
#' \item "Line by line" file format - input data should be in a data frame format that contains the following columns:
#'\itemize{
#'\item features - the left-hand side of the rule corresponding to comma-separated attributes and their values, of type, type "factor"
#'\item decision - the right-hand side of the rule corresponding to the decision value, of type "factor"
#'\item accuracyRHS - the rule accuracy, of type "numeric"
#'\item supportRHS - the rule support, of type "numeric"
#'\item decisionCoverage or coverageRHS - the fraction of correcly classfied objects, of type "numeric"
#'}
#'}
#'
#' @param type  a character string specifying the type of the input data:
#' \itemize{
#' \item "RDF" - the R.ROSETTA output (see \code{\link[R.ROSETTA]{rosetta}})
#' \item "L" - the "Line by line" file format (see \code{\link[R.ROSETTA]{saveLineByLine}})
#'}
#'
#' @param NodeColorType a character string specifying the color of nodes:
#' \itemize{
#'   \item "DL" - feature discretization levels, option is available for data discretized into three levels: 1, 2 and 3.
#'   In the case of gene expression, data discretization levels correspond to: 1 - under-expressed gene, 2 - no change gene expression and 3 - over-expressed gene.
#'   \item "A" - color of nodes defined by the mean accuracy value for the node.
#' }
#'The default is "DL".
#'
# -- Elsa suggestion: Update this part to sum if that is what we decide to use ---
#' @param NodeSizeMetric a character string specifying the metric used for node filtering:
#' \itemize{
#'   \item "DC" - the mean decision coverage for the feature
#'   \item "S" - the mean support for the feature
#' }
#' If the decision coverage value is unavailable, the support is taken by default.
#'
#' @param CustObjectNodes a list that contains the customized VisuNet output for nodes. The list needs to contain two variables:
#'\itemize{
#'   \item nodes - a customized VisuNet output for nodes
#'   \item CustCol - the names of variables added/changed in the VisuNet output for nodes.
#'   See \code{\link[visNetwork]{visNodes}} for details.
#' }
#'
#' @param CustObjectEdges  a list that contains customized VisuNet output for edges.
#' The list needs to contain two variables:
#'\itemize{
#'   \item edges - a customized VisuNet output for edges
#'   \item CustCol - the names of variables added/changed in the VisuNet output for edges.
#'   See \code{\link[visNetwork]{visEdges}} for details.
#' }
#'
# -- Elsa: new documentaiton comments  ---
#' @param addGO a logical value indicating whether to add Gene Ontology (GO) annotations to nodes.
#' Default is FALSE. Requires clusterProfiler, org.Hs.eg.db, and GO.db packages.
#'
#' @param GO_ontology Ontology to use for GO analysis. Options: "MF" (Molecular Function),
#' "BP" (Biological Process), "CC" (Cellular Component). Default: "MF"
#'
#' @param NodeSize a character string specifying the aggregation method for node size calculation:
#' \itemize{
#'   \item "mx" - use the maximum value of support/decision coverage for the node
#'   \item "mean" - use the mean value of support/decision coverage for the node
#'   \item "sum" - use the sum of support/decision coverage for the node (default)
#' }
#'
#' @param NodeSizeScale a Vector containing the min and max values for the size of the Nodes in the visualization:
#' Default is c(15,50)
#'
#' @param NodeBorderScale a Vector containing the min and max values for the thickness of the node border in the visualization:
#' Default is c(1,12)
#'
#' @param EdgeWidthScale a Vector containing the min and max values for the thickness of the edges in the visualization:
#' Default is c(1,6)
#'
#' @param minAcc threshold for the minimum Accuracy filtering:
#' Default is -1, which sets the threshold to include the top 10% of rules
#'
#' @param minSupp threshold for the minimum Support filtering:
#' Default is -1, which sets the threshold to include the top 10% of rules
#'
#' @param minDecisionCoverage threshold for the minimum Decision Coverage filtering:
#' Default is -1, which sets the threshold to include the top 10% of rules
#'
#' @param TopNodes set the amount of nodes to showcase:
#' Default is 0, which uses all of the Nodes
#'
#' @param style Boolean that controls whether cytoscape should create a styling or not:
#' Default is TRUE, needs to be TRUE the first time the function is run
#' Is used to speed up the process if the filtering needs to be redone
#'
#'@references
#' See the \href{https://komorowskilab.github.io/VisuNet/}{documentation} for more details and examples.
#'
#' @return Rule Network Object - a collection of lists corresponding to decision variables and an additional list for the combined decision ‘all’.
#' The lists contain information required to reproduce the rule network, i.e. data frames for nodes, edges
#' and RulesSetPerNode - a list that shows rules for each node.
#' \cr
#' \cr
#' Structure of the data frame for nodes:
#' \itemize{
#' \item id - a unique node id, based on attribute value and left-hand side value of the rule set
#' \item label - the attribute variable without the ‘=value’ part from the left-hand side of the rule set
#' \item DiscState - the attribute value
#' \item color.background - the node color
#' \item value - the node size
#' \item color.border - the color of the node border
#' \item meanAcc - the mean accuracy value of all rules that contain the node
#' \item meanSupp - the mean support value of all rules that contain the node
#' \item NRules - the number of rules that contain the node
#' \item PrecRules - fraction of rules that contain the node
#' \item NodeConnection - the total connection value obtained from the rules that contain the node
#' \item title - information visible on the tooltip
#' \item group - the decision value that occurs most frequently (>50%) in rules associated with the node;
#' otherwise group contains all comma-separated decision values corresponding to rules associated with the node.
#' group defines the content of the ‘Select by decision’ drop-down box.
#' }
#'
#' \cr
#' Structure of the data frame for edges:
#' \itemize{
#' \item source, target - the pair of nodes that create the edge
#' \item conn - the connection variable obtained from the edge-associated rules.
#' \item connNorm - the connection variable normalized according to the maximum connection variable in the rule network
#' \item label2 - the edge id
#' \item color - the edge color
#' \item title - information visible on the tooltip
#' \item width - the edge width, defined according to the normalized connection value

#'
#' }
#' @keywords misc
#' @export
#' @examples
#'
#' #The R.ROSETTA output format
#' #the rule-based model construction using R.ROSETTA
#' resultsRos <- rosetta(autcon)
#' vis_out <- visunet(resultsRos$main, type = "RDF")
#'------------
#'
#' #"Line by line" file format
#' rules <- autcon_ruleset
#'
#' visunetcyto(rules,minAcc = 0.8,minSupp = 6,minDecisionCoverage = 0.1)
#'
#'
# Function to extract rule properties and add to edges
add_rule_properties_to_edges <- function(network_data) {
  
  # Check if RulesSetPerNode exists
  if (!"RulesSetPerNode" %in% names(network_data)) {
    message("⚠ No RulesSetPerNode found in network data")
    return(network_data)
  }
  
  # Flatten all rules into a single dataframe
  all_rules <- do.call(rbind, lapply(names(network_data$RulesSetPerNode), function(node_name) {
    rules <- network_data$RulesSetPerNode[[node_name]]
    if (nrow(rules) > 0) {
      rules$node <- node_name
      return(rules)
    }
    return(NULL)
  }))
  
  message("Total rules found: ", nrow(all_rules))
  message("Total edges: ", nrow(network_data$edges))
  
  # For each edge, try to find matching rules and aggregate their properties
  edges_with_rules <- network_data$edges
  
  # Add columns for rule properties
  edges_with_rules$min_accuracy <- NA
  edges_with_rules$max_accuracy <- NA
  edges_with_rules$mean_accuracy <- NA
  edges_with_rules$min_support <- NA
  edges_with_rules$max_support <- NA
  edges_with_rules$mean_support <- NA
  edges_with_rules$min_pValue <- NA
  edges_with_rules$max_pValue <- NA
  edges_with_rules$mean_pValue <- NA
  edges_with_rules$min_length <- NA
  edges_with_rules$max_length <- NA
  edges_with_rules$n_rules <- 0
  
  # For each edge, find rules that involve both nodes
  for (i in 1:nrow(edges_with_rules)) {
    from_node <- edges_with_rules$from[i]
    to_node <- edges_with_rules$to[i]
    
    # Find rules mentioning both nodes
    # Rules are stored under each node, so check both
    matching_rules <- all_rules[all_rules$node == from_node | all_rules$node == to_node, ]
    
    if (nrow(matching_rules) > 0) {
      edges_with_rules$min_accuracy[i] <- min(matching_rules$accuracy, na.rm = TRUE)
      edges_with_rules$max_accuracy[i] <- max(matching_rules$accuracy, na.rm = TRUE)
      edges_with_rules$mean_accuracy[i] <- mean(matching_rules$accuracy, na.rm = TRUE)
      
      edges_with_rules$min_support[i] <- min(matching_rules$support, na.rm = TRUE)
      edges_with_rules$max_support[i] <- max(matching_rules$support, na.rm = TRUE)
      edges_with_rules$mean_support[i] <- mean(matching_rules$support, na.rm = TRUE)
      
      edges_with_rules$min_pValue[i] <- min(matching_rules$pValue, na.rm = TRUE)
      edges_with_rules$max_pValue[i] <- max(matching_rules$pValue, na.rm = TRUE)
      edges_with_rules$mean_pValue[i] <- mean(matching_rules$pValue, na.rm = TRUE)
      
      edges_with_rules$min_length[i] <- min(matching_rules$length, na.rm = TRUE)
      edges_with_rules$max_length[i] <- max(matching_rules$length, na.rm = TRUE)
      
      edges_with_rules$n_rules[i] <- nrow(matching_rules)
    }
  }
  
  message("✓ Added rule properties to ", sum(edges_with_rules$n_rules > 0), " edges")
  
  network_data$edges <- edges_with_rules
  return(network_data)
}
visunetcyto_dev = function(ruleSet, title="VisuNet_Networks", type ="RDF",
                           NodeColorType = "DL", NodeSizeMetric = "DC", EdgeColor = 'R',
                           EdgeWidth=10, CustObjectNodes=list(), CustObjectEdges=list(),
                           addGO = FALSE, GO_ontology = "MF", NodeSize = "sum",
                           NodeSizeScale=c(15,50), NodeBorderScale=c(1,12),
                           EdgeWidthScale=c(1,6),
                           minAcc=-1, minSupp=-1, minDecisionCoverage=-1, style=TRUE, TopNodes=0){
  
  rules <- ruleSet
  rules <-  data_input(rules, type)
  rules_10per_param <-  filtration_rules_10per(rules)
  
  # Calculate default thresholds
  if (minAcc == -1){
    minAcc <-  rules_10per_param$minAcc
    message(paste0("Default minimum accuracy is ", minAcc))
  }
  if (minSupp == -1){
    minSupp <-  rules_10per_param$minSupp
    message(paste0("Default minimum support is ", minSupp))
  }
  if (minDecisionCoverage == -1){
    minDecisionCoverage <- rules_10per_param$minDecisionCoverage
    message(paste0("Default minimum DecisionCoverage is ", minDecisionCoverage))
    message("")
  }
  
  value_slider = minDecisionCoverage
  if (NodeSizeMetric == "DC"){
    value_slider = minDecisionCoverage
    message("Using Decision Coverage and accuracy for filter")
    FiltrParam = 'Min Decision Coverage'
  } else {
    value_slider = minSupp
    FiltrParam = "Min Support"
    message("Using Support and accuracy for filter")
    message("")
  }
  
  decs = unique(as.matrix(rules$decision))
  
  # Generate data for ALL networks WITHOUT pre-filtering
  data = generate_object(decs, rules, type, TopNodes, FiltrParam, NodeColorType, 
                         EdgeColor, EdgeWidth, CustObjectNodes, CustObjectEdges, NodeSize)
  
  if(addGO) {
    message("Adding Go Annotations")
    message("")
    suppressMessages({
      data <- addGOannotations(data, GO_ontology)
    })
  }
  
  # Create Cytoscape networks
  clearCollection(title)
  
  net_name <- "all"
  network <<- data[[net_name]]
  message("Loading ALL data (unfiltered)...")
  message("")
  
  # ============================================================
  # ADD RULE PROPERTIES TO EDGES
  # ============================================================
  message("=== ADDING RULE PROPERTIES TO EDGES ===")
  
  # Check if RulesSetPerNode exists
  if ("RulesSetPerNode" %in% names(network)) {
    
    # Flatten all rules into a single dataframe
    all_rules <- do.call(rbind, lapply(names(network$RulesSetPerNode), function(node_name) {
      rules_df <- network$RulesSetPerNode[[node_name]]
      if (nrow(rules_df) > 0) {
        rules_df$node <- node_name
        return(rules_df)
      }
      return(NULL)
    }))
    
    message("Total rules found: ", nrow(all_rules))
    message("Total edges: ", nrow(network$edges))
    
    # Add columns for rule properties to edges
    network$edges$min_accuracy <- NA
    network$edges$max_accuracy <- NA
    network$edges$mean_accuracy <- NA
    network$edges$min_support <- NA
    network$edges$max_support <- NA
    network$edges$mean_support <- NA
    network$edges$min_pValue <- NA
    network$edges$max_pValue <- NA
    network$edges$mean_pValue <- NA
    network$edges$min_length <- NA
    network$edges$max_length <- NA
    network$edges$n_rules <- 0
    
    # For each edge, find matching rules and aggregate their properties
    for (i in 1:nrow(network$edges)) {
      from_node <- network$edges$from[i]
      to_node <- network$edges$to[i]
      
      # Find rules mentioning either node (rules stored under each node)
      matching_rules <- all_rules[all_rules$node == from_node | all_rules$node == to_node, ]
      
      if (nrow(matching_rules) > 0) {
        network$edges$min_accuracy[i] <- min(matching_rules$accuracy, na.rm = TRUE)
        network$edges$max_accuracy[i] <- max(matching_rules$accuracy, na.rm = TRUE)
        network$edges$mean_accuracy[i] <- mean(matching_rules$accuracy, na.rm = TRUE)
        
        network$edges$min_support[i] <- min(matching_rules$support, na.rm = TRUE)
        network$edges$max_support[i] <- max(matching_rules$support, na.rm = TRUE)
        network$edges$mean_support[i] <- mean(matching_rules$support, na.rm = TRUE)
        
        network$edges$min_pValue[i] <- min(matching_rules$pValue, na.rm = TRUE)
        network$edges$max_pValue[i] <- max(matching_rules$pValue, na.rm = TRUE)
        network$edges$mean_pValue[i] <- mean(matching_rules$pValue, na.rm = TRUE)
        
        network$edges$min_length[i] <- min(matching_rules$length, na.rm = TRUE)
        network$edges$max_length[i] <- max(matching_rules$length, na.rm = TRUE)
        
        network$edges$n_rules[i] <- nrow(matching_rules)
      }
    }
    
    message("✓ Added rule properties to ", sum(network$edges$n_rules > 0), " edges")
    message("")
    message("New edge columns: min_accuracy, max_accuracy, mean_accuracy,")
    message("                  min_support, max_support, mean_support,")
    message("                  min_pValue, max_pValue, mean_pValue,")
    message("                  min_length, max_length, n_rules")
    
  } else {
    message("⚠ No 'RulesSetPerNode' found - skipping rule property extraction")
  }
  
  message("")
  
  # ============================================================
  # RESTRUCTURE AND CREATE NETWORK
  # ============================================================
  
  # DIAGNOSTIC: Check edge columns BEFORE restructure
  message("=== BEFORE restructureNetworkDF ===")
  message("Edge columns: ", paste(names(network$edges), collapse = ", "))
  message("")
  
  network <- restructureNetworkDF(network, NodeSizeScale, NodeBorderScale, EdgeWidthScale, addGO)
  
  # DIAGNOSTIC: Check edge columns AFTER restructure
  message("=== AFTER restructureNetworkDF ===")
  message("Edge columns: ", paste(names(network$edges), collapse = ", "))
  message("")
  
  # Check if rule columns survived
  rule_cols <- c("mean_accuracy", "mean_support", "mean_pValue", "n_rules")
  survived <- rule_cols[rule_cols %in% names(network$edges)]
  missing <- rule_cols[!rule_cols %in% names(network$edges)]
  
  if (length(missing) > 0) {
    message("⚠ WARNING: Rule columns REMOVED by restructureNetworkDF:")
    message("  Lost: ", paste(missing, collapse = ", "))
    message("")
  }
  
  if (length(survived) > 0) {
    message("✓ Rule columns preserved: ", paste(survived, collapse = ", "))
    message("")
  }
  
  suppressMessages({
    net_suid <- createNetworkFromDataFrames(network$nodes, network$edges, 
                                            title=net_name, collection=title)
  })
  
  # Verify edge columns in Cytoscape
  message("=== VERIFYING EDGE TABLE IN CYTOSCAPE ===")
  edge_cols_in_cytoscape <- getTableColumnNames(table = "edge", network = net_suid)
  message("Edge columns in Cytoscape:")
  message("  ", paste(edge_cols_in_cytoscape, collapse = ", "))
  message("")
  
  # ============================================================
  # ADD NETWORK TABLE DATA
  # ============================================================
  message("=== POPULATING NETWORK TABLE ===")
  
  network_data <- data.frame(
    name = net_name,
    minAccuracy = minAcc,
    minSupport = minSupp,
    minDecisionCoverage = minDecisionCoverage,
    filterType = FiltrParam,
    totalNodes = nrow(network$nodes),
    totalEdges = nrow(network$edges),
    stringsAsFactors = FALSE
  )
  
  loadTableData(
    data = network_data,
    data.key.column = "name",
    table = "network",
    table.key.column = "name",
    network = net_suid
  )
  
  # ============================================================
  # APPLY STYLES
  # ============================================================
  style_name <- paste(title, net_name, '_style')
  if (style | !(style_name %in% getVisualStyleNames())){
    message("Creating style...")
    createStyle(style_name, network)
  }
  message("Applying style...")
  setVisualStyle(style_name)
  
  message("Applying layout...")
  layoutNetwork("force-directed-cl", network)
  
  # ============================================================
  # CREATE EDGE-BASED RULE FILTERS
  # ============================================================
  message("")
  message("=== CREATING EDGE-BASED RULE FILTERS ===")
  
  filter_result <- tryCatch({
    
    quiet_createColumnFilter <- function(...) {
      suppressMessages(createColumnFilter(...))
    }
    
    created_filters <- c()
    
    # --- Filter: mean_accuracy ---
    if ("mean_accuracy" %in% edge_cols_in_cytoscape) {
      edge_df <- getTableColumns(table = "edge", columns = "mean_accuracy", network = net_suid)
      vals <- na.omit(edge_df$mean_accuracy)
      
      if (length(vals) > 0) {
        max_val <- max(vals)
        actual_min <- max(minAcc, min(vals))
        
        quiet_createColumnFilter(
          filter.name = "Edge mean accuracy",
          column      = "mean_accuracy",
          criterion   = c(actual_min, max_val),
          predicate   = "BETWEEN",
          type        = "edges",
          network     = net_suid,
          apply       = FALSE
        )
        created_filters <- c(created_filters, "Edge mean accuracy")
        message("✓ Created edge:mean_accuracy filter (", round(actual_min, 4), " to ", round(max_val, 4), ")")
      }
    } else {
      message("⚠ Column 'mean_accuracy' not found in edge table")
    }
    
    # --- Filter: mean_support ---
    if ("mean_support" %in% edge_cols_in_cytoscape) {
      edge_df <- getTableColumns(table = "edge", columns = "mean_support", network = net_suid)
      vals <- na.omit(edge_df$mean_support)
      
      if (length(vals) > 0) {
        max_val <- max(vals)
        actual_min <- max(minSupp, min(vals))
        
        quiet_createColumnFilter(
          filter.name = "Edge mean support",
          column      = "mean_support",
          criterion   = c(actual_min, max_val),
          predicate   = "BETWEEN",
          type        = "edges",
          network     = net_suid,
          apply       = FALSE
        )
        created_filters <- c(created_filters, "Edge mean support")
        message("✓ Created edge:mean_support filter (", round(actual_min, 2), " to ", round(max_val, 2), ")")
      }
    } else {
      message("⚠ Column 'mean_support' not found in edge table")
    }
    
    # --- Filter: mean_pValue ---
    if ("mean_pValue" %in% edge_cols_in_cytoscape) {
      edge_df <- getTableColumns(table = "edge", columns = "mean_pValue", network = net_suid)
      vals <- na.omit(edge_df$mean_pValue)
      
      if (length(vals) > 0) {
        min_val <- min(vals)
        actual_max <- min(0.05, max(vals))
        
        quiet_createColumnFilter(
          filter.name = "Edge mean pValue",
          column      = "mean_pValue",
          criterion   = c(min_val, actual_max),
          predicate   = "BETWEEN",
          type        = "edges",
          network     = net_suid,
          apply       = FALSE
        )
        created_filters <- c(created_filters, "Edge mean pValue")
        message("✓ Created edge:mean_pValue filter (", signif(min_val, 3), " to ", signif(actual_max, 3), ")")
      }
    } else {
      message("⚠ Column 'mean_pValue' not found in edge table")
    }
    
    # --- Create composite filter ---
    if (length(created_filters) > 0) {
      createCompositeFilter(
        filter.name = "Rule edge filters (composite)",
        filter.list = created_filters,
        type        = "ALL",
        hide        = FALSE,
        network     = net_suid,
        apply       = FALSE
      )
      message("✓ Created composite edge filter")
    } else {
      message("⚠ No edge filters created - rule columns not found")
    }
    
    list(columns = edge_cols_in_cytoscape, filters = created_filters)
    
  }, error = function(e) {
    message("✗ Error creating edge filters: ", e$message)
    list(columns = edge_cols_in_cytoscape, filters = c())
  })
  
  # ============================================================
  # DISPLAY FILTER SUMMARY
  # ============================================================
  # Verify filters were created
  all_filters <- tryCatch({
    filters <- getFilterList()
    if (length(filters) > 0) {
      message("")
      message("✓ All available filters:")
      for (f in filters) {
        message("  • ", f)
      }
    }
    filters
  }, error = function(e) {
    message("Could not retrieve filter list: ", e$message)
    NULL
  })
  
  message("")
  message("╔════════════════════════════════════════════════════════════╗")
  message("║  EDGE-BASED RULE FILTERING ENABLED                        ║")
  message("╚════════════════════════════════════════════════════════════╝")
  message("")
  message("Filters operate on EDGES (individual rules)")
  message("")
  message("In Cytoscape:")
  message("  1. Select tab → 'Rule edge filters (composite)'")
  message("  2. Adjust sliders:")
  message("     • edge:mean_accuracy (rule accuracy)")
  message("     • edge:mean_support (rule support)")
  message("     • edge:mean_pValue (rule p-value)")
  message("  3. Hidden edges = rules NOT meeting filter criteria")
  message("")
  if (!is.null(filter_result) && length(filter_result$filters) > 0) {
    message("✓ Filters created: ", paste(filter_result$filters, collapse = ", "))
  } else {
    message("⚠ No filters were created - check edge columns")
  }
  message("")
  
  # ============================================================
  # CREATE SUBNETWORKS
  # ============================================================
  for (net_name in names(data)) {
    if (net_name != "all"){
      node_ids <- data[[net_name]]$nodes$id
      sub_suid <- createSubnetwork(
        nodes = node_ids, 
        nodes.by.col = "id",
        subnetwork.name = net_name, 
        network = net_suid
      )
      setCurrentNetwork(network = sub_suid)
      
      loadTableData(
        data = data.frame(
          name = net_name,
          minAccuracy = minAcc,
          minSupport = minSupp,
          minDecisionCoverage = minDecisionCoverage,
          parentNetwork = "all",
          stringsAsFactors = FALSE
        ),
        data.key.column = "name",
        table = "network",
        table.key.column = "name",
        network = sub_suid
      )
      
      positions <- getNodePosition(node.names = NULL)
      x_coords <- as.numeric(unlist(positions$x))
      y_coords <- as.numeric(unlist(positions$y))
      
      if (length(x_coords) > 0 && !any(is.na(x_coords))) {
        x_center <- (max(x_coords) + min(x_coords)) / 2
        y_top <- min(y_coords) - 100
        
        addAnnotationText(
          text = net_name,
          x.pos = x_center,
          y.pos = y_top,
          fontSize = 30
        )
      }
      fitContent()
    }
  }
  
  setCurrentNetwork(network = net_suid)
  message("")
  message("=== COMPLETE ===")
  message("")
  
  return(data)
}