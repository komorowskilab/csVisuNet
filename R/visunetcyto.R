#' VisuNet: an interactive tool for network visualization of rule-based models in R
#'

#' VisuNet is an interactive tool for network visualization of complex rule-based classifiers. See the \href{https://komorowskilab.github.io/VisuNet/}{documentation}.
#' @import visNetwork shiny shinythemes R.ROSETTA
# --- Elsa: Import functions for GO annotations ---
#' @importFrom clusterProfiler bitr groupGO
#' @importFrom GO.db Term
#' @importFrom tidyr separate_rows
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
#' \item from, to - the pair of nodes that create the edge
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
#' vis_out <- visunet(rules, type = "L")
#'

visunetcyto = function(ruleSet, title="VisuNet_Networks", type ="RDF",
                       NodeColorType = "DL", NodeSizeMetric = "DC", EdgeColor = 'R',
                       EdgeWidth=10, CustObjectNodes=list(), CustObjectEdges=list(),
                       addGO = FALSE, GO_ontology = "MF", NodeSize = "sum",
                       NodeSizeScale=c(15,50), NodeBorderScale=c(1,12),
                       EdgeWidthScale=c(1,6),
                       minAcc=-1, minSupp=-1, minDecisionCoverage=-1, style=TRUE){
  rules <- ruleSet
  rules <-  data_input(rules, type)
  rules_10per_param <-  filtration_rules_10per(rules)
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
  FiltrParam = NodeSizeMetric

  if (FiltrParam == "DC"){
    value_slider = minDecisionCoverage
    message("Using Decision Coverage and accuracy for filter")
  } else {
    value_slider = minSupp
    message("Using Support and accuracy for filter")
    message("")
  }

  TopNodes = 0

  decs = unique(as.matrix(rules$decision))
  validate(
    filter_rules(rules, minAcc, minSupp, FiltrParam, value_slider)
  )
  RulesFiltr =  filtration_rules(rules, minAcc, FiltrParam, value_slider)
  data = generate_object(decs, RulesFiltr, type, TopNodes, FiltrParam, NodeColorType, EdgeColor, EdgeWidth, CustObjectNodes, CustObjectEdges, NodeSize)
  if(addGO) {
    message("Adding Go Annotations")
    message("")
    suppressMessages({
      data <- addGOannotations(data, GO_ontology)
    })
  }

  # New stuff for cytoscape below, above uses original code to extract networks

  clearCollection(title)

  net_name="all"
  network <- data[[net_name]]
  message("Loading data...")
  message("")
  network <- restructureNetworkDF(network, NodeSizeScale, NodeBorderScale, EdgeWidthScale, addGO)
  suppressMessages({
    net_suid <- createNetworkFromDataFrames(network$nodes,network$edges, title=net_name, collection=title)
  })

     #styles
  style_name = paste(title, net_name, '_style')
  if (style | !(style_name %in% getVisualStyleNames())){
    message("Creating style...")
    message("")
    createStyle(style_name, network)


  }
  message("Applying style...")
  message("")
  setVisualStyle(style_name)



  #filters -- New --
  #filter_slides(network = net_suid)

  for (net_name in names(data)) {
    if (net_name!="all"){
      edge_ids = data[[net_name]]$edges$id
      createSubnetwork(edges=edge_ids, edges.by.col = "id",
                       subnetwork.name=net_name, network=net_suid)
    }
  }
  return(data)
}
