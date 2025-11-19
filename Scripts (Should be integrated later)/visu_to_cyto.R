library(RCy3)
library(devtools)

load_all("/Users/ibraheem/Desktop/visu_to_cyto/VisuNet.v.1.1")

# Skapar nätverk i Cytoscape från VisuNet regler
visunetcyto <- function(ruleSet, title,
                        type ="RDF",  NodeColorType = "DL", NodeSizeMetric = "DC",
                        EdgeColor = 'R', EdgeWidth = 10,
                        CustObjectNodes = list(), CustObjectEdges = list(),
                        addGO = FALSE, GO_ontology = "MF", NodeSize = "sum") {
  
  # Förbered regler
  rules <- ruleSet
  rules <- data_input(rules, type)
  rules_10per_param <- filtration_rules_10per(rules)
  
  # Gränser för filtrering
  minAcc <- 0
  minSupp <- rules_10per_param$minSupp
  minDecisionCoverage <- rules_10per_param$minDecisionCoverage
  filter_value <- if (minDecisionCoverage == 0) minSupp else minDecisionCoverage
  if (minDecisionCoverage == 0) NodeSizeMetric <- "S"
  
  TopNodes <- 0  
  decs <- unique(as.matrix(rules$decision))  # unika beslut
  
  # Filtrera regler och bygg objekt för nätverk
  RulesFiltr <- filtration_rules(rules, minAcc, NodeSizeMetric, filter_value)
  data <- generate_object(decs, RulesFiltr, type, TopNodes,
                          NodeSizeMetric, NodeColorType, EdgeColor, EdgeWidth,
                          CustObjectNodes, CustObjectEdges, NodeSize)
  
  if (addGO) data <- addGOannotations(data, GO_ontology)
  
  clearCollection(title)
  
  # Spara id:n för autism- och control-noder (om de finns)
  ids_autism <- if (!is.null(data$autism) && !is.null(data$autism$nodes)) {
    data$autism$nodes$id
  } else {
    character(0)
  }
  
  ids_control <- if (!is.null(data$control) && !is.null(data$control$nodes)) {
    data$control$nodes$id
  } else {
    character(0)
  }
  
  # Byter kolumnnamn från/to till source/target
  rename_edges <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(df)
    names(df)[names(df) == "from"] <- "source"
    names(df)[names(df) == "to"]   <- "target"
    df
  }
  
  # Säkerställ att varje nod har ett beslut (decision)
  ensure_decision <- function(nodes, net_name) {
    # Om decision redan finns, gör inget
    if (any(names(nodes) == "decision")) return(nodes)
    
    # Om hela nätverket är autism eller control
    if (net_name == "autism" || net_name == "control") {
      nodes$decision <- net_name
      return(nodes)
    }
    
    # Kolla om nod-id finns i autism- eller control-listorna
    in_autism  <- match(nodes$id, ids_autism,  nomatch = 0) > 0
    in_control <- match(nodes$id, ids_control, nomatch = 0) > 0
    
    # Standard: blandat
    nodes$decision <- "mixed"
    # Endast autism
    nodes$decision[in_autism  & !in_control] <- "autism"
    # Endast control
    nodes$decision[in_control & !in_autism]  <- "control"
    
    nodes
  }
  
  # Gå igenom varje nätverk i data-listan
  for (net_name in names(data)) {
    
    network <- data[[net_name]]
    
    network <- if (exists("restructureNetworkDF")) restructureNetworkDF(network) else network
    
    network$edges <- rename_edges(network$edges)
    network$nodes <- ensure_decision(network$nodes, net_name)
    
    # Skapa nätverket i Cytoscape
    net_suid <- createNetworkFromDataFrames(network$nodes, network$edges,
                                            title = net_name, collection = title)
    
    # Skapa och sätt stil
    style_name <- paste(title, net_name, "_style")
    createStyle(style_name, network)
    setVisualStyle(style_name)
    
    # Sliders för filtrering i Cytoscape
    makeMeanAccuracySlider(network = net_suid)
    makeMeanSupportSlider(network = net_suid)
    makeMeanDecCoverageSlider(network = net_suid)
    
    # Kombinera filtren 
    createCompositeFilter(
      filter.name = "meanAcc + meanSupp + meanDecisionCoverage",
      filter.list = c("meanAcc filter","Mean support filter","Mean decision coverage filter"),
      type = "ALL", hide = FALSE, network = net_suid, apply = FALSE
    )
  }
}
