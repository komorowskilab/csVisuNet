generate_object = function(decs, rules, type, TopNodes, FiltrParam,
                           NodeColorType, EdgeColor, EdgeWidth,
                           NewDataNodes, NewDataEdges, NodeSize){
  AllNets = NULL
  Net = NULL
  for (i in decs){
    # --- Elsa: removed recRulesDecs (unused) ---
    RulesDec = rules[which(rules$decision == i), ]
    RulesSetSize = dim(RulesDec)[1]
    if (RulesSetSize != 0){
      Net = generateNet(i, RulesDec, type, RulesSetSize, TopNodes, FiltrParam,
                        NodeColorType = NodeColorType, EdgeColor, EdgeWidth,
                        NewDataNodes, NewDataEdges, NodeSize)
    } else {
      # --- Elsa: changed NodeRulesSetPerNode -> RulesSetPerNode ---
      Net = list(nodes = NULL, edges = NULL, RulesSetPerNode = NULL)
    }
    AllNets[[i]] = Net
  }
  
  # --- Elsa: replaced 'generateNet(all, ...)' call with merged combination of existing networks ---
  if (length(decs) > 0) {
    all_nodes <- NULL
    all_edges <- NULL
    all_rulesets <- list()
    
    for (d in decs) {
      if (!is.null(AllNets[[d]]$nodes)) {
        all_nodes <- rbind(all_nodes, AllNets[[d]]$nodes)
      }
      if (!is.null(AllNets[[d]]$edges)) {
        all_edges <- rbind(all_edges, AllNets[[d]]$edges)
      }
      if (!is.null(AllNets[[d]]$RulesSetPerNode)) {
        all_rulesets <- c(all_rulesets, AllNets[[d]]$RulesSetPerNode)
      }
    }
    
    AllNets[['all']] <- list(nodes = all_nodes,
                             edges = all_edges,
                             RulesSetPerNode = all_rulesets)
  } else {
    AllNets[['all']] <- list(nodes = NULL, edges = NULL, RulesSetPerNode = NULL)
  }
  
  return(AllNets)
}

# --- OLD VERSION ---
# generate_object = function(decs, rules,type,  TopNodes, FiltrParam,  NodeColorType,  EdgeColor, EdgeWidth, NewDataNodes, NewDataEdges){
#   
#   AllNets = NULL
#   Net = NULL
#   for (i in decs){
#     #i = "control"
#     recRulesDecs = NULL
#     #rules for decision
#     RulesDec = rules[which(rules$decision ==i),]
#     #generate nodes and edges for decision
#     RulesSetSize=dim(RulesDec)[1]
#     if(RulesSetSize != 0){
#       Net = generateNet(i, RulesDec, type, RulesSetSize, TopNodes,FiltrParam,  NodeColorType = NodeColorType,EdgeColor, EdgeWidth, NewDataNodes, NewDataEdges)
#     }else{
#       Net = list(nodes = NULL, edges = NULL, NodeRulesSetPerNode = NULL)
#     }
#     
#     AllNets[[i]] = Net
#     Net =  NULL
#   }
#   if(dim(rules)[1] != 0){
#     AllNets[['all']] = generateNet('all', rules, type, dim(recRulesFiltr)[1], TopNodes, FiltrParam,  NodeColorType = NodeColorType, EdgeColor, EdgeWidth,
#                                    NewDataNodes, NewDataEdges)
#   }else{
#     AllNets[['all']] = list(nodes = NULL, edges = NULL, NodeRulesSetPerNode = NULL)
#   }
#   return(AllNets)
# }