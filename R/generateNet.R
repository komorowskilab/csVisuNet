generateNet=function(decs, rules, type, RulesSetSite, TopNodes,FiltrParam,
                     NodeColorType, EdgeColor, EdgeWidth, NewDataNodes, NewDataEdges, NodeSize){
  if(type == 'RDF'){
    vec = as.character(as.matrix(rules["features"]))
    lst1 = sapply(vec, function(x) strsplit(x, ",", fixed = TRUE))
    vec2 = as.character(as.matrix(rules["levels"]))
    lst2 = sapply(vec2, function(x) strsplit(x, ",", fixed = TRUE))
    newLst = mapply(paste, collapse = ",", sep = "=", lst1, lst2)
    NodeID = as.character(unname(newLst))
    rules$id=NodeID
  }else{
    rules$id = as.matrix(rules$features)
  }
  
  # Rule connection value
  rules$CONNECTION = rules$supportRHS * rules$accuracyRHS
  
  # Node information
  Nodes_vec=sapply(rules$id, function(x) strsplit(x, ","))
  NodeUniq=unique(unlist(Nodes_vec))
  NodeInfoDF = NULL
  NodeState = NULL
  meanAcc = NULL
  meanSupp = NULL
  meanDecisionCoverage = NULL
  NRules = NULL
  PrecRules = NULL
  NodeConnection = NULL
  NodeRulesSet = NULL
  DecisionSet = NULL
  # --- Elsa: Added new maxSupp, maxDecisionCoverage, sumSupp and sumDecisionCoverage ---
  maxSupp = NULL
  maxDecisionCoverage  = NULL
  sumSupp = NULL
  sumDecisionCoverage = NULL
  
  for (nod in NodeUniq){
    node_id = (which(lapply(Nodes_vec, function(x) length(which(x == nod))) !=0))
    
    # discrete state
    NodeState = c(NodeState,strsplit(nod, '=')[[1]][2])
    
    # mean accuracy (hover info)
    meanAcc = c(meanAcc, mean(rules[node_id,"accuracyRHS"]))
    
    # --- Elsa: added max and sum row 48-61, original version used only mean ---
    # mean + sum + max support
    meanSupp = c(meanSupp, mean(rules[node_id,"supportRHS"]))
    sumSupp  = c(sumSupp,  sum(rules[node_id,"supportRHS"]))
    maxSupp = c(maxSupp, max(rules[node_id,"supportRHS"]))
    
    # mean + sum decision coverage
    if("decisionCoverage" %in% colnames(rules[node_id,])) {
      meanDecisionCoverage = c(meanDecisionCoverage, mean(rules[node_id,"decisionCoverage"]))
      sumDecisionCoverage  = c(sumDecisionCoverage,  sum(rules[node_id,"decisionCoverage"]))
      maxDecisionCoverage = c(maxDecisionCoverage, max(rules[node_id,"decisionCoverage"]))
    } else {
      meanDecisionCoverage = c(meanDecisionCoverage, NA)
      sumDecisionCoverage  = c(sumDecisionCoverage, NA)
      maxDecisionCoverage = c(maxDecisionCoverage, NA)
    }
    
    # number of rules
    NRules = c(NRules, dim(rules[node_id,])[1])
    
    # % from rules in decision
    PrecRules = c(PrecRules, dim(rules[node_id,])[1] / dim(rules)[1] )
    
    # --- Elsa SUGGESTION: Node connectiion should be removed ---
    # Connection value
    NodeConnection = c(NodeConnection, sum(rules[node_id,]$CONNECTION *
                                             (unlist((lapply(Nodes_vec[node_id], length)))-1)))
    
    # Set of rules per Node
    if(type == 'L'){
      rules_RDF_fin <- suppressWarnings(Viewrules_type_L(rules, node_id))
      NodeRulesSet[[nod]] = viewRules(rules_RDF_fin)
    }else{
      NodeRulesSet[[nod]] = viewRules(rules[node_id,])
    }
    
    # dominant decision
    Dec_table <- sort(table(as.character(rules[node_id, "decision"])), decreasing=TRUE)
    if(Dec_table[1] > sum(Dec_table)*0.5){
      dec <- names(Dec_table)[1]
    }else{
      dec <- paste0(names(Dec_table), collapse=',')
    }
    DecisionSet = c(DecisionSet, dec)
    # --- Elsa: removed dec <-NULL and the uncommented line #DecisionSet = c(..) since both unused ---
  }
  
  # --- Elsa: same as before but comments are removed/changed ---
  NodeColor = NULL
  if(NodeColorType == 'DL'){
    NodeColor = rep('#999999', length(NodeUniq))
    NodeColor[which(NodeState == '1')] = '#56B4E9'
    NodeColor[which(NodeState == '3')] = '#E69F00'
    NodeLabel = unlist(lapply(NodeUniq, function(x) strsplit(x, '=')[[1]][1]))
  }else if(NodeColorType == 'A'){
    breaks=seq(0.7, 1, by=0.001)
    colFunc60_100 = colorRampPalette(c("#F1CDB0", "#D55E00"))
    colVec = colFunc60_100(length(breaks))
    breaks2 = seq(0,0.69,by=0.001)
    colFunc0_60 = colorRampPalette(c("white", "#F1CDB0"))
    colVec2 = colFunc0_60(length(breaks2))
    ColorMat = cbind(as.numeric(c(breaks2, breaks)),c(colVec2, colVec))
    NodeColor = ColorMat[match(round(meanAcc,2),ColorMat[,1]),2]
    NodeLabel = NodeUniq
  }else{
    print('The color schema value is wrong!')
  }
  
  # --- Elsa: Changed "Connection" to "Node connection" in hover menu, but (see below suggestion)---
  # --- Elsa: Changed to value=sumDecisionCoverage and value=sumSupp---
  # --- Elsa suggestion: Node connection should be removed, mean support/decision coverage should be replaced with sum support/decision coverage ---
  if (is.na(meanDecisionCoverage)[1] == FALSE){
    NodeTitle = paste0('Name: <b>', NodeUniq, '</b><br/>Edges: <b>', NRules,
                       '</b><br/>Node connection: <b>', round(NodeConnection,2),
                       '</b><br/>Mean accuracy: <b>', round(meanAcc,2),
                       '</b><br/>Mean support: <b>', round(meanSupp,2),
                       '</b><br/>Mean decision coverage: <b>', round(meanDecisionCoverage,2))
    
    NodeInfoDF = data.frame(
      id = NodeUniq, label = NodeLabel, DiscState = NodeState,
      color.background = NodeColor,
      value = sumDecisionCoverage,
      borderWidth = (PrecRules*20), color.border = c("#0072B2"),
      meanAcc = meanAcc, meanSupp = meanSupp, meanDecisionCoverage = meanDecisionCoverage,
      NRules = NRules, PrecRules = PrecRules,
      NodeConnection = NodeConnection, title = NodeTitle)
  } else {
    NodeTitle = paste0('Name: <b>', NodeUniq, '</b><br/>Edges: <b>', NRules,
                       '</b><br/>Node connection: <b>', round(NodeConnection,2),
                       '</b><br/>Mean accuracy: <b>', round(meanAcc,2),
                       '</b><br/>Mean support: <b>', round(meanSupp,2))
    NodeInfoDF = data.frame(
      id = NodeUniq, label = NodeLabel, DiscState = NodeState,
      color.background = NodeColor,
      value = sumSupp,
      borderWidth = (PrecRules*20), color.border = c("#0072B2"),
      meanAcc = meanAcc, meanSupp = meanSupp,
      NRules = NRules, PrecRules = PrecRules,
      NodeConnection = NodeConnection, title = NodeTitle
    )
  }
  # OlD version
  # # overwrite if filtering is on support vs coverage
  # if(FiltrParam != 'Min Decision Coverage'){
  #   NodeInfoDF$value <- sumSupp   # <-- SUM instead of mean
  # }
  
  # --- Node size calculation based on NodeSize ---
  if (FiltrParam != 'Min Decision Coverage') {
    # Use support values
    if (NodeSize == "mx") {
      NodeInfoDF$value <- maxSupp
    } else if (NodeSize == "mean") {
      NodeInfoDF$value <- meanSupp
    } else {  # default "sum"
      NodeInfoDF$value <- sumSupp
    }
  } else {
    # Use decision coverage values
    if (NodeSize == "mx") {
      NodeInfoDF$value <- maxDecisionCoverage
    } else if (NodeSize == "mean") {
      NodeInfoDF$value <- meanDecisionCoverage
    } else {  # default "sum"
      NodeInfoDF$value <- sumDecisionCoverage
    }
  }
  
  
  if(decs == 'all'){
    NodeInfoDF$group = DecisionSet
  }
  
  NodeInfoDF$font.size = 20

  # --- Elsa: Removed commented block of code that was here---
  
  NodeInfoDF = NodeInfoDF[order(NodeInfoDF$NodeConnection, decreasing = TRUE),]
  
  if(TopNodes != 0 & TopNodes <= dim(NodeInfoDF)[1]){
    NodeInfoDF = NodeInfoDF[1:TopNodes,]
  }

  
  # ---- EDGES ----
  AllRuleLen = (lapply(Nodes_vec, length))
  EdgesInfo = NULL
  if(length(which(AllRuleLen !=1)) != 0){
    rules2elem = which(AllRuleLen == 2)
    EdgesInfo2Ele=cbind(do.call(rbind,Nodes_vec[rules2elem]),
                        rules[rules2elem,c("CONNECTION")])
    
    rules3AndMoreElem = which(AllRuleLen > 2)
    if(!is.null(length(rules3AndMoreElem))){
      rules3AndMoreElemList = lapply(Nodes_vec[rules3AndMoreElem],
                                     function(x) matrix(x[combn(1:length(x), 2)],ncol = 2, byrow = TRUE))
      EdgesInfo3Ele = do.call(rbind,mapply('cbind',  rules3AndMoreElemList,
                                           (rules[rules3AndMoreElem,"CONNECTION"]), SIMPLIFY=FALSE))
      if(length(EdgesInfo2Ele) == 0){ 
        EdgesInfoAll=EdgesInfo3Ele 
      }else{  
        EdgesInfoAll=rbind(EdgesInfo2Ele, EdgesInfo3Ele)
      }
    }else{
      EdgesInfoAll=EdgesInfo2Ele
    }
    EdgesInfoTemp = as.data.frame(EdgesInfoAll)
    colnames(EdgesInfoTemp) = c('from' , 'to' , 'conn')
    EdgesInfoAllSort=t(apply(subset(EdgesInfoTemp, select=c("from", "to")), 1, sort))
    colnames(EdgesInfoAllSort) = c('from' , 'to')
    EdgesInfoAllSort2=data.frame(EdgesInfoAllSort,'conn' = EdgesInfoTemp$conn )
    EdgesInfo_tmp <- aggregate(EdgesInfoAllSort2,
                               by=list(EdgesInfoAllSort2$from,EdgesInfoAllSort2$to),
                               FUN= function(x) sum(as.numeric(x)))
    
    EdgesInfo = EdgesInfo_tmp[,-c(3,4)]
    colnames(EdgesInfo) = c('from' , 'to' , 'conn')
    
    if(dim(EdgesInfo)[1] == 1 ) {
      EdgesInfo$connNorm = 1 
    } else {
      EdgesInfo$connNorm = ((EdgesInfo$conn-min(EdgesInfo$conn))/
                              (max(EdgesInfo$conn)-min(EdgesInfo$conn)))
    }
    EdgesInfo$label2 = paste0(EdgesInfo$from, '-', EdgesInfo$to )
    if(EdgeColor=='B'){
      EdgesInfo$color = rep('#c2c2c2', length(EdgesInfo$connNorm))
      EdgesInfo$color[which(EdgesInfo$connNorm >= 0.85)] = '#000000'
      EdgesInfo$color[which(EdgesInfo$connNorm < 0.85 & EdgesInfo$connNorm >= 0.7)] = '#666666'
      EdgesInfo$color[which(EdgesInfo$connNorm < 0.7 & EdgesInfo$connNorm >= 0.55)] = '#999999'
      EdgesInfo$width  = (EdgesInfo$connNorm *EdgeWidth)
    }else{
      EdgesInfo$color = rep('#c2c2c2', length(EdgesInfo$connNorm))
      EdgesInfo$color[which(EdgesInfo$connNorm >= 0.85)] = '#ea1d1d'
      EdgesInfo$color[which(EdgesInfo$connNorm < 0.85 & EdgesInfo$connNorm >= 0.7)] = '#d86431'
      EdgesInfo$color[which(EdgesInfo$connNorm < 0.7 & EdgesInfo$connNorm >= 0.55)] = '#dbcb33'
      EdgesInfo$width  = (EdgesInfo$connNorm *EdgeWidth)
    }
    # --- Elsa: Removed commented block that was here---
    EdgesTile = paste0('Edge: <b>', EdgesInfo$from, ', ', EdgesInfo$to, '</b><br/>Connection: <b>', round(EdgesInfo$conn,2), '</b>')
    EdgesInfo$title = EdgesTile
  }
  
  if(length(NewDataNodes)>0){
    NewDataNodesDF = NewDataNodes$nodes
    CustCol = NewDataNodes$CustCol
    NEWNodeInfoDF = NodeInfoDF
    ind_Col = which(CustCol %in% colnames(NEWNodeInfoDF))
    NewDataNodesDF$id = as.character(unlist(NewDataNodesDF$id))
    NEWNodeInfoDF$id = as.character(unlist(NEWNodeInfoDF$id))
    int_id = intersect((NewDataNodesDF$id), (NEWNodeInfoDF$id))
    NewDataNodesDF_int =  NewDataNodesDF[(which(as.character(NewDataNodesDF$id) %in% int_id)),]
    if(length(ind_Col)>0){
      for(i in 1:length(ind_Col)){
        NEWNodeInfoDF[CustCol[ind_Col[i]]] = as.character(unlist(NEWNodeInfoDF[CustCol[ind_Col[i]]]))
        NewDataNodesDF[CustCol[ind_Col[i]]] = as.character(unlist(NewDataNodesDF[CustCol[ind_Col[i]]]))
        NEWNodeInfoDF[match(NewDataNodesDF_int$id, as.character(NEWNodeInfoDF$id)),
                      CustCol[ind_Col[i]]] = NewDataNodesDF_int[CustCol[ind_Col[i]]]
      }
      ind_Col_diff = CustCol[setdiff(seq(1, length(CustCol)),ind_Col)]
    }else{ind_Col_diff = CustCol}
    if(length(ind_Col_diff) != 0){
      for(i in 1:length(ind_Col_diff)){
        NEWNodeInfoDF$newcolumn = rep(NA, length(NEWNodeInfoDF$id))
        NewDataNodesDF[ind_Col_diff[i]] = as.character(unlist(NewDataNodesDF[ind_Col_diff[i]]))
        NEWNodeInfoDF[match(NewDataNodesDF_int$id, as.character(NEWNodeInfoDF$id)),
                      ind_Col_diff[i]] = NewDataNodesDF_int[ind_Col_diff[i]]
      }
    }
    NodeInfoDF = NEWNodeInfoDF
  }
  if(length(NewDataEdges) != 0){
    NewDataEdgesDF = NewDataEdges$edges
    CustCol = NewDataEdges$CustCol
    NEWEdgesInfoDF = EdgesInfo
    ind_Col = which(CustCol %in% colnames(NEWEdgesInfoDF))
    NewDataEdgesDF$label2 = as.character(unlist(NewDataEdgesDF$label2))
    NEWEdgesInfoDF$label2 = as.character(unlist(NEWEdgesInfoDF$label2))
    int_id = intersect((NewDataEdgesDF$label2), (NEWEdgesInfoDF$label2))
    NewDataEdgesDF_int =  NewDataEdgesDF[(which(as.character(NewDataEdgesDF$label2) %in% int_id)),]
    if(length(ind_Col)>0){
      for(i in 1:length(ind_Col)){
        NEWEdgesInfoDF[CustCol[ind_Col[i]]] = as.character(unlist(NEWEdgesInfoDF[CustCol[ind_Col[i]]]))
        NewDataEdgesDF[CustCol[ind_Col[i]]] = as.character(unlist(NewDataEdgesDF[CustCol[ind_Col[i]]]))
        NEWEdgesInfoDF[match(NewDataEdgesDF_int$label2, as.character(NEWEdgesInfoDF$label2)),
                       CustCol[ind_Col[i]]] = NewDataEdgesDF_int[CustCol[ind_Col[i]]]
      }
      ind_Col_diff = CustCol[setdiff(seq(1, length(CustCol)),ind_Col)]
    }else{ind_Col_diff = CustCol}
    for(i in 1:length(ind_Col_diff)){
      NEWEdgesInfoDF$newcolumn = rep(NA, length(NEWEdgesInfoDF$label2))
      colnames(NEWEdgesInfoDF)[which(colnames(NEWEdgesInfoDF) == 'newcolumn')] = ind_Col_diff[i]
      NewDataEdgesDF[ind_Col_diff[i]] = as.character(unlist(NewDataEdgesDF[ind_Col_diff[i]]))
      NEWEdgesInfoDF[match(NewDataEdgesDF_int$label2, as.character(NEWEdgesInfoDF$label2)),
                     ind_Col_diff[i]] = NewDataEdgesDF_int[ind_Col_diff[i]]
    }
    EdgesInfo = NEWEdgesInfoDF
  }
  
  Net = list(nodes = NodeInfoDF, edges = EdgesInfo, RulesSetPerNode = NodeRulesSet)
  return(Net)
}