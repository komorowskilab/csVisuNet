#' VisuArc: An arcdiagram for VisuNet diagram
#' @export
#' @import arcdiagram
#' @param net The saved VisuNet object
#' @param decision The decision for which you want to display the diagram.
#' @param feature The feature that you are interested to explore. By default is the hub node.
#' @return An arc diagram
#' @examples
#' # ros<-rosetta(autcon)
#' # vis<-visunet(ros$main)
#' # visuArc(net= vis,decision= 'autism',feature='ZSCAN18')


visuArc <- function(net, decision, feature=NULL){
  #load libraries if you only copy the function
  #library(VisuNet)
  # if (!require(arcdiagram)) install.packages('arcdiagram')
  # library(arcdiagram)

  #make df:  a data frame containing network connection in columns
 # df<- eval(parse(text=paste("net$", decision,'$nodes' ,sep = "")))
  df<-net[[decision]][['nodes']]
  cols_select<- c('label','DiscState','NodeConnection')
  df<- subset(df,select = cols_select)
  rownames(df)  <-make.names(df[,'label'], unique = TRUE)
  df <- df[,-1]

  #scale Node connection fro 0 to 1
  scale_values <- function(x){(x-min(x))/(max(x)-min(x))}
  df$NodeConnection <- scale_values(df$NodeConnection)


  #check for feature
  if(is.null(feature)){
    feature <- rownames(df[which.max(df[,'NodeConnection']),])
  }

  discLev<-df[feature,'DiscState']
  feature2 <- gsub("\\s*\\([^\\)]+\\)","",as.character(feature))
  edges <- net[[decision]]$edges[,1:4]
  edges_from <- as.character(edges$from)
  edges_to <- as.character(edges$to) ## gsub("\\=.*","",as.character(net$short$edges$to))
  # --- Mattias: Changed the row below to use the new ids
  indx <- unique(c(which(edges_from == paste0(decision,"_",feature2,"=",discLev)),which(edges_to == paste0(decision,"_",feature2,"=",discLev))))
  # old version:
  # indx <- unique(c(which(edges_from == paste0(feature2,"=",discLev)),which(edges_to == paste0(feature2,"=",discLev))))
  edges2 <- edges[indx,]
  M <- as.matrix(edges2[,1:2])

  #colors
  valsCols <- round(edges2$connNorm, digits = 1)*10
  # --- Mattias: Changed this so normConn = 1 is sorted correctly
  edgesCon <- as.integer(factor(valsCols, levels = sort(unique(valsCols))))
  # old version:
  # edgesCon <- as.numeric(as.factor(as.character(valsCols)))
  colors <- colorRampPalette(c("gainsboro","lavender","darkorchid3"))(length(unique(valsCols)))[edgesCon]
  labelsNodes <- unique(c(t(M)))
  colNodes <- c("#56B4E9","#999999","#E69F00")[as.numeric(sub(".*=","",labelsNodes))] #EDIT THIS WITH YOUR COLOR CODING

  #nodes values
  nodesDlev<- df[,'DiscState']
  nodesNams <- gsub("\\s*\\([^\\)]+\\)","",as.character(rownames(df)))
  nodes2 <- paste0(nodesNams,"=",nodesDlev)
  nodeSize <- round(df[match(labelsNodes, nodes2),2],digits = 1)*10

  ordV <- c(1,order(valsCols, decreasing = T)+1)

  colsLabs <- rep("black",length(labelsNodes))
  colsLabs[which(labelsNodes == paste0(feature2,"=",discLev))] <- "red"
  # --- Mattias and GP: Changed the row below to use the new ids and turn them back into nodeNames
  arcplot(M, lwd.arcs=edgesCon, col.arcs = colors, col.nodes = colNodes, labels=sub("^[^_]+_","",sub("=.*","",labelsNodes)),
          ordering=ordV, col.labels=colsLabs, cex.labels=0.7, font=1, lwd.nodes = nodeSize, horizontal = FALSE,main = decision,adj=1)
  # old version:
  # arcplot(M, lwd.arcs=edgesCon, col.arcs = colors, col.nodes = colNodes, labels=sub("=.*","",labelsNodes),
  #         ordering=ordV, col.labels=colsLabs, cex.labels=0.7, font=1, lwd.nodes = nodeSize, horizontal = FALSE,main = decision,adj=1)
}

#Manual
#decision : decision
#net : Visunet output
#feature : central or interested hub feature. P.S. It should have connections!!!!!!! If null it chooses the central hub by default.

##Program
#ros<-rosetta(autcon)
#vis<-visunet(ros$main)
#visuArc(vis,'autism',feature='ZSCAN18')
#visuArc(vis,'Asian')
