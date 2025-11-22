visunetcyto <- function(vis, title = "Autism_VisuNet_optA") {
  
  rename_edges <- function(df){
    if (is.null(df) || nrow(df) == 0) return(df)
    names(df)[names(df) == "from"] <- "source"
    names(df)[names(df) == "to"]   <- "target"
    df
  }
  
  createStyle <- function(style_name, net){
    nodes <- net$nodes
    edges <- net$edges
    
    defaults <- list(NODE_SHAPE = "circle", EDGE_LABEL = "")
    nodeLabel <- mapVisualProperty("node label", "label", "p")
    
    # Storlek bara baserad på meanSupp (value behövs inte)
    nodeSize <- mapVisualProperty("node size", "meanSupp", "c",
                                  range(nodes$meanSupp, finite = TRUE),
                                  c("30", "75"))
    
    nodeFill  <- if ("color.background" %in% names(nodes))
      mapVisualProperty("node fill color", "color.background", "p") else NULL
    nodeBW    <- if ("borderWidth" %in% names(nodes))
      mapVisualProperty("node border width", "borderWidth", "p") else NULL
    nodeBC    <- if ("color.border" %in% names(nodes))
      mapVisualProperty("node border paint", "color.border", "p") else NULL
    edgeCol   <- if (!is.null(edges) && "color" %in% names(edges))
      mapVisualProperty("edge stroke unselected paint", "color", "p") else NULL
    edgeW     <- if (!is.null(edges) && "width" %in% names(edges))
      mapVisualProperty("edge width", "width", "p") else NULL
    
    if (style_name %in% getVisualStyleNames()) deleteVisualStyle(style_name)
    
    createVisualStyle(
      style_name,
      defaults,
      Filter(Negate(is.null),
             list(nodeLabel, nodeFill, nodeBW, nodeBC, nodeSize, edgeCol, edgeW))
    )
  }
  
  # Id-listor för noder som tillhör autism/control-näten
  idsA <- if (!is.null(vis$autism$nodes))  vis$autism$nodes$id  else character(0)
  idsC <- if (!is.null(vis$control$nodes)) vis$control$nodes$id else character(0)
  
  # Endast kolumner som används av VisuNet + färg/stil + beslut
  # (vi tar inte med value eller title här för att hålla tabellen renare)
  node_cols_keep <- c(
    "id",                     # behövs för createNetworkFromDataFrames()
    "label",                  # nodlabel som visas i Cytoscape
    "decision",               # beslut (autism/control/mixed) per nod
    "meanAcc",                # medel-accuracy (för tolkning/ev. filter)
    "meanSupp",               # medel-support (styr nodstorlek i createStyle)
    "meanDecisionCoverage",   # medel decision coverage
    "color.background",       # nodfärg från VisuNet
    "borderWidth",            # kanttjocklek runt noden
    "color.border"            # kantfärg runt noden
  )
  
  # Endast kant-kolumner vi faktiskt använder i stil och tolkning
  edge_cols_keep <- c(
    "source",   # från 'from' → källa
    "target",   # från 'to'   → mål
    "color",    # kantfärg
    "width",    # kantbredd
    "label2"    # extra etikett för kanter (t.ex. vikt/info)
  )
  
  for (net_name in names(vis)) {
    net <- vis[[net_name]]
    if (is.null(net) || is.null(net$nodes)) next
    
    nodes <- net$nodes
    edges <- rename_edges(net$edges)
    
    # Lägg till decision om saknas
    # - om nätet heter "autism"/"control" sätts alla noder till det beslutet
    # - annars kollar vi om nod-id finns i autism- respektive control-nät
    #   och märker dem som "autism", "control" eller "mixed"
    if (!("decision" %in% names(nodes))) {
      if (net_name %in% c("autism", "control")) {
        nodes$decision <- net_name
      } else {
        inA <- nodes$id %in% idsA      # noden finns i autism-nätet
        inC <- nodes$id %in% idsC      # noden finns i control-nätet
        nodes$decision <- ifelse(inA & !inC, "autism",
                                 ifelse(inC & !inA, "control", "mixed"))
      }
    }
    
    # Plocka bara ut de kolumner vi vill visa/skicka till Cytoscape
    nodes_cyto <- nodes[, intersect(node_cols_keep, names(nodes)), drop = FALSE]
    edges_cyto <- if (!is.null(edges) && nrow(edges) > 0) {
      edges[, intersect(edge_cols_keep, names(edges)), drop = FALSE]
    } else {
      NULL
    }
    
    # Ta bort ev. gammalt nätverk med samma namn
    if (net_name %in% getNetworkList()) deleteNetwork(net_name)
    
    # Skapa nätverket i Cytoscape med de slimmade tabellerna
    if (!is.null(edges_cyto) && nrow(edges_cyto) > 0) {
      net_suid <- createNetworkFromDataFrames(nodes_cyto, edges_cyto,
                                              title = net_name,
                                              collection = title)
    } else {
      net_suid <- createNetworkFromDataFrames(nodes = nodes_cyto,
                                              title = net_name,
                                              collection = title)
    }
    
    # Rensa bort onödiga kolumner från tabellerna inne i Cytoscape
    node_cols <- getTableColumnNames(table = "node", network = net_suid)
    edge_cols <- getTableColumnNames(table = "edge", network = net_suid)
    
    # Ta bort id, value, title från nod-tabellen om de finns
    # (Cytoscape har egna id/name-kolumner så dessa blir bara dubbletter)
    for (col in c("id", "value", "title")) {
      if (col %in% node_cols) {
        deleteTableColumn(col, table = "node", network = net_suid)
      }
    }
    
    # Ta bort title + data.key.column från edge-tabellen om de finns
    # (rensar hjälpkolumner som inte behövs i GUI:t)
    for (col in c("title", "data.key.column")) {
      if (col %in% edge_cols) {
        deleteTableColumn(col, table = "edge", network = net_suid)
      }
    }
    
    style.name <- paste0(title, "_", net_name, "_style")
    createStyle(style.name, list(nodes = nodes_cyto, edges = edges_cyto))
    setVisualStyle(style.name)
  }
}
