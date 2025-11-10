addGOannotations <- function(vis_object, ontology = "MF") {
  library(clusterProfiler)
  library(GO.db)
  library(org.Hs.eg.db)

  gene_names <- vis_object$all$nodes$label
  gene_entrez <- bitr(gene_names, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = org.Hs.eg.db)

  if (nrow(gene_entrez) == 0) return(vis_object)

  go_list <- list()
  for (i in 1:nrow(gene_entrez)) {
    entrez_id <- gene_entrez$ENTREZID[i]
    symbol <- gene_entrez$SYMBOL[i]

    go_data <- tryCatch(
      AnnotationDbi::select(org.Hs.eg.db, keys = entrez_id, columns = c("GO", "ONTOLOGY"), keytype = "ENTREZID"),
      error = function(e) NULL
    )

    if (!is.null(go_data) && nrow(go_data) > 0) {
      if (ontology != "ALL") go_data <- go_data[go_data$ONTOLOGY == ontology, ]
      if (nrow(go_data) > 0) {
        go_ids <- as.character(unique(go_data[, 'GO']))
        # Get TERM from GO.db using GO IDs
        go_term_data <- tryCatch(
          AnnotationDbi::select(GO.db, keys = go_ids, columns = c("TERM"), keytype = "GOID"),
          error = function(e) NULL
        )
        if (!is.null(go_term_data) && nrow(go_term_data) > 0) {
          go_terms <- as.character(unique(go_term_data$TERM))
          go_list[[symbol]] <- paste(go_terms, collapse = "<br/>")
        }
      }
    }
  }

  if (length(go_list) == 0) return(vis_object)

  go_df <- data.frame(label = names(go_list), GO_function = unlist(go_list), stringsAsFactors = FALSE)

  nodes <- merge(vis_object$all$nodes, go_df, by = "label", all.x = TRUE)
  nodes$title <- ifelse(!is.na(nodes$GO_function),
                       paste0(nodes$title, '<br/><span style="font-weight: normal;">GO:</span> <b>', ifelse(nodes$GO_function == "", "NA", nodes$GO_function), '</b>'),
                       nodes$title)
  vis_object$all$nodes <- nodes

  for (dec in setdiff(names(vis_object), "all")) {
    if (!is.null(vis_object[[dec]]$nodes) && nrow(vis_object[[dec]]$nodes) > 0) {
      dec_nodes <- merge(vis_object[[dec]]$nodes, go_df, by = "label", all.x = TRUE)
      dec_nodes$title <- ifelse(!is.na(dec_nodes$GO_function),
                               paste0(dec_nodes$title, '<br/><span style="font-weight: normal;">GO:</span> <b>', ifelse(dec_nodes$GO_function == "", "NA", dec_nodes$GO_function), '</b>'),
                               dec_nodes$title)
      vis_object[[dec]]$nodes <- dec_nodes
    }
  }

  message("GO annotations added")
  return(vis_object)
}