library(devtools)
if(!"RCy3" %in% installed.packages()){
    install.packages("BiocManager")
    BiocManager::install("RCy3")
}
library(RCy3)
load_all(path = "~/Documents/GitHub/VisuNet.v.1.1/Scripts (Should be integrated later)/VisuNetXCytoscape.R")
~/Documents/GitHub/VisuNet.v.1.1/Scripts (Should be integrated later)/VisuNetXCytoscape.R

autconJohnson <- rosetta(autcon, roc = TRUE, clroc = "autism")
rules <- autconJohnson$main

title <- "Autism_Visunet"

visunetcyto(rules, title, addGO = TRUE) # integration of everything in copy of visunet file

# --
Autism_Visunet <- visunet(rules)

cytoscapePing() #För eventuella problem med connection till Cytoscape



clearCollection(title)


for (net_name in names(Autism_Visunet)) {

  network <- Autism_Visunet[[net_name]]
  network <- restructureNetworkDF(network)
  net_suid <- createNetworkFromDataFrames(network$nodes,network$edges, title=net_name, collection=title)


  style_name = paste(title, net_name, '_style')
  createStyle(style_name, network)

  setVisualStyle(style_name)

  makeMeanAccuracySlider(network = net_suid)
  makeMeanSupportSlider(network = net_suid)
  makeMeanDecCoverageSlider(network = net_suid)

  # Composite-filter som samlar alla tre i EN ruta
  createCompositeFilter(
    filter.name = "meanAcc + meanSupp + meanDecisionCoverage",
    filter.list = c(
      "meanAcc filter",
      "Mean support filter",
      "Mean decision coverage filter"
    ),
    type   = "ALL",    # ALL = AND, ANY = OR
    hide   = FALSE,
    network = net_suid,
    apply  = FALSE     # bara skapa, inte köra direkt
  )
}
