#
# SPDB project
# Spatial data clustering study
#
# author: Mateusz Zawislak
# December 2014
#

setwd("D:/Github/spatial-clustering/src")

source('clustering.quality.R')

# Wine data
wine.data.description <- list(
  "class.column" = c("V1"),
  "string.columns" = list(list(col = "V1")),
  "numerical.columns" = list(list(col = "V6"), list(col = "V7"), list(col = "V8"), list(col = "V9"),
                             list(col = "V10"), list(col = "V11"), list(col = "V12"), list(col = "V13"), list(col = "V14")),
  "points" = list(list("x" = list(col = "V2"), "y" = list(col = "V5"))),
  "geographic.coordinates" = list(list("long" = list(col = "V3"), "lat" = list(col = "V4")))
)

wine.clustering.description <- list(
  "data.description" = wine.data.description,
  
  "params" = list(
    "clusters.number" = 6,
    "weight.vector" = list("non.spatial" = 1, "spatial" = 1)
  )
)

# USA cities data
usa.cities.description <- list(
  "class.column" = "V3",
  "numerical.columns" = list(list(col = "V5")),
  "geographic.coordinates" = list(list("long" = list(col = "V7"), "lat" = list( col = "V6")))
)

usa.cities.clustering.description <- list (
  "data.description" = usa.cities.description,
  
  "params" = list(
    "clusters.number" = 31,
    "weight.vector" = list("non.spatial" = 1, "spatial" = 1)
  )
)

# Mississippi Police Crime Reports data
police.data.description <- list(
  "numerical.columns" = list(list(col = "TAX"), list(col = "TRANSFER"), list(col = "INC"), list(col = "CRIME"), list(col = "UNEMP"), list(col = "OWN"), list(col = "COLLEGE"), list(col = "WHITE"), list(col = "COMMUTE")),
  "polygons" = list(list(col = "V22"))
)

police.clustering.description <- list (
  "data.description" = police.data.description,
  
  "params" = list(
    "clusters.number" = 6,
    "weight.vector" = list("non.spatial" = 1, "spatial" = 14)
   )
)

main <- function() {
  uciData <- read.csv(file="../data/police.csv", head=TRUE, sep=",", skip=0, dec=".")
  
  system.time(testClustering(uciData, police.clustering.description))
}