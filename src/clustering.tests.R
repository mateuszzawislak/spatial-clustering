#
# SPDB project
# Spatial data clustering study
#
# author: Mateusz Zawislak
# December 2014
#

setwd("D:/Github/spatial-clustering/src")

source('clustering.quality.R')

wine.data.description.spatial <- list(
  "ignore.columns" = c("V9"),
  "string.columns" = c("V1"),
  "points" = list(list("x" = "V2", "y" = "V5")),
  "geographic.coordinates" = list(list("long" = "V3", "lat" = "V4"))
)

wine.data.description <- list(
  "class.column" = "V1",
  "ignore.columns" = c("V9")
)

main <- function() {
  uciData <- read.csv(file="../data/wine.data", head=FALSE, sep=",", skip=0, dec=".")
  
  system.time(test.clustering(uciData, wine.data.description, wine.data.description.spatial))
}