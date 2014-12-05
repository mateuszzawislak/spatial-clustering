#
# SPDB project
# Spatial data clustering study
#
# author: Mateusz Zawislak
# December 2014
#

setwd("D:/Github/spatial-clustering/src")

source('clustering.core.R')

wine.data.description <- list(
  "classIndex" = 1
)

remove.class.column <- function(data, classIndex) {
  data[,-classIndex]
}

main <- function() {
  uciData <- read.csv(file="../data/wine.data", head=FALSE, sep=",", skip=0)
  
  cluster.model <- spatial.cluster(uciData, wine.data.description)
  print(cluster.model)
}