#
# SPDB project
# Spatial data clustering study
#
# author: Mateusz Zawislak
# December 2014
#

setwd("D:/Github/spatial-clustering/src")

source('clustering.quality.R')

wine.data.description <- list(
  "classIndex" = 1
)

main <- function() {
  uciData <- read.csv(file="../data/wine.data", head=FALSE, sep=",", skip=0)
  
  system.time(test.clustering(uciData, wine.data.description))
}