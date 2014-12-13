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
  "class.column" = "V1",
  "ignore.columns" = c("V9"),
  "points" = list(list("x" = "V2", "y" = "V5")),
  "geographic.coordinates" = list(list("long" = "V3", "lat" = "V4"))
)

wine.data.description <- list(
  "class.column" = "V1",
  "ignore.columns" = c("V9")
)

usa.cities.description <- list(
  "class.column" = "V3",
  "ignore.columns" = c("V1", "V2", "V4")
)

usa.cities.description.spatial <- list(
  "class.column" = "V3",
  "ignore.columns" = c("V1", "V2", "V4"),
  "geographic.coordinates" = list(list("long" = "V7", "lat" = "V6"))
)

police.description <- list(
  "ignore.columns" = c("AREA","PERIMETER","CNTY_","CNTY_ID","NAME","STATE_NAME","STATE_FIPS","CNTY_FIPS","FIPS","FIPSNO","POLICE","POP"),
  "polygons" = list("V22")
)

main <- function() {
  uciData <- read.csv(file="../data/police.csv", head=TRUE, sep=",", skip=0, dec=".")
  
  system.time(test.clustering(uciData, police.description, police.description))
}