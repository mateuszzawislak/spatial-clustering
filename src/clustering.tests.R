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
  "class.column" = "V1",
  "ignore.columns" = c("V9"),
  "points" = list(list("x" = "V2", "y" = "V5")),
  "geographic.coordinates" = list(list("long" = "V3", "lat" = "V4"))
)

wine.clustering.description <- list(
  "data.description" = wine.data.description,
  
  "params" = list(
    "clusters.number" = 6
  )
)

# USA cities data
usa.cities.description <- list(
  "class.column" = "V3",
  "ignore.columns" = c("V1", "V2", "V4"),
  "geographic.coordinates" = list(list("long" = "V7", "lat" = "V6"))
)

usa.cities.clustering.description <- list (
  "data.description" = usa.cities.description,
  
  "params" = list(
    "clusters.number" = 31
  )
)

# Police Crime Reports data
police.data.description <- list(
  "ignore.columns" = c("AREA","PERIMETER","CNTY_","CNTY_ID","NAME","STATE_NAME","STATE_FIPS","CNTY_FIPS","FIPS","FIPSNO","POLICE","POP"),
  "polygons" = list("V22")
)

police.clustering.description <- list (
  "data.description" = police.data.description,
  
  "params" = list(
    "clusters.number" = 6
   )
)

main <- function() {
  uciData <- read.csv(file="../data/USA_Cities.csv", head=FALSE, sep=";", skip=0, dec=".")
  
  system.time(test.clustering(uciData, usa.cities.clustering.description))
}