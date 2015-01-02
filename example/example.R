require(rclusterstudy)

# example using Package 'rclusterstudy' ver 0.9

# !!! need to secipfy this
setwd("D:/Github/spatial-clustering/example/")

uciData <- read.csv(file="example_data.csv", head=TRUE, sep=",", skip=0, dec=".")

# USA cities data
usa.cities.description <- list(
  "class.column" = "V3",
  "numerical.columns" = list(list(col = "V5")),
  "geographic.coordinates" = list(list("long" = list(col = "V7"), "lat" = list( col = "V6")))
)

usa.cities.clustering.description <- list (
  "data.description" = usa.cities.description,
  
  "params" = list(
    "clusters.number" = 10,
    "weight.vector" = list("non.spatial" = 1, "spatial" = 1),
    "spatial" = TRUE
  )
)

# cluster data and print cluster model
print(spatialCluster(uciData, usa.cities.clustering.description))