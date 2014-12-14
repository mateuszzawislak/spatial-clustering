#
# SPDB project
# Spatial data clustering study
#
# author: Mateusz Zawislak
# December 2014
#

library(rgeos)

# example clustering description
example.clustering.description <- list (
  "data.description" = list(
    "class.column" = c("V1"),
    "string.columns" = list(list(col = "V1")),
    "numerical.columns" = list(list(col = "V6"), list(col = "V7"), list(col = "V8"), list(col = "V9")),
    "points" = list(list("x" = list(col = "V2"), "y" = list(col = "V5"))),
    "geographic.coordinates" = list(list("long" = list(col = "V3"), "lat" = list(col = "V4")))
  ),
  
  "params" = list(
    "clusters.number" = 6
  )
)

getAttributesColumns <- function(data, clustering.description) {
  # Args:
  #   data: Data.
  #   clustering.description: Data description and clustering params.
  #
  # Returns:
  #   Attributes array that should be calculated by the clustering algorithm.
  
  data.description <- clustering.description$data.description
  
  column.names <- c()
  if(length(data.description$points) > 0) {
    for(i in 1:length(data.description$points)) {
      point <- data.description$points[i][[1]]
      column.names <- append(column.names, point$x$col)
      column.names <- append(column.names, point$y$col)
    }
  }
  
  if(length(data.description$geographic.coordinates) > 0) {
    for(i in 1:length(data.description$geographic.coordinates)) {
      point <- data.description$geographic.coordinates[i][[1]]
      column.names <- append(column.names, point$lat$col)
      column.names <- append(column.names, point$long$col)
    }
  }
  
  if(length(data.description$string.columns) > 0) {
    for(i in 1:length(data.description$string.columns)) {
      string.col <- data.description$string.columns[i][[1]]
      column.names <- append(column.names, string.col$col)
    }
  }
  
  if(length(data.description$numerical.columns) > 0) {
    for(i in 1:length(data.description$numerical.columns)) {
      num.col <- data.description$numerical.columns[i][[1]]
      column.names <- append(column.names, num.col$col)
    }
  }
  
  if(length(data.description$polygons) > 0) {
    for(i in 1:length(data.description$polygons)) {
      polygon.col <- data.description$polygons[i][[1]]
      column.names <- append(column.names, polygon.col$col)
    }
  }
  
  column.names
}

appendFeaturesRanges <- function(objects, clustering.description) {
  # Function that appends clustering description with all features' values ranges.
  #
  # Args:
  #   data: Data.
  #   clustering.description: Data description and clustering params.
  #
  # Returns:
  #  New clustering description with all features' values ranges.
  
  data.description <- clustering.description$data.description
  
  # points
  if(length(data.description$points) > 0) {
    lapply(1:length(data.description$points), function(index) {
        point.feature <- data.description$points[index][[1]]
        x.col = point.feature$x$col
        y.col = point.feature$y$col
        
        x.min <- min(objects[,x.col])
        x.max <- max(objects[,x.col])
        y.min <- min(objects[,y.col])
        y.max <- max(objects[,y.col])
        
        clustering.description$data.description$points[index][[1]]$x["min"] <<- x.min
        clustering.description$data.description$points[index][[1]]$x["max"] <<- x.max
        clustering.description$data.description$points[index][[1]]$y["min"] <<- y.min
        clustering.description$data.description$points[index][[1]]$y["max"] <<- y.max
      }
    )
  }
  
  # polygons
  if(length(data.description$polygons) > 0) {
    lapply(1:length(data.description$polygons), function(index) {
      polygon.feature <- data.description$polygons[index][[1]]
      
      get.all.mbb.values <- function(a1, a2) {
        do.call(c, lapply(1:nrow(objects), function(index) {
          object <- objects[index,]
          
          polygon <- readWKT(object[polygon.feature$col][1,])
          mbb <- bbox(polygon)
          
          mbb[a1, a2]
        }))
      }
      
      mbb.x.min <- get.all.mbb.values("x", "min")
      mbb.x.max <- get.all.mbb.values("x", "max")
      mbb.y.min <- get.all.mbb.values("y", "min")
      mbb.y.max <- get.all.mbb.values("y", "max")
      
      clustering.description$data.description$polygons[index][[1]]["xminmin"] <<- min(mbb.x.min)
      clustering.description$data.description$polygons[index][[1]]["xminmax"] <<- max(mbb.x.min)
      clustering.description$data.description$polygons[index][[1]]["xmaxmin"] <<- min(mbb.x.max)
      clustering.description$data.description$polygons[index][[1]]["xmaxmax"] <<- max(mbb.x.max)
      clustering.description$data.description$polygons[index][[1]]["yminmin"] <<- min(mbb.y.min)
      clustering.description$data.description$polygons[index][[1]]["yminmax"] <<- max(mbb.y.min)
      clustering.description$data.description$polygons[index][[1]]["ymaxmin"] <<- min(mbb.y.max)
      clustering.description$data.description$polygons[index][[1]]["ymaxmax"] <<- max(mbb.y.max)
    }
    )
  }
  
  # geographic coordinates
  if(length(data.description$geographic.coordinates) > 0) {
    lapply(1:length(data.description$geographic.coordinates), function(index) {
      geo.point.feature <- data.description$geographic.coordinates[index][[1]]
      long.col = geo.point.feature$long$col
      lat.col = geo.point.feature$lat$col
      
      long.min <- min(objects[,long.col])
      long.max <- max(objects[,long.col])
      lat.min <- min(objects[,lat.col])
      lat.max <- max(objects[,lat.col])
      
      clustering.description$data.description$geographic.coordinates[index][[1]]$long["min"] <<- long.min
      clustering.description$data.description$geographic.coordinates[index][[1]]$long["max"] <<- long.max
      clustering.description$data.description$geographic.coordinates[index][[1]]$lat["min"] <<- lat.min
      clustering.description$data.description$geographic.coordinates[index][[1]]$lat["max"] <<- lat.max
    }
    )
  }

  # numerical columns
  if(length(data.description$numerical.columns) > 0) {
    lapply(1:length(data.description$numerical.columns), function(index) {
      numerical.feature <- data.description$numerical.columns[index][[1]]
      
      min.val <- min(objects[,numerical.feature$col])
      max.val <- max(objects[,numerical.feature$col])
      
      clustering.description$data.description$numerical.columns[index][[1]]["min"] <<- min.val
      clustering.description$data.description$numerical.columns[index][[1]]["max"] <<- max.val
    }
    )
  }

  clustering.description
}