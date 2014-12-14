#
# SPDB project
# Spatial data clustering study
#
# author: Mateusz Zawislak
# December 2014
#

library(sp)
library(stringdist)
library(cluster)
library(rgeos)

source('clustering.core.description.R')

#
# Utils
#

getColumnIndex <- function(data, column.name) {
  # Args:
  #   data: Data.
  #   column.name: Column name.
  #
  # Returns:
  #   Column index for the give column name
  
  match(column.name, colnames(data))
}

getColumnName <- function(data, index) {
  # Args:
  #   data: Data.
  #   index: Column index.
  #
  # Returns:
  #   Column name for the give column index
  
  colnames(data)[index]
}

# distance metrics

distanceBrayCurtis <- function(val1, val2) {
  # Args:
  #   val1: Attribute value.
  #   val2: Another attribute value.
  #
  # Returns:
  #   Bray Curtis distance between given attribute's values.
  
  abs(val1 - val2) / (val1 + val2)
}

distancePolygons <- function(polygon1, polygon2) {
  # Args:
  #   polygon1: Polygon description accepted by rgeos package.
  #   polygon2: Second polygon.
  #
  # Returns:
  #   Length of the shortest path between the given polygons.
  
  polygon1 <- readWKT(polygon1)
  polygon2 <- readWKT(polygon2)
  
  gDistance(polygon1, polygon2)
}

distanceReal <- function(val1, val2, min.val = 0, max.val = 1) {
  # Args:
  #   val1: Attribute value.
  #   val2: Another attribute value.
  #   min.val: Minimum value of the feature in the collection.
  #   max.val: Maximum value of the feature in the collection.
  #
  # Returns:
  #   Scaled distance between given attribute's values, which type is numerical.
  
  difference <- abs(val1 - val2)
  if(min.val - max.val != 0) {
    difference = difference / abs(min.val - max.val)
  }  

  difference
}

distanceHaversine <- function(long1, lat1, long2, lat2) {
  # Args:
  #   long1: First point's longitude.
  #   lat1: First point's latitude.
  #   long2: Second point's longitude.
  #   lat2: Second point's latitude.
  #
  # Returns:
  #   Haversine distance between two given geographic coordinates.
  
  distHaversine(c(long1, lat1), c(long2, lat2))
}

distanceEuclidean <- function(x1, y1, x2, y2) {
  # Args:
  #   x1: First point's x value.
  #   y1: First point's y value.
  #   x2: Second point's x value.
  #   y2: Second point's y value.
  #
  # Returns:
  #   Euclidian distance between two given points.
  
  dist(rbind(c(x1,y1), c(x2,y2)), method = "euclidean")
}

distanceLevensthein <- function(x, y) {
  # Args:
  #   x: First string.
  #   y: Second string.
  #
  # Returns:
  #   Levensthein distance between two given strings.
  
  stringdist(x, y, method = c("lv"))
}

stringDistance <- function(x, y) {
  # Args:
  #   x: First string.
  #   y: Second string.
  #
  # Returns:
  #   Scaled distance between two given strings.
  
  distanceLevensthein(x, y)/max(length(x), length(y))
}

objectsDistance <- function(d1, d2, clustering.description) {
  # Function that calculates distance between two given objects.
  # It sums distances between all atrributes values.
  #
  # Args:
  #   d1: First object.
  #   d2: Second object.
  #   clustering.description: Data description and clustering params.
  #
  # Returns:
  #   Distance bettween two given objects.
  
  data.description <- clustering.description$data.description
  is.spatial <- clustering.description$params$spatial
  spatial.weight <- clustering.description$params$weight.vector$spatial
  nonspatial.weight <- clustering.description$params$weight.vector$non.spatial
  
  distance = 0
  
  # points
  if(length(data.description$points) > 0) {
    if (is.spatial) {
      distance = distance + spatial.weight * do.call(sum, lapply(data.description$points, function(point) {
              x.col = point$x$col
              y.col = point$y$col

              distanceEuclidean(d1[x.col], d1[y.col], d2[,x.col], d2[,y.col])/distanceEuclidean(point$x$min,point$y$min,point$x$max,point$y$max)
            }
          )
        )
    } else {
      distance = distance + nonspatial.weight * do.call(sum, lapply(data.description$points, function(point) {
              x.col = point$x$col
              y.col = point$y$col

              distance <- distanceReal(d1[,x.col], d2[,x.col], min.val = point$x$min, max.val = point$x$max)
              distance <- distance + distanceReal(d1[,y.col], d2[,y.col], min.val = point$y$min, max.val = point$y$max)
              
              return(distance/2)
            }
          )
        )
    }
  }
  
  # polygons
  if(length(data.description$polygons) > 0) {
    if (is.spatial) {
          distance = distance + spatial.weight * do.call(sum, lapply(data.description$polygons, function(polygon) {   
            distancePolygons(d1[polygon$col][1,], d2[polygon$col][1,])/distanceEuclidean(x1 = polygon$xminmin, y1 = polygon$yminmin, x2 = polygon$xmaxmax, y2 = polygon$ymaxmax)
          }
        )
      )
    } else {
        distance = distance + nonspatial.weight * do.call(sum, lapply(data.description$polygons, function(polygon) {   
            polygon1 <- readWKT(d1[polygon$col][1,])
            polygon2 <- readWKT(d2[polygon$col][1,])
            
            bbox1 <- bbox(polygon1)
            bbox2 <- bbox(polygon2)
            
            distance <- distanceReal(bbox1["x","min"], bbox2["x","min"], min.val = polygon$xminmin, max.val = polygon$xminmax)
            distance <- distance + distanceReal(bbox1["x","max"], bbox2["x","max"], min.val = polygon$xmaxmin, max.val = polygon$xmaxmax)
            distance <- distance + distanceReal(bbox1["y","min"], bbox2["y","min"], min.val = polygon$yminmin, max.val = polygon$yminmax)
            distance <- distance + distanceReal(bbox1["y","max"], bbox2["y","max"], min.val = polygon$ymaxmin, max.val = polygon$ymaxmax)
            
            return(distance/4)
          }
        )
      )
    }
  }
  
  # geographic coordinates
  if(length(data.description$geographic.coordinates) > 0) {
    if (is.spatial) {
      distance = distance + spatial.weight * do.call(sum, lapply(data.description$geographic.coordinates, function(geo.point) {
            long.col = geo.point$long$col
            lat.col = geo.point$lat$col

            distanceHaversine(d1[,long.col], d1[,lat.col], d2[,long.col], d2[,lat.col])/distanceHaversine(geo.point$long$min,geo.point$lat$min,geo.point$long$max,geo.point$lat$max)
          }
        )
      )
    } else {
      distance = distance + nonspatial.weight * do.call(sum, lapply(data.description$geographic.coordinates, function(geo.point) {
            long.col = geo.point$long$col
            lat.col = geo.point$lat$col
            
            distance <- distanceReal(d1[,long.col], d2[,long.col], min.val = geo.point$long$min, max.val = geo.point$long$max)
            distance <- distance + distanceReal(d1[,lat.col], d2[,lat.col], min.val = geo.point$lat$min, max.val = geo.point$lat$max)
            
            return(distance/2)
          }
        )
      )
    }
  }
  
  # string attributes
  if(length(data.description$string.columns) > 0) {
    distance = distance + nonspatial.weight * do.call(sum, lapply(data.description$string.columns,function(column) {
      stringDistance(d1[column$col], d2[column$col])
    }))
  }

  # distance for numerical type attributes
  if(length(data.description$numerical.columns) > 0) {
    distance = distance + nonspatial.weight * do.call(sum, lapply(data.description$numerical.columns,function(column) {
      distanceReal(d1[column$col], d2[column$col], min.val = column$min, max.val = column$max)
    }))
  }
  
  distance
}

calculateDistancesFromObject <- function(objects, object, clustering.description) {
  # Function that calculates distances between given object and the list of objects.
  #
  # Args:
  #   objects: List of objects.
  #   object: Core object.
  #   clustering.description: Data description and clustering params.
  #
  # Returns:
  #   The vector of distances from object to each one from objects.
  #   Vector length is equal to objects number.
  
  print('speeding...')
  distances <- lapply(1:nrow(objects),function(i) { objectsDistance(objects[i,], object, clustering.description) })
}

calculateDistanceMatrix <- function(objects, clustering.description) {
  # Function that calculates objects distance matrix.
  # It calculates only bottom-left part of matrix, because it is symetric.
  #
  # Args:
  #   objects: List of objects.
  #   clustering.description: Data description and clustering params.
  #
  # Returns:
  #   Distance matrix between given objects.
  
  require(geosphere)
  
  distancesFromObject <- function(objectIndex, objects, clustering.description) {
    distances <- rep(0,nrow(objects))

    attributes.indexes <- getAttributesColumns(objects, clustering.description)
    
    # from the given object to the end of the row
    distances[objectIndex:nrow(objects)] <- calculateDistancesFromObject(objects[objectIndex:nrow(objects), attributes.indexes], objects[objectIndex,attributes.indexes], clustering.description)
    return(distances)
  }
  
  clustering.description <- appendFeaturesRanges(objects, clustering.description)
  
  # for every object calculate distances to all objects
  # hint: cbind to pionowe z³¹cznie wektorów
  dm <- do.call(cbind, lapply(1:nrow(objects), distancesFromObject, objects, clustering.description))
  return(as.dist(dm))
}

spatialCluster <- function(data, clustering.description) {
  # Function that clusters data.
  #
  # Args:
  #   data: Data to cluster.
  #   clustering.description: Data description and clustering params.
  #
  # Returns:
  #   Cluster model.
  
  # distance matrix
  objectsDistances.matrix <- calculateDistanceMatrix(data, clustering.description)
  
  # cluster data
  cluster.model <- pam(objectsDistances.matrix, k=clustering.description$params$clusters.number)
  cluster.model
}
