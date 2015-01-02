#' A package for studying clustering algorithms for spatial data
#' 
#' 
#' @description
#' SPDB project studying clustering algorithms for spatial data.
#' 
#' \bold{More:}
#' Comparison of the quality and speed of spatial data clustering using a clustering algorithm PAM in two versions:
#' 
#' - Spatial data are treated the same as other data - is the one calculated the distance (similarity) on the basis of all available attributes.
#' 
#' - The distance (similarity) between objects is calculated based on two components: spatial distance and distance (similarity) calculated on the basis of descriptive attributes.
#' 
#' \bold{Main function:} \code{spatialCluster}
#'
#' 
#' @details
#' \bold{Package:} rclusterstudy
#' 
#' \bold{Version:} 0.9
#' 
#' \bold{Date:} 2015-01-02
#' 
#' \bold{Depends:} R (>= 3.1.1), sp, stringdist, cluster, rgeos, geosphere 
#' 
#' 
#' @author
#' Mateusz Zawislak \email{mateusz.andrzej.zawislak@@gmail.com}
#' @name rclusterstudy
#' @examples
#' # Mississippi Police Crime Reports data description
#' 
#' police.data.description <- list(
#'   "numerical.columns" = list(list(col = "TAX"), list(col = "TRANSFER"), list(col = "INC"),
#'                         list(col = "CRIME"), list(col = "UNEMP"), list(col = "OWN"),
#'                         list(col = "COLLEGE"), list(col = "WHITE"), list(col = "COMMUTE")),
#'   "polygons" = list(list(col = "V22"))
#' )
#' 
#' police.clustering.description <- list (
#'   "data.description" = police.data.description,
#'   
#'   "params" = list(
#'     "clusters.number" = 6,
#'     "weight.vector" = list("non.spatial" = 1, "spatial" = 14),
#'     "spatial" = TRUE
#'   )
#' )
#' 
#' # cluster data
#' uciData <- read.csv(file="../data/police.csv", head=TRUE, sep=",", skip=0, dec=".")
#' spatialCluster(uciData, clustering.description.nonspatial)
#' @docType package

library(sp)
library(stringdist)
library(cluster)
library(rgeos)

source('C:/Users/Mateusz/Documents/rclusterstudy/R/clustering.core.description.R')
# source('R/clustering.core.description.R')

#
# Utils
#

#' Function that returns column index for the given column name in data.frame
#' 
#' @param data data
#' @param column.name column name
#'
#' @return column index for the given column name
#' 
#' @export
getColumnIndex <- function(data, column.name) {
  match(column.name, colnames(data))
}

#' Function that gives column name for the give column index in data.frame
#' 
#' @param data Data.
#' @param index Column index.
#'
#' @return Column name for the give column index.
#' 
#' @export
getColumnName <- function(data, index) {
  colnames(data)[index]
}

# distance metrics

#' Function that returns Bray Curtis distance between given attribute's values.
#' 
#' @param val1 Attribute value.
#' @param val2 Another attribute value.
#' 
#' @export
#'
#' @return Bray Curtis distance between given attribute's values.
#' 
#' @keywords distance
#' 
#' @examples
#' distanceBrayCurtis(3, 5)
distanceBrayCurtis <- function(val1, val2) {
  abs(val1 - val2) / (val1 + val2)
}

#' Function that returns distance between two given polygons.
#' 
#' @details Polygons should be able to read by function \code{readWKT} from \code{rgeos} package.
#' 
#' @param polygon1 Polygon description accepted by rgeos package.
#' @param polygon2 Second polygon.
#'
#' @return Length of the shortest path between the given polygons.
#' 
#' @keywords distance spatial
#' 
#' @export
#' 
#' @examples
#' distancePolygons("POLYGON((1 1,5 1,5 5,1 5,1 1))","POLYGON((12 12,12 13,13 13,13 12,12 12))")
distancePolygons <- function(polygon1, polygon2) {
  polygon1 <- readWKT(polygon1)
  polygon2 <- readWKT(polygon2)
  
  gDistance(polygon1, polygon2)
}

#' Function that returns distance between two given numbers. It can be scaled to range [min.val, max.val]
#' 
#' @param val1 Attribute value.
#' @param val2 Another attribute value.
#' @param min.val (Optional) Minimum value of the feature in the collection.
#' @param max.val (Optional) Maximum value of the feature in the collection.
#'
#' @return Scaled distance between given attribute's values, which type is numerical.
#' 
#' @export
#' 
#' @keywords distance
#' 
#' @examples
#' distanceReal(3, 4)
#' distanceReal(5, 12.4, 3, 18.9)
distanceReal <- function(val1, val2, min.val = 0, max.val = 1) {
  difference <- abs(val1 - val2)
  if(min.val - max.val != 0) {
    difference <- difference / abs(min.val - max.val)
  }
  
  difference
}

#' Function that returns distance between two given geographic coordinates
#' 
#' @param long1: First point's longitude.
#' @param lat1: First point's latitude.
#' @param long2: Second point's longitude.
#' @param lat2: Second point's latitude.
#'
#' @return Haversine distance between two given geographic coordinates.
#' 
#' @export
#' 
#' @keywords distance spatial
#' 
#' @examples
#' distanceHaversine(1, 2, 1.1, 2.2)
distanceHaversine <- function(long1, lat1, long2, lat2) {  
  distHaversine(c(long1, lat1), c(long2, lat2))
}

#' Function that calculates Euclidian distance between two given points.
#' 
#' @param x1 First point's x value.
#' @param y1 First point's y value.
#' @param x2 Second point's x value.
#' @param y2 Second point's y value.
#'
#' @return Euclidian distance between two given points.
#' 
#' @export
#' 
#' @keywords distance spatial
#' 
#' @examples
#' distanceEuclidean(1.2, 3, 5, 6)
distanceEuclidean <- function(x1, y1, x2, y2) {
  dist(rbind(c(x1,y1), c(x2,y2)), method = "euclidean")
}

#' Function that calculates Levensthein distance between two given strings.
#' 
#' @param x First string.
#' @param y Second string.
#'
#' @return Levensthein distance between two given strings.
#' 
#' @export
#' 
#' @keywords distance
#' 
#' @examples
#' distanceLevensthein("mateusz", "mateuss") # result: 1
distanceLevensthein <- function(x, y) { 
  stringdist(x, y, method = c("lv"))
}

#' Function that calculates scaled distance between two strings.
#' 
#' @param x First string.
#' @param y Second string.
#'
#' @return Scaled distance between two given strings.
#' 
#' @export
#' 
#' @keywords distance
#' 
#' @examples
#' stringDistance("mateusz", "mateuss") # result: 0.1428571
stringDistance <- function(x, y) {
  distanceLevensthein(x, y)/max(nchar(x), nchar(y))
}

#' Function that calculates distance between two given objects.
#' 
#' It sums distances between all atrributes values.
#'
#' @param d1 First object.
#' @param d2 Second object.
#' @param clustering.description Data description and clustering params.
#'
#' @return Distance between two given objects as double.
#' 
#' @export
#' 
#' @keywords distance
objectsDistance <- function(d1, d2, clustering.description) {  
  data.description <- clustering.description$data.description
  is.spatial <- clustering.description$params$spatial
  spatial.weight <- clustering.description$params$weight.vector$spatial
  nonspatial.weight <- clustering.description$params$weight.vector$non.spatial
  
  distance = 0
  
  # calculate distance between all point type features
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
  
  # calculate distance between all polygon type features
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
  
  # calculate distance between all geographic coordinates type features
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
  
  # calculate distance between all string type features
  if(length(data.description$string.columns) > 0) {
    distance = distance + nonspatial.weight * do.call(sum, lapply(data.description$string.columns,function(column) {
      stringDistance(d1[column$col], d2[column$col])
    }))
  }
  
  # calculate distance between all numeric type features
  if(length(data.description$numerical.columns) > 0) {
    numerical.columns <- do.call(c, lapply(data.description$numerical.columns, function(column) {column$col}))
    numerical.columns.val <- do.call(cbind, lapply(data.description$numerical.columns, function(column) {c(column$min, column$max)}))
    
    distance = distance + nonspatial.weight * sum(mapply(function(x, y, min.val, max.val) {distanceReal(x, y, min.val = min.val, max.val = max.val)}, d1[,numerical.columns], d2[,numerical.columns], numerical.columns.val[1,], numerical.columns.val[2,]))
  }
  
  distance
}

#' Function that calculates distances between given object and the list of objects.
#'
#' @param objects List of objects.
#' @param object Core object.
#' @param clustering.description Data description and clustering params.
#'
#' @return The vector of distances from object to each one from objects. Vector length is equal to objects number.
calculateDistancesFromObject <- function(objects, object, clustering.description) {
  print('speeding...')
  distances <- lapply(1:nrow(objects),function(i) { objectsDistance(objects[i,], object, clustering.description) })
}

#' Function that calculates objects distance matrix.
#' 
#' It calculates only bottom-left part of matrix, because it is symetric.
#'
#' @param objects List of objects.
#' @param clustering.description Data description and clustering params.
#' 
#' @export
#' 
#' @keywords distance
#'
#' @return Distance matrix between given objects.
calculateDistanceMatrix <- function(objects, clustering.description) {
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
  # hint: cbind to pionowe złącznie wektorów
  dm <- do.call(cbind, lapply(1:nrow(objects), distancesFromObject, objects, clustering.description))
  return(as.dist(dm))
}

#' Function that clusters data with PAM algorithm.
#'
#' @param data data to cluster
#' @param clustering.description data, description and clustering params
#'
#' @return cluster model
#' 
#' @keywords cluster
#' 
#' @export
#' 
#' @examples
#' # Mississippi Police Crime Reports data description
#' 
#' police.data.description <- list(
#'   "numerical.columns" = list(list(col = "TAX"), list(col = "TRANSFER"), list(col = "INC"),
#'                         list(col = "CRIME"), list(col = "UNEMP"), list(col = "OWN"),
#'                         list(col = "COLLEGE"), list(col = "WHITE"), list(col = "COMMUTE")),
#'   "polygons" = list(list(col = "V22"))
#' )
#' 
#' police.clustering.description <- list (
#'   "data.description" = police.data.description,
#'   
#'   "params" = list(
#'     "clusters.number" = 6,
#'     "weight.vector" = list("non.spatial" = 1, "spatial" = 14),
#'     "spatial" = TRUE
#'   )
#' )
#' 
#' # cluster data
#' uciData <- read.csv(file="../data/police.csv", head=TRUE, sep=",", skip=0, dec=".")
#' spatialCluster(uciData, clustering.description.nonspatial)
spatialCluster <- function(data, clustering.description) {  
  # distance matrix
  objectsDistances.matrix <- calculateDistanceMatrix(data, clustering.description)
  
  # cluster data
  print('Started PAM clustering')
  cluster.model <- pam(objectsDistances.matrix, k = clustering.description$params$clusters.number)
  print('Clustering completed')
  cluster.model
}
