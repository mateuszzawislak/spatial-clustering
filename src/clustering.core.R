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

# example clustering description
# by default columns are treated as real
example.clustering.description <- list (
  "data.description" = list(
    "class.column" = "V3",
    "ignore.columns" = c("V1", "V2", "V4"),
    "geographic.coordinates" = list(list("long" = "V7", "lat" = "V6"))
  ),
  
  "params" = list(
    "clusters.number" = 6
  )
)

#
# Utils
#

get.column.index <- function(data, columnName) {
  # Args:
  #   data: Data
  #   columnName: Column name
  #
  # Returns:
  #   Column index for the give column name
  
  match(columnName, colnames(data))
}

get.column.name <- function(data, index) {
  # Args:
  #   data: Data
  #   index: Column index
  #
  # Returns:
  #   Column name for the give column index
  
  colnames(data)[index]
}

get.attributes.columns <- function(data, clustering.description) {
  # Args:
  #   data: Data
  #   data.description: Data description
  #
  # Returns:
  #   Attributes array that should be calculated by the clustering algorithm
  
  data.description <- clustering.description$data.description
  
  column.names <- colnames(data)
  if(is.null(data.description$class.column) == FALSE) {
    column.names <- column.names[-get.column.index(data, data.description$class.column)]
  }
  
  if(length(data.description$ignore.columns) > 0) {
    lapply(data.description$ignore.columns,function(col.name) {
        column.names <<- column.names[-match(col.name,column.names)]
      }
    )
  }
  
  column.names
}


# distance metrics

distance.bray.curtis <- function(val1, val2) {
  # Args:
  #   val1: Attribute value
  #   val2: Another attribute value
  #
  # Returns:
  #   Bray Curtis distance between given attribute's values.
  
  abs(val1 - val2) / (val1 + val2)
}

distance.polygons <- function(polygon1, polygon2) {
  polygon1 <- readWKT(polygon1)
  polygon2 <- readWKT(polygon2)
  
  gDistance(polygon1, polygon2)
}

distance.real <- function(val1, val2) {
  # Args:
  #   val1: Attribute value
  #   val2: Another attribute value
  #
  # Returns:
  #   Distance between given attribute's values, which type is numerical.
  
  difference <- abs(val1 - val2)
  if(difference != 0) {
    difference = difference / dist(rbind(c(0,0),c(val1,val2)), method = "euclidean")
  }
  
  difference
}

distance.haversine <- function(long1, lat1, long2, lat2) {
  # Args:
  #   long1: First point's longitude
  #   lat1: First point's latitude
  #   long2: Second point's longitude
  #   lat2: Second point's latitude
  #
  # Returns:
  #   Haversine distance between two given geographic coordinates
  
  distHaversine(c(long1,lat1),c(long2,lat2))
}

distance.euclidean <- function(x1, y1, x2, y2) {
  # Args:
  #   x1: First point's x value
  #   y1: First point's y value
  #   x2: Second point's x value
  #   y2: Second point's y value
  #
  # Returns:
  #   Euclidian distance between two given points
  
  dist(rbind(c(x1,y1), c(x2,y2)), method = "euclidean")
}

distance.levensthein <- function(x, y) {
  # Args:
  #   x: First string
  #   y: Second string
  #
  # Returns:
  #   Levensthein distance between two given strings.
  
  stringdist(x, y, method = c("lv"))
}

objects.distance <- function(d1, d2, clustering.description) {
  # Function that calculates distance between two given objects.
  # It sums distances between all atrributes values.
  #
  # Args:
  #   d1: First object
  #   d2: Second object
  #   data.description: Data description
  #
  # Returns:
  #   Distance bettween two given objects.
  
  data.description <- clustering.description$data.description
  is.spatial <- clustering.description$params$spatial
  
  distance = 0
  
  left.attributes <- names(d1)
  
  # points
  if(length(data.description$points) > 0) {
    if (is.spatial) {
      distance = distance + do.call(sum, lapply(data.description$points, function(point) {
              x.col = point$x
              y.col = point$y
              
              left.attributes <<- left.attributes[left.attributes != x.col] 
              left.attributes <<- left.attributes[left.attributes != y.col] 
              
              distance.euclidean(d1[x.col], d1[y.col], d2[,x.col], d2[,y.col])
            }
          )
        )
    } else {
      distance = distance + do.call(sum, lapply(data.description$points, function(point) {
              x.col = point$x
              y.col = point$y
              
              left.attributes <<- left.attributes[left.attributes != x.col] 
              left.attributes <<- left.attributes[left.attributes != y.col] 
              
              distance <- distance.real(d1[,x.col], d2[,x.col])
              distance <- distance + distance.real(d1[,y.col], d2[,y.col])
              
              return(distance/2)
            }
          )
        )
    }
  }
  
  # polygons
  if(length(data.description$polygons) > 0) {
    if (is.spatial) {
          distance = distance + 5*do.call(sum, lapply(data.description$polygons, function(polygon) {
            left.attributes <<- left.attributes[left.attributes != polygon]
            
            distance.polygons(d1[polygon][1,], d2[polygon][1,])
          }
        )
      )
    } else {
        distance = distance + 5*do.call(sum, lapply(data.description$polygons, function(polygon) {
            left.attributes <<- left.attributes[left.attributes != polygon]
            
            polygon1 <- readWKT(d1[polygon][1,])
            polygon2 <- readWKT(d2[polygon][1,])
            
            bbox1 <- bbox(polygon1)
            bbox2 <- bbox(polygon2)
            
            distance <- distance.real(bbox1["x","min"], bbox2["x","min"])
            distance <- distance + distance.real(bbox1["x","max"], bbox2["x","max"])
            distance <- distance + distance.real(bbox1["y","min"], bbox2["y","min"])
            distance <- distance + distance.real(bbox1["y","max"], bbox2["y","max"])
            
            return(distance/4)
          }
        )
      )
    }
  }
  
  # geographic coordinates
  if(length(data.description$geographic.coordinates) > 0) {
    if (is.spatial) {
      distance = distance + do.call(sum, lapply(data.description$geographic.coordinates, function(geo.point) {
            long.col = geo.point$long
            lat.col = geo.point$lat
            
            left.attributes <<- left.attributes[left.attributes != long.col] 
            left.attributes <<- left.attributes[left.attributes != lat.col] 
            
            distance.haversine(d1[,long.col], d1[,lat.col], d2[,long.col], d2[,lat.col])
          }
        )
      )
    } else {
      distance = distance + do.call(sum, lapply(data.description$geographic.coordinates, function(geo.point) {
            long.col = geo.point$long
            lat.col = geo.point$lat
            
            left.attributes <<- left.attributes[left.attributes != long.col] 
            left.attributes <<- left.attributes[left.attributes != lat.col] 
            
            distance <- distance.real(d1[,long.col], d2[,long.col])
            distance <- distance + distance.real(d1[,lat.col], d2[,lat.col])
            
            return(distance/2)
          }
        )
      )
    }
  }
  
  # string attributes
  if(length(data.description$string.columns) > 0) {
    distance = distance + do.call(sum, lapply(data.description$string.columns,function(column) {
      left.attributes <<- left.attributes[left.attributes != column]
      
      distance.levensthein(d1[column], d2[column])
    }))
  }

  # distance for attributes type real
  if(length(d1[left.attributes]) > 0) {
    distance = distance + do.call(sum, list(mapply(distance.real,d1[left.attributes],d2[left.attributes])))
  }
  
  distance
}

calculate.distances.from.object <- function(objects, object, clustering.description) {
  # Function that calculates distances between given object and the list of objects.
  #
  # Args:
  #   objects: List of objects
  #   object: Core object
  #   clustering.description: Clustering description
  #
  # Returns:
  #   The vector of distances from object to each one from objects.
  #   Vector length is equal to objects number.
  
  print('speeding...')
  distances <- lapply(1:nrow(objects),function(i) { objects.distance(objects[i,], object, clustering.description) })
}

calculate.distance.matrix <- function(objects, clustering.description) {
  # Function that calculates objects distance matrix.
  # It calculates only bottom-left part of matrix, because it is symetric.
  #
  # Args:
  #   objects: List of objects
  #   clustering.description: Clustering description
  #
  # Returns:
  #   Distance matrix between given objects.
  
  require(geosphere)
  
  distances.from.object <- function(objectIndex, objects, clustering.description) {
    distances <- rep(0,nrow(objects))

    attributes.indexes <- get.attributes.columns(objects, clustering.description)
    
    # from the given object to the end of the row
    distances[objectIndex:nrow(objects)] <- calculate.distances.from.object(objects[objectIndex:nrow(objects), attributes.indexes], objects[objectIndex,attributes.indexes], clustering.description)
    return(distances)
  }
  
  # for every object calculate distances to all objects
  # hint: cbind to pionowe z³¹cznie wektorów
  dm <- do.call(cbind, lapply(1:nrow(objects), distances.from.object, objects, clustering.description))
  return(as.dist(dm))
}

spatial.cluster <- function(data, clustering.description) {
  # Function that clusters data.
  #
  # Args:
  #   data: Data to cluster
  #   clustering.description: Clustering description
  #
  # Returns:
  #   Cluster model.
  
  # distance matrix
  objects.distances.matrix <- calculate.distance.matrix(data, clustering.description)
  
  # cluster data
  cluster.model <- pam(objects.distances.matrix, k=clustering.description$params$clusters.number)
  cluster.model
}
