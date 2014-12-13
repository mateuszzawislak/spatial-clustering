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

# example data description
# by default columns are treated as real
example.data.description <- list(
    "class.column" = "V1",
    "ignore.columns" = "V9",
    "string.columns" = c("V10"),
    "points" = list(list("x" = "V2", "y" = "V3"),list("x" = "V4", "y" = "V5")),
    "geographic.coordinates" = list(list("long" = "V7", "lat" = "V6"))
  )

printDataDescription <- function(description) {
  # Function that prints data description.
  #
  # Args:
  #   description: Data description
  
  print('Class column')
  if(exists(description$class.column)) {
    print(description$class.column)
  }
  
  if(length(description$points) > 0 ) {
    for(i in 1:length(description$points)) {
      print('Point: ')
      print(description$points[i])
    }
  }
}

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

get.attributes.columns <- function(data, data.description) {
  # Args:
  #   data: Data
  #   data.description: Data description
  #
  # Returns:
  #   Attributes array that should be calculated by the clustering algorithm
  
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

objects.distance <- function(d1, d2, data.description) {
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
  
  distance = 0
  
  left.attributes <- names(d1)
  
  # points
  if(length(data.description$points) > 0) {
    distance = distance + do.call(sum, lapply(data.description$points, function(point) {
            x.col = point$x
            y.col = point$y
            
            left.attributes <<- left.attributes[left.attributes != x.col] 
            left.attributes <<- left.attributes[left.attributes != y.col] 
            
            distance.euclidean(d1[x.col], d1[y.col], d2[,x.col], d2[,y.col])
          }
        )
      )
  }
  
  if(length(data.description$polygons) > 0) {
    distance = distance + 5*do.call(sum, lapply(data.description$polygons, function(polygon) {
      left.attributes <<- left.attributes[left.attributes != polygon]
      
      distance.polygons(d1[polygon][1,], d2[polygon][1,])
    }
    )
    )
  }
  
  # geographic coordinates
  if(length(data.description$geographic.coordinates) > 0) {
    distance = distance + do.call(sum, lapply(data.description$geographic.coordinates, function(geo.point) {
          long.col = geo.point$long
          lat.col = geo.point$lat
          
          left.attributes <<- left.attributes[left.attributes != long.col] 
          left.attributes <<- left.attributes[left.attributes != lat.col] 
          
          distance.haversine(d1[,long.col], d1[,lat.col], d2[,long.col], d2[,lat.col])
        }
      )
    )
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

calculate.distances.from.object <- function(objects, object, data.description) {
  # Function that calculates distances between given object and the list of objects.
  #
  # Args:
  #   objects: List of objects
  #   object: Core object
  #   data.description: Data description
  #
  # Returns:
  #   The vector of distances from object to each one from objects.
  #   Vector length is equal to objects number.
  
  print('speeding...')
  distances <- lapply(1:nrow(objects),function(i) { objects.distance(objects[i,],object,data.description) })
}

calculate.distance.matrix <- function(objects, data.description) {
  # Function that calculates objects distance matrix.
  # It calculates only bottom-left part of matrix, because it is symetric.
  #
  # Args:
  #   objects: List of objects
  #   data.description: Data description
  #
  # Returns:
  #   Distance matrix between given objects.
  
  require(geosphere)
  
  distances.from.object <- function(objectIndex, objects, data.description) {
    distances <- rep(0,nrow(objects))

    attributes.indexes <- get.attributes.columns(objects, data.description)
    
    # from the given object to the end of the row
    distances[objectIndex:nrow(objects)] <- calculate.distances.from.object(objects[objectIndex:nrow(objects),attributes.indexes],objects[objectIndex,attributes.indexes],data.description)
    return(distances)
  }
  
  # for every object calculate distances to all objects
  # hint: cbind to pionowe z³¹cznie wektorów
  dm <- do.call(cbind, lapply(1:nrow(objects), distances.from.object, objects, data.description))
  return(as.dist(dm))
}

spatial.cluster <- function(data, data.description) {
  # Function that clusters data.
  #
  # Args:
  #   data: Data to cluster
  #   data.description: Data description
  #
  # Returns:
  #   Cluster model.
  
  # distance matrix
  objects.distances.matrix <- calculate.distance.matrix(data, data.description)
  
  # cluster data
  cluster.model <- pam(objects.distances.matrix, k=6)
  cluster.model
}
