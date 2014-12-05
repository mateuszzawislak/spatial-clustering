#
# SPDB project
# Spatial data clustering study
#
# author: Mateusz Zawislak
# December 2014
#

library(sp)


# by default columns are treated as real
example.data.description <- list(
    "classIndex" = 1,
    "points" = list(list("x" = "V2", "y" = "V3"),list("x" = "V4", "y" = "V5")),
    "geoPoints" = list(list("long" = "V7", "lat" = "V6"))
  )

printDataDescription <- function(description) {
  print('Class index')
  print(description$classIndex)
  
  if(length(description$points) > 0 ) {
    for(i in 1:length(description$points)) {
      print('Point: ')
      print(description$points[i])
    }
  }
}

# Utils
get.column.index <- function(data, columnName) {
  match(columnName, colnames(data))
}

get.column.name <- function(data, index) {
  colnames(data)[index]
}

get.attributes.indexes <- function(columns.number, data.description) {
  indexes <- 1:columns.number
  indexes[-data.description$classIndex]
}


# distance metrics
distance.bray.curtis <- function(val1, val2) {
  abs(val1 - val2) / (val1 + val2)
}

distance.real <- function(val1, val2) {
  difference <- abs(val1 - val2)
  if(difference != 0) {
    difference = difference / sqrt(val1*val1+val2*val2)
  }
  
  difference
}

distance.euclidean <- function(x1, y1, x2, y2) {
  dist(rbind(c(x1,y1),c(x2,y2)), method = "euclidean")
}

objects.distance <- function(d1, d2, data.description) {
  distance = 0
  
  left.attributes <- names(d1)
  
  # points
  if(length(data.description$points) > 0) {
    distance = distance + do.call(sum, lapply(data.description$points, function(point) {
            x.col = point$x
            y.col = point$y
            
            left.attributes <<- left.attributes[left.attributes != x.col] 
            left.attributes <<- left.attributes[left.attributes != y.col] 
            
            distance.euclidean(d1[x.col], d1[y.col], d2[x.col], d2[y.col])
          }
        )
      )
  }

  if(length(d1[left.attributes]) > 0) {
    distance = distance + do.call(sum, list(mapply(distance.real,d1[left.attributes],d2[left.attributes])))
  }
  
  distance
}

# returns the vector of distances from object to each one from objects
# vector length is equal to objects count
calculate.distances.from.object <- function(objects, object, data.description) {
  print('speeding...')
  distances <- apply(objects,1,function(neighbour) { objects.distance(neighbour,object,data.description) })
}

# calculates objects distance matrix
# calculates only bottom-left part of matrix, because it is symetric
calculate.distance.matrix <- function(objects, data.description) {
  require(geosphere)
  
  distances.from.object <- function(objectIndex, objects, data.description) {
    distances <- rep(0,nrow(objects))

    attributes.indexes <- get.attributes.indexes(ncol(objects), data.description)
    
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
  # distance matrix
  objects.distances.matrix <- calculate.distance.matrix(data, data.description)
  
  # cluster data
  cluster.model <- kmeans(objects.distances.matrix, centers=3)
  cluster.model
}
