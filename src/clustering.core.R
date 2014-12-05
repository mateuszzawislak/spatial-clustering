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
    "points" = list(list("x" = 2, "y" = 3),list("x" = 4, "y" = 5)),
    "geoPoints" = list(list("long" = 7, "lat" = 6))
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

objects.distance <- function(d1, d2, data.description) {
  distance = 0
  for(i in 1:length(d1)) {
    # TODO
    distance = distance + distance.real(d1[1,i],d2[1,i])
  }
  
  distance
}

# returns the vector of distances from object to each one from objects
# vector length is equal to objects count
calculate.distances.from.object <- function(objects, object, data.description) {
  distances <- c()
  for(i in 1:nrow(objects)) {
    dist <- objects.distance(objects[i,], object, data.description)
    distances <- append(distances, dist)
  }
  
  distances
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
