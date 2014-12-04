#
# SPDB project
# Spatial data clustering study
#
# author: Mateusz Zawislak
# December 2014
#

library(sp)


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

objects.distance <- function(d1, d2) {
  distance = 0
  for(i in 1:length(d1)) {
    # TODO
    distance = distance + distance.real(d1[1,i],d2[1,i])
  }
  
  distance
}

# returns the vector of distances from object to each one from objects
# vector length is equal to objects count
calculate.distances.from.object <- function(objects, object) {
  distances <- c()
  for(i in 1:nrow(objects)) {
    dist <- objects.distance(objects[i,], object)
    distances <- append(distances, dist)
  }
  
  distances
}

# calculates objects distance matrix
# calculates only bottom-left part of matrix, because it is symetric
calculate.distance.matrix <- function(objects) {
  require(geosphere)
  
  distances.from.object <- function(objectIndex, objects) {
    distances <- rep(0,nrow(objects))

    # from the given object to the end of the row
    distances[objectIndex:nrow(objects)] <- calculate.distances.from.object(objects[objectIndex:nrow(objects),2:ncol(objects)],objects[objectIndex,2:ncol(objects)])
    return(distances)
  }
  
  # for every object calculate distances to all objects
  # hint: cbind to pionowe z³¹cznie wektorów
  dm <- do.call(cbind, lapply(1:nrow(objects), distances.from.object, objects))
  return(as.dist(dm))
}


main <- function() {
  uciData <- read.csv(file="D:/Github/spatial-clustering/data/wine.data", head=FALSE, sep=",", skip=0)
  
  # distance matrix
  objects.distances.matrix <- calculate.distance.matrix(uciData)
  km <- kmeans(objects.distances.matrix, centers=3)
  print(km)
}
