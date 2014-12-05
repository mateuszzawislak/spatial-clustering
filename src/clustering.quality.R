#
# SPDB project
# Spatial data clustering study
#
# author: Mateusz Zawislak
# December 2014
#

source('clustering.core.R')

rate.cluster <- function(cluster.model, data, data.description) {
  class.column <- data.description$classIndex
  
  # TODO
}

remove.class.column <- function(data, classIndex) {
  data[,-classIndex]
}

test.clustering <- function(data, data.description) {
  cluster.model <- spatial.cluster(data, data.description)
  
  quality <- rate.cluster(cluster.model, data, data.description)
  
  print(cluster.model)
  print('Quality')
  print(quality)
}
