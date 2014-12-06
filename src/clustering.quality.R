#
# SPDB project
# Spatial data clustering study
#
# author: Mateusz Zawislak
# December 2014
#

source('clustering.core.R')

split.data <- function(data, percent) {
  # calculate columns count
  columns.count <- floor(nrow(data) * percent / 100)
  # get data random indexes
  random.indexes <- sample(seq(1,nrow(data)),columns.count, replace=FALSE)
  
  # select data
  selected.data <- data[random.indexes,]
  
  selected.data
}

rate.cluster <- function(cluster.model, data, data.description) {
  class.column <- data.description$classIndex
  
  # TODO
  sample(seq(0.1, 0.9, 0.1), 1)
}

remove.class.column <- function(data, classIndex) {
  data[,-classIndex]
}

test.clustering <- function(data, data.description, data.description.spatial) {
  execution.times <- c()
  execution.times.spatial <- c()
  clustering.quality <- c()
  clustering.quality.spatial <- c()
  
  portions <- seq(10, 100, 10)
  
  for(portion in portions) {
    splitted.data <- split.data(data, portion)
    
    execution.time <- system.time(cluster.model <- spatial.cluster(splitted.data, data.description))
    execution.time.spatial <- system.time(cluster.model.spatial <- spatial.cluster(splitted.data, data.description.spatial))
    
    quality <- rate.cluster(cluster.model, data, data.description)
    quality.spatial <- rate.cluster(cluster.model.spatial, data, data.description)
    
    print('done')
    execution.times = append(execution.times, execution.time[3])
    execution.times.spatial = append(execution.times.spatial, execution.time.spatial[3])
    clustering.quality = append(clustering.quality, quality)
    clustering.quality.spatial = append(clustering.quality.spatial, quality.spatial)
  }
  
  # plots
  plot(portions, clustering.quality, type = "p", pch = 4, col = "blue", xlab = "percent of training data [%]", ylab = "clustering quality")
  points(portions, clustering.quality.spatial, type = "p", pch = 8, col = "red")
  
  plot(portions, execution.times.spatial, type = "l", col = "red", xlab = "percent of training data [%]", ylab = "execution time [s]")
  points(portions, execution.times, type = "l", col = "blue")
}
