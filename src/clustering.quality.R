#
# SPDB project
# Spatial data clustering study
#
# author: Mateusz Zawislak
# December 2014
#

source('clustering.core.R')

splitData <- function(data, percent) {
  # Args:
  #   data: Data which should be splitted.
  #   precent: Interesting percent of data.
  #
  # Returns:
  #   Sample part of the given data.
  
  # calculate columns count
  columns.count <- floor(nrow(data) * percent / 100)
  # get data random indexes
  random.indexes <- sample(seq(1,nrow(data)),columns.count, replace=FALSE)
  
  # select data
  selected.data <- data[sort(random.indexes),]
  
  selected.data
}

rateCluster <- function(cluster.model, data, data.description) {
  # Function evalutaing clustering model.
  #
  # Args:
  #   cluster.model: Cluster model.
  #   data: Data.
  #   data.description: Data description.
  #
  # Returns:
  #   Cluster mdoel quality (Rand index).
  
  class.column <- data.description$class.column
  
  quality = 0
  
  # rand index
  if(is.null(data.description$class.column) == FALSE) {
    a <- 0
    b <- 0
    
    lapply(1:length(cluster.model$cluster), function(i) {
      if(i+1 <= length(cluster.model$cluster)) {
        lapply((i+1):length(cluster.model$cluster), function(j) {
          i.class = data[i,][class.column]
          j.class = data[j,][class.column]
          
          i.prediction = cluster.model$cluster[i]
          j.prediction = cluster.model$cluster[j]
          if(i.prediction == j.prediction && i.class == j.class) {
            a <<- a + 1
          } else if(i.prediction != j.prediction && i.class != j.class) {
            b <<- b + 1
          }
        })
      }
    })
    
    quality <- 2*(a+b)/(nrow(data)*(nrow(data) - 1))
  }
  
  quality
}

removeClassColumn <- function(data, classIndex) {
  # Removes given column from the data.
  #
  # Args:
  #   data: Data.
  #   classIndex: Attribute column index.
  #
  # Returns:
  #   Data without given attribute.
  
  data[,-classIndex]
}

testClustering <- function(data, clustering.description) {
  # Function testing clustering method.
  #
  # Args:
  #   data: Data.
  #   clustering.description: Data description and clustering params.
  #
  # Returns:
  #   Nothing returns, just draws plots.
  
  data.description <- clustering.description$data.description
  clustering.description.nonspatial <- clustering.description
  clustering.description.nonspatial$params$spatial <- FALSE
  clustering.description.spatial <- clustering.description
  clustering.description.spatial$params$spatial <- TRUE
  
  execution.times <- c()
  execution.times.spatial <- c()
  clustering.quality <- c()
  clustering.quality.spatial <- c()
  
  portions <- seq(100, 100, 10)
  
  for(portion in portions) {
    splitted.data <- splitData(data, portion)
    
    execution.time <- system.time(cluster.model <- spatialCluster(splitted.data, clustering.description.nonspatial))
    execution.time.spatial <- system.time(cluster.model.spatial <- spatialCluster(splitted.data, clustering.description.spatial))
    
    quality <- rateCluster(cluster.model, splitted.data, data.description)
    quality.spatial <- rateCluster(cluster.model.spatial, splitted.data, data.description)
    
    print('Done part:')
    print(portion)
    
    execution.times = append(execution.times, execution.time[3])
    execution.times.spatial = append(execution.times.spatial, execution.time.spatial[3])
    clustering.quality = append(clustering.quality, quality)
    clustering.quality.spatial = append(clustering.quality.spatial, quality.spatial)
  }
  
  # plots
  print('Normal clustering quality:')
  print(clustering.quality)
  plot(portions, clustering.quality, type = "p", pch = 4, col = "blue", xlab = "percent of training data [%]", ylab = "clustering quality", ylim = range(c(clustering.quality,clustering.quality.spatial)))
  
  print('Spatial clustering quality:')
  print(clustering.quality.spatial)
  points(portions, clustering.quality.spatial, type = "p", pch = 8, col = "red")
  
  print('Spatial times:')
  print(execution.times.spatial)
  plot(portions, execution.times.spatial, type = "l", col = "red", xlab = "percent of training data [%]", ylab = "execution time [s]")
  print('Normal times:')
  print(execution.times)
  points(portions, execution.times, type = "l", col = "blue")
}
