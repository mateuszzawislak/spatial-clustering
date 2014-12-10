#
# SPDB project
# Spatial data clustering study
#
# author: Mateusz Zawislak
# December 2014
#

source('clustering.core.R')

split.data <- function(data, percent) {
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
  selected.data <- data[random.indexes,]
  
  selected.data
}

rate.cluster <- function(cluster.model, data, data.description) {
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

remove.class.column <- function(data, classIndex) {
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

test.clustering <- function(data, data.description, data.description.spatial) {
  # Function testing clustering method.
  #
  # Args:
  #   data: Data.
  #   data.description: Data description without spatial attributes.
  #   data.description.spatial: Data description with spatial attributes.
  #
  # Returns:
  #   Nothing returns, just draws plots.
  
  execution.times <- c()
  execution.times.spatial <- c()
  clustering.quality <- c()
  clustering.quality.spatial <- c()
  
  portions <- seq(1, 2, 1)
  
  for(portion in portions) {
    splitted.data <- split.data(data, portion)
    
    execution.time <- system.time(cluster.model <- spatial.cluster(splitted.data, data.description))
    execution.time.spatial <- system.time(cluster.model.spatial <- spatial.cluster(splitted.data, data.description.spatial))
    
    quality <- rate.cluster(cluster.model, splitted.data, data.description)
    quality.spatial <- rate.cluster(cluster.model.spatial, splitted.data, data.description)
    
    print('done')
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
  plot(portions, execution.times.spatial, type = "l", col = "red", xlab = "percent of training data [%]", ylab = "execution time [s]")
  print('Normal times:')
  points(portions, execution.times, type = "l", col = "blue")
}
