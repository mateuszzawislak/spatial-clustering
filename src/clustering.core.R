library(sp)


# http://stackoverflow.com/questions/21095643/approaches-for-spatial-geodesic-latitude-longitude-clustering-in-r-with-geodesic

cities <- read.csv("C:/Users/Mateusz/Documents/R-Projects/GeoLiteCity-Location.csv",header=T,skip=1)
CA     <- cities[cities$country=="US" & cities$region=="CA",]
CA     <- CA[sample(1:nrow(CA),100),]   # 100 random cities in California
locationsData     <- data.frame(long=CA$long, lat=CA$lat, city=CA$city, metroCode=CA$metroCode)

# zwraca wektor o dlugosci rownej dlugosci wektora p1
# zwracany wektor zawiera odleglosci miedzy kolejnymi punktami z p1 a punktem p2
dMza <- function(p1, p2) {
  distHaversine(p1,p2)
}

# returns distance matrix
metric.distance <- function(locationsData) {
  require(geosphere)
  distances.from.point <- function(numerWiersza,lokalizacje) {         # z[1:2] contain long, lat
    dist <- rep(0,nrow(lokalizacje))
    # ode danego wiersza do konca zeby nie powtarzac macierzy symetrycznie
    dist[numerWiersza:nrow(lokalizacje)] <- dMza(lokalizacje[numerWiersza:nrow(lokalizacje),1:2],lokalizacje[numerWiersza,1:2])
    return(dist)
  }
  
  # dla kazdego wiersza zaaplikuj funckje d do wszyskich lokalizacji
  # locationsData to parametr funkcji d
  dm <- do.call(cbind, lapply(1:nrow(locationsData), distances.from.point, locationsData))
  return(as.dist(dm))
}

d      <- metric.distance(locationsData)   # distance matrix
km <- kmeans(metric.distance(locationsData),centers=4)

