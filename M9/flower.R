flower <- read.csv("flower.csv", header = FALSE)
str(flower)

flowerMatrix <- as.matrix(flower)
str(flowerMatrix)

flowerVector <- as.vector(flowerMatrix)
str(flowerVector)

flowerVector2 <- as.vector(flower)
str(flowerVector2)

distance <- dist(flowerVector, method = "euclidean")
clusterIntensity <- hclust(distance, method = "ward.D")
plot(clusterIntensity)
rect.hclust(clusterIntensity, k = 3, border = "red")
rect.hclust(clusterIntensity, k = 2, border = "green")
flowerClusters <- cutree(clusterIntensity, k = 3)
flowerClusters
tapply(flowerVector, flowerClusters, mean)

dim(flowerClusters) = c(50, 50)
image(flowerClusters, axis = FALSE)
image(flowerMatrix, axis = FALSE, col = grey(seq(0,1,length = 256)))
