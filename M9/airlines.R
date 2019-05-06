airlines <- read.csv("AirlinesCluster.csv")
summary(airlines)

library(caret)

preproc <- preProcess(airlines)
airlinesNorm <- predict(preproc, airlines)
summary(airlinesNorm)

sd(airlinesNorm$Balance)
sd(airlinesNorm$QualMiles)
sd(airlinesNorm$BonusMiles)
sd(airlinesNorm$BonusTrans)
sd(airlinesNorm$FlightMiles)
sd(airlinesNorm$FlightTrans)
sd(airlinesNorm$DaysSinceEnroll)

distances <- dist(airlinesNorm, method = "euclidean")
HierGroups <- hclust(distances, method = "ward.D")
plot(HierGroups)

rect.hclust(HierGroups, k = 2, border = "red")
rect.hclust(HierGroups, k = 3, border = "blue")
rect.hclust(HierGroups, k = 6, border = "green")
rect.hclust(HierGroups, k = 7, border = "yellow")

ClusterGroups <- cutree(HierGroups, k = 5)
table(ClusterGroups)
# The number of observations in the Cluster1 are 776.

MainCluster <- split(airlinesNorm, ClusterGroups)

tapply(airlines$Balance, ClusterGroups, mean)
tapply(airlines$QualMiles, ClusterGroups, mean)
tapply(airlines$BonusMiles, ClusterGroups, mean)
tapply(airlines$BonusTrans, ClusterGroups, mean)
tapply(airlines$FlightMiles, ClusterGroups, mean)
tapply(airlines$FlightTrans, ClusterGroups, mean)
tapply(airlines$DaysSinceEnroll, ClusterGroups, mean)

colMeans(MainCluster[[1]])


set.seed(88)
kmc <- kmeans(airlinesNorm, centers = 5, iter.max = 1000)
KmeansCluster <- split(airlinesNorm, kmc$cluster)

table(kmc$cluster)
# There are 2 clusters which have more than 1000 observations.

kmc$centers

tapply(airlines$Balance, kmc$cluster, mean)
tapply(airlines$QualMiles, kmc$cluster, mean)
tapply(airlines$BonusMiles, kmc$cluster, mean)
tapply(airlines$BonusTrans, kmc$cluster, mean)
tapply(airlines$FlightMiles, kmc$cluster, mean)
tapply(airlines$FlightTrans, kmc$cluster, mean)
tapply(airlines$DaysSinceEnroll, kmc$cluster, mean)
