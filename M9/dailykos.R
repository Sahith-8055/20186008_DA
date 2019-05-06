dailykos <- read.csv("dailykos.csv")
str(dailykos)

distances <- dist(dailykos, method = "euclidean")
ClusterKos <- hclust(distances, method = "ward.D")
summary(dailykos)

plot(ClusterKos)
rect.hclust(ClusterKos, k = 3, border = "red")
rect.hclust(ClusterKos, k = 4, border = "green")
rect.hclust(ClusterKos, k = 2, border = "blue")

rect.hclust(ClusterKos, k = 7, border = "yellow")
ClusterGroups <- cutree(ClusterKos, k = 7)

Cluster1 <- subset(dailykos, ClusterGroups == 1)
Cluster2 <- subset(dailykos, ClusterGroups == 2)
Cluster3 <- subset(dailykos, ClusterGroups == 3)
Cluster4 <- subset(dailykos, ClusterGroups == 4)
Cluster5 <- subset(dailykos, ClusterGroups == 5)
Cluster6 <- subset(dailykos, ClusterGroups == 6)
Cluster7 <- subset(dailykos, ClusterGroups == 7)

str(Cluster3)
nrow(Cluster3)
# 374 observations are present in the Cluster 3.
nrow(Cluster1)
nrow(Cluster2)
nrow(Cluster4)
nrow(Cluster5)
nrow(Cluster6)
nrow(Cluster7)
# Cluster 1 and Cluster 4 have the maximum and minimum number of observations.

MainCluster <- split(dailykos, ClusterGroups)
MainCluster[[1]]
nrow(MainCluster[[1]])

tail(sort(colMeans(MainCluster[[1]])))
tail(sort(colMeans(MainCluster[[2]])))
tail(sort(colMeans(MainCluster[[3]])))
tail(sort(colMeans(MainCluster[[4]])))
tail(sort(colMeans(MainCluster[[5]])))
tail(sort(colMeans(MainCluster[[6]])))
tail(sort(colMeans(MainCluster[[7]])))


set.seed(1000)
kmc <- kmeans(dailykos, centers = 7)
str(kmc)

KmeansMainCluster <- split(dailykos, kmc$cluster)
nrow(KmeansMainCluster[[3]])
# There are 277 observations.

sort(table(kmc$cluster))
# Cluster 4 and Cluster 2 have the maximum and minimum observations respectively.

tail(sort(colMeans(KmeansMainCluster[[1]])))
tail(sort(colMeans(KmeansMainCluster[[2]])))
tail(sort(colMeans(KmeansMainCluster[[3]])))
tail(sort(colMeans(KmeansMainCluster[[4]])))
tail(sort(colMeans(KmeansMainCluster[[5]])))
tail(sort(colMeans(KmeansMainCluster[[6]])))
tail(sort(colMeans(KmeansMainCluster[[7]])))

table(ClusterGroups, kmc$cluster)
