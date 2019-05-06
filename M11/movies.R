movies <- read.table("Movies.txt", header = FALSE, sep = "|", quote = "\"")

colnames(movies) <- c("ID","Title","ReleaseDate","VideoReleaseDate","IMDB","Unknown","Action","Adventure","Animation","Childrens","Comedy","Crime","Documentary","Drama","Fantasy","FilmNoir","Horror","Musical","Mystery","Romance","SciFi","Thriller","War","Western")
str(movies)

movies$ID <- NULL
movies$Title <- NULL
movies$ReleaseDate <- NULL
movies$VideoReleaseDate <- NULL

str(movies)

table(movies$Action, movies$Horror)

distances <- dist(movies[2:20], method = "euclidean")
clusterMovies <- hclust(distances, method = "ward.D")

plot(clusterMovies)

rect.hclust(clusterMovies, k = 8, border = "red")
rect.hclust(clusterMovies, k = 4, border = "blue")
rect.hclust(clusterMovies, k = 3, border = "yellow")

clusterGroups <- cutree(clusterMovies, k = 7)
sort(table(clusterGroups))

colSums(subset(movies[2:20], clusterGroups == 1))
colSums(subset(movies[2:20], clusterGroups == 2))
colSums(subset(movies[2:20], clusterGroups == 3))
colSums(subset(movies[2:20], clusterGroups == 4))
colSums(subset(movies[2:20], clusterGroups == 5))
colSums(subset(movies[2:20], clusterGroups == 6))
colSums(subset(movies[2:20], clusterGroups == 7))

table(clusterGroups, movies$Adventure)


set.seed(1000)
kmc <- kmeans(movies[2:20], centers = 7)
str(kmc)
sort(table(kmc$cluster))

table(clusterGroups, kmc$cluster)


colSums(subset(movies[2:20], kmc$cluster == 1))
colSums(subset(movies[2:20], kmc$cluster == 2))
colSums(subset(movies[2:20], kmc$cluster == 3))
colSums(subset(movies[2:20], kmc$cluster == 4))
colSums(subset(movies[2:20], kmc$cluster == 5))
colSums(subset(movies[2:20], kmc$cluster == 6))
colSums(subset(movies[2:20], kmc$cluster == 7))
