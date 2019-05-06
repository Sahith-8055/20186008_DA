movies <- read.table("movieLens.txt", header = FALSE, sep = "|", quote = "\"")
str(movies)

colnames(movies) <- c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
str(movies)

movies$ID <- NULL
movies$ReleaseDate <- NULL
movies$VideoReleaseDate <- NULL
movies$IMDB <- NULL
movies <- unique(movies)
str(movies)

distances <- dist(movies[2:20], method = "euclidean")
clusterMovies <- hclust(distances, method = "ward")
plot(clusterMovies)
clusterGroups <- cutree(clusterMovies, k = 10)
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)
subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]
cluster2 <- subset(movies, clusterGroups==2)
cluster2$Title[1:10]
