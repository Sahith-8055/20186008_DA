edges <- read.csv("edges.csv")
str(edges)
users <- read.csv("users.csv")
str(users)

nrow(edges) * 2 / nrow(users)

tapply(users$id, users$id, mean)

table(users$locale)

table(users$gender, users$school)

install.packages("igraph")
library(igraph)

g <- graph.data.frame(edges, FALSE, users)
plot(g, vertex.size = 5, vertex.label = NA)

table(degree(g))
# 9 friends have friends more than 10.

V(g)$size <- degree(g) / 2 + 2

plot(g, vertex.label = NA)
# The maximum assigned size is 11 and the minimum assigned size is 2.

V(g)$color <- "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label = NA)

V(g)$color <- "black"
V(g)$color[V(g)$school == "A"] = "blue"
V(g)$color[V(g)$school == "AB"] = "green"
plot(g, vertex.label = NA)


V(g)$color <- "black"
V(g)$color[V(g)$locale == "A"] = "dark red"
V(g)$color[V(g)$locale == "B"] = "dark blue"
plot(g, vertex.label = NA)

?igraph.plotting

install.packages("rgl")
library(rgl)

rglplot(g, vertex.label = NA)
plot(g, edge.width = 2, vertex.label = NA)
