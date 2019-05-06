library(ggplot2)
library(ggmap)

intlall <- read.csv("intlall.csv", stringsAsFactors = FALSE)
str(intlall)

head(intlall)

intlall[is.na(intlall)] = 0
head(intlall)

world_map <- map_data("world")
str(world_map)

world_map <- merge(world_map, intlall, by.x = "region", by.y = "Citizenship")
str(world_map)

ggplot(world_map, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") + coord_map("mercator")
install.packages("mapproj")
library(mapproj)

world_map <- world_map[order(world_map$region, world_map$order),]

table(intlall$Citizenship)

intlall$Citizenship[intlall$Citizenship == "China (People's Republic Of)"] = "China"
world_map <- merge(map_data("world"), intlall, by.x = "region", by.y = "Citizenship")

ggplot(world_map, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = Total), color = "black") + coord_map("mercator")
ggplot(world_map, aes(x = long, y = lat, group = group)) + geom_polygon(aes(fill = Total), color = "black") + coord_map("ortho", orientation = c(20,30,0))
