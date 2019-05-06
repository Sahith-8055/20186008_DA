baseball <- read.csv("baseball.csv")
str(baseball)
# 1232 are the number of observations in the baseball set.

length(table(baseball$Year))
# 47 are the number of years present in the category of years.

baseball <- subset(baseball, baseball$Playoffs==1)
str(baseball)
# 244 are the number of team/year pairs that went to playoffs.

table(baseball$Year)
table(table(baseball$Year))

PlayOffTable <- table(baseball$Year)
PlayOffTable
names(PlayOffTable)
str(names(PlayOffTable))
PlayOffTable[c("1990", "2001")]
baseball$NumCompetitors <- PlayOffTable[as.character(baseball$Year)]
baseball$NumCompetitors
table(baseball$NumCompetitors)
baseball$WorldSeries <- as.numeric(baseball$RankPlayoffs==1)
table(baseball$WorldSeries)

baseballModel1 <- glm(WorldSeries ~ League, data = baseball)
summary(baseballModel1)
baseballModel2 <- glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball)
summary(baseballModel2)
cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])
