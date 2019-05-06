songs <- read.csv("songs.csv")
str(songs)
table(songs$artistname=="Michael Jackson")
# 18 songs

sub2010 <- subset(songs, year==2010)
str(sub2010)
nrow(sub2010)
# 373 observations

jack <- subset(songs, artistname=="Michael Jackson")
jackTop10 <- subset(jack, Top10==1)
jackTop10$songtitle
# You Rock My World
# You Are Not Alone

table(songs$timesignature)
# 0, 1, 3, 4, 5, 7
sort(table(songs$timesignature))
# 4 is the most frequently occuring value

songs[which.max(songs$tempo), "songtitle"]
# Wanna Be Startin' Somethin'

table(songs$year)
songsTrain <- subset(songs, year < 2010)
songsTest <- subset(songs, year >= 2010)
nonvars <- c("year", "songtitle", "artistname", "songID", "artistID")
songsTrain <- songsTrain[, !(names(songsTrain)%in%nonvars)]
songsTest <- songsTest[, !names(songsTest)%in%nonvars]
str(songsTrain)
str(songsTest)
Topmodel1 <- glm(Top10 ~ ., data = songsTrain, family = "binomial")
summary(Topmodel1)
cor.test(songsTrain$loudness, songsTrain$energy)
Topmodel2 <- glm(Top10 ~ .-loudness, data = songsTrain, family = "binomial")
summary(Topmodel2)
Topmodel3 <- glm(Top10 ~ .-energy, data = songsTrain, family = "binomial")
summary(Topmodel3)
Topprediction1 <- predict(Topmodel3, newdata = songsTest, type = "response")
table(songsTest$Top10, Topprediction1 >= 0.45)
(309+19)/(309+19+5+40)
# 19 songs are likely to make it to Top10
# 5 songs the model predicted would make cut to Top10 didn't make it
19/59
# sensitivity of the current model is 0.3220339
309/314
# specificity of the current model is 0.9840764
