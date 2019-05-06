data(state)
statedata <- cbind(data.frame(state.x77), state.abb, state.area, state.center, state.division, state.name, state.region)
# statedata <- read.csv("statedata.csv")
str(statedata)

plot(statedata$x, statedata$y, main = "Plot", pch=5, type = "b")

boxplot(statedata$Murder ~ statedata$state.region)
tapply(statedata$HS.Grad, state.region, mean)

ne <- subset(statedata, state.region=="Northeast")
str(ne)
table(ne$state.name, ne$Murder)
# table(ne$Murder,ne$state.name)
# table(ne$Murder,ne$state.abb)

model1 <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(model1)

plot(statedata$Income, statedata$Life.Exp, main = "Plot", pch=2, type = "b")

model1 <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(model1)

predict1 <- predict(model1, newdata = statedata)
sort(predict1)

state.name[which.min(statedata$Life.Exp)]
state.name[which.max(statedata$Life.Exp)]

