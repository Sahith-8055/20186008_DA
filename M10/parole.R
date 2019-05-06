parole <- read.csv("parole.csv")

parole$male <- as.factor(parole$male)
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)

tbl1 <- table(parole$male, parole$violator)
tbl1[3] / (14 + 64)
# The proportion of female violators is 0.1794872.

table(parole$crime, parole$state)

library(ggplot2)

ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, boundary = 0, color = 'black', fill = 'cornflowerblue')

ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, boundary = 0, color = 'blue', fill = 'cornflowerblue')

ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, boundary = 0) + facet_grid(male ~ .)

ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, boundary = 0) + facet_grid(.~ male)

ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, boundary = 0)

colorPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, boundary = 0) + scale_fill_manual(values = colorPalette)

ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, boundary = 0, position = "identity", alpha = 0.5) + scale_fill_manual(values = colorPalette)


str(parole)

ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1, boundary = 0, color = 'black', fill = 'cornflowerblue')

ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 0.1, boundary = 0, color = 'black', fill = 'cornflowerblue')

ggplot(data = parole, aes(x = time.served)) + geom_histogram(binwidth = 1, boundary = 0) + facet_grid(crime ~ .)


ggplot(data = parole, aes(x = time.served, fill = crime)) + geom_histogram(binwidth = 1, boundary = 0, position = "identity", alpha = 0.5)
