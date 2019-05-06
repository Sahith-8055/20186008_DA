library(ggplot2)
intl <- read.csv("intl.csv")
str(intl)

ggplot(intl, aes(x = Region, y = PercentOfIntl)) + geom_bar(stat = "identity", fill = "red") + ylab("Percentage of International Students") + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(intl, aes(x = Region, y = PercentOfIntl)) + coord_polar("x", start = 0, direction = 1)
