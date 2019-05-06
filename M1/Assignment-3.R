CPS <- read.csv("CPSData.csv")
str(CPS)
summary(CPS)

which.max(table(CPS$Industry))
which.min(sort(table(CPS$State)))
CPS$State[max(table(CPS$State))]
table(CPS$Citizenship)

hispanic <- subset(CPS, CPS$Hispanic==T)
table(hispanic$Race)

summary(CPS)

table(CPS$Region, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))

table(CPS$State, is.na(CPS$MetroAreaCode))
table(CPS$Region, is.na(CPS$MetroAreaCode))

sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

MetroAreaMap <- read.csv("MetroAreaCodes.csv")
CountryMap <- read.csv("CountryCodes.csv")
nrow(MetroAreaMap)
nrow(CountryMap)

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(CPS)
str(CPS)
sort(CPS$MetroArea)
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean, na.rm=T))
sort(tapply(CPS$Race=="Asian", CPS$MetroArea, mean, na.rm=T))

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=T))
CPS = merge(CPS, CountryMap, by.x = "CountryOfBirthCode", by.y = "Code", all.x = TRUE)
str(CPS)

sum(is.na(CPS$Country))
sort(table(CPS$CountryOfBirthCode))
newyork <- subset(CPS, CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA", na.rm=T)
mean(newyork$Country!="United States", na.rm=T)
sort(tapply(CPS$Country=="India", CPS$MetroArea, sum, na.rm=T))
sort(tapply(CPS$Country=="Brazil", CPS$MetroArea, sum, na.rm=T))
sort(tapply(CPS$Country=="Somalia", CPS$MetroArea, sum, na.rm=T))