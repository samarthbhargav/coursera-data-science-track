#Q1
data <- read.csv(file = 'getdata-data-ss06hid.csv')
print(strsplit(names(data), "wgtp")[[123]])
#Q2
data <- read.csv('getdata-data-GDP.csv', header = F, nrows=190, as.is = T)
gdp <- gsub(",","",data$V5)
gdp <- gsub(" ","",gdp)
gdp <- gsub("\\.\\.","",gdp)
print(mean(as.numeric(gdp), na.rm=T))
#Q3
print(length(grep("^United", data$V4)))
#Q4
edu <- read.csv('getdata-data-EDSTATS_Country.csv', as.is = T)
data <- merge(data, edu, by.x="V1", by.y="CountryCode")
print(length(grep("^Fiscal year end: June.*", data[[19]])))
#Q5
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn) 
print(length(grep("^2012",sampleTimes)))