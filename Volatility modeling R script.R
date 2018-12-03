clear()

rm(list = ls())
graphics.off()
setwd("~/Masters/ECO5069S/Project-20181009T150358Z-001/Project")


library(readxl)
# install.packages("devtools")
# devtools::install_github("KevinKotze/tsm")
# install.packages("mFilter", repos = "https://cran.rstudio.com/")
# install.packages("forecast")
library(forecast)
# install.packages("tsm")
library(tsm)
# install.packages("vars")
library(vars)
# install.packages("mFilter")
library(mFilter)
library(tseries)
library(ggplot2)
# install.packages("gridExtra")
library(gridExtra)
library(xts)
library(zoo)
# install.packages("pracma")
library(oce)
# install.packages("timeSeries")
library(pracma)
# install.packages("fGarch")
library(fGarch)
# install.packages("rugarch")
require(rugarch)
# install.packages("rmgarch")
library(rmgarch)
# install.packages("aTSA")
library(aTSA)
# install.packages("astsa")
library(astsa)
# install.packages("WeightedPortTest")
library(WeightedPortTest)
library(rmgarch)
# install.packages("lubridate")
# install.packages("stringi")
library(lubridate)
#install.packages("tidyverse")
library(scales);
library(tidyverse);
#install.packages("magrittr")
library(magrittr)
#install.packages("dplyr")
library(dplyr)
#install.packages("stochvol")
library(stochvol)
#devtools::install_github("ewenharrison/finalfit")
library(finalfit)
library(tidyr)
#install.packages("broom")
library(broom)
#install.packages("stargazer")
library(stargazer)
#install.packages("gdata")
library(gdata)
# install.packages("data.table")
library(data.table)
# install.packages("httr")
library(httr)
# install.packages("lubridate")
library(lubridate)
library(data.table)
library(plyr)

##########################   Data frames for White Maize ###############################
for(i in 1998:2009) {
  if(i==2000) next # 2000 has a slightly different format
  WhiteMaize_url <- paste0("http://www.sagis.org.za/Historic_-_SAFEX_(White_Maize)_", i, ".xls")
  GET(WhiteMaize_url, write_disk(tf <- tempfile(fileext= ".xls")))
  WhiteMaize <- read_excel(tf, range = "A5:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
  colnames(WhiteMaize)[colnames(WhiteMaize)==c("Datum/", "Gemiddelde daaglikse pryse/Average daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
  WhiteMaize <- WhiteMaize[!is.na(strptime(WhiteMaize$Date,format="%Y-%m-%d")),]
  WhiteMaize$Spot_price <- ifelse(is.na(WhiteMaize$Spot_price), WhiteMaize$Future_price, WhiteMaize$Spot_price)
  assign(paste0("WhiteMaize", i), data.frame(WhiteMaize))
  }

url1 <- "http://www.sagis.org.za/Historic_-_SAFEX_(White_Maize)_2000.xls"
GET(url1, write_disk(tf <- tempfile(fileext= ".xls")))
WhiteMaize2000 <- read_excel(tf, range = "A4:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
colnames(WhiteMaize2000)[colnames(WhiteMaize2000)==c("Datum/", "Gemiddelde daaglikse pryse/Average daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
WhiteMaize2000 <- WhiteMaize2000[!is.na(strptime(WhiteMaize2000$Date,format="%Y-%m-%d")),]
WhiteMaize2000$Spot_price <- ifelse(is.na(WhiteMaize2000$Spot_price), WhiteMaize2000$Future_price, WhiteMaize2000$Spot_price)


for(i in 2010:2013) {
  WhiteMaize_url <- paste0("http://www.sagis.org.za/Historic_-_SAFEX_(White_Maize)_", i, ".xls")
  GET(WhiteMaize_url, write_disk(tf <- tempfile(fileext= ".xls")))
  WhiteMaize <- read_excel(tf, range = "A4:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
  colnames(WhiteMaize)[colnames(WhiteMaize)==c("Datum/", "Gemiddelde daaglikse pryse/Average daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
  WhiteMaize <- WhiteMaize[!is.na(strptime(WhiteMaize$Date,format="%Y-%m-%d")),]
  WhiteMaize$Spot_price <- ifelse(is.na(WhiteMaize$Spot_price), WhiteMaize$Future_price, WhiteMaize$Spot_price)
  assign(paste0("WhiteMaize", i), data.frame(WhiteMaize))
}

for(i in 2014:2015) {
  WhiteMaize_url <- paste0("http://www.sagis.org.za/Historic_-_SAFEX_(White_Maize)_", i, ".xls")
  GET(WhiteMaize_url, write_disk(tf <- tempfile(fileext= ".xls")))
  WhiteMaize <- read_excel(tf, range = "A5:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
  colnames(WhiteMaize)[colnames(WhiteMaize)==c("Datum/", "Gemiddelde daaglikse pryse/Average daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
  WhiteMaize <- WhiteMaize[!is.na(strptime(WhiteMaize$Date,format="%Y-%m-%d")),]
  WhiteMaize$Spot_price <- ifelse(is.na(WhiteMaize$Spot_price), WhiteMaize$Future_price, WhiteMaize$Spot_price)
  assign(paste0("WhiteMaize", i), data.frame(WhiteMaize))
}

## The final years annoyingly don't keep the same URL format
url1 <- "http://www.sagis.org.za/Historic_-_SAFEX_(White_Maize)_2016_01.xls"
GET(url1, write_disk(tf <- tempfile(fileext= ".xls")))
WhiteMaize2016 <- read_excel(tf, range = "A5:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
colnames(WhiteMaize2016)[colnames(WhiteMaize2016)==c("Datum/", "Gemiddelde daaglikse pryse/Average daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
WhiteMaize2016 <- WhiteMaize2016[!is.na(strptime(WhiteMaize2016$Date,format="%Y-%m-%d")),]
WhiteMaize2016$Spot_price <- ifelse(is.na(WhiteMaize2016$Spot_price), WhiteMaize2016$Future_price, WhiteMaize2016$Spot_price)

url1 <- "http://www.sagis.org.za/Historic_-_SAFEX_(White_Maize)_201712_1.xls"
GET(url1, write_disk(tf <- tempfile(fileext= ".xls")))
WhiteMaize2017 <- read_excel(tf, range = "A5:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
colnames(WhiteMaize2017)[colnames(WhiteMaize2017)==c("Datum/", "Gemiddelde daaglikse pryse/Average daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
WhiteMaize2017 <- WhiteMaize2017[!is.na(strptime(WhiteMaize2017$Date,format="%Y-%m-%d")),]
WhiteMaize2017$Spot_price <- ifelse(is.na(WhiteMaize2017$Spot_price), WhiteMaize2017$Future_price, WhiteMaize2017$Spot_price)

## Creating a list of dataframes
rm(WhiteMaize)
lst <- mget(ls(pattern='^WhiteMaize\\d+'))

## Appending the dataframes together and deleting the future price column
WhiteMaize <- rbindlist(lst)
WhiteMaize$Future_price <- NULL

## Adjusting for inflation
CPI_url <- "https://github.com/Tyryn/Vol.Modeling/blob/master/pre2008%20cpi.xlsx?raw=true"
GET(CPI_url, write_disk(tf <- tempfile(fileext= ".xlsx")))
CPI <- read_excel(tf, range = "A1:B272", col_names = TRUE, col_types = c("date", "guess"))
cpiWhiteMaize <- merge(transform(WhiteMaize, Date = format(as.Date(Date), "%Y-%m")), 
      transform(CPI, Date = format(as.Date(Date), "%Y-%m")))
cpiWhiteMaize$Spot_price <- as.numeric(as.character(cpiWhiteMaize$Spot_price))
cpiWhiteMaize$adjusted <- cpiWhiteMaize$Spot_price*(100/cpiWhiteMaize$CPI)
cpiWhiteMaize <- cpiWhiteMaize[, -c(2:3)]
colnames(cpiWhiteMaize)

## Stick the adjusted spot prices into the original so as to retain date information
WhiteMaize$observation <- 1:nrow(WhiteMaize) 
cpiWhiteMaize$observation <- 1:nrow(cpiWhiteMaize)
cpiWhiteMaize <- merge(WhiteMaize, cpiWhiteMaize, by = "observation", all=T)
cpiWhiteMaize <- cpiWhiteMaize[, -c(1, 3, 4)] 
colnames(cpiWhiteMaize) <- c("Date", "WHITE_MAIZE")

## Getting rid of all dataframes except those needed
rm(list=setdiff(ls(), c("cpiWhiteMaize", "CPI")))


########################### Yellow Maize ############################################

for(i in 1998:2009) {
  # if(i==2000) next # 2000 has a slightly different format
  YellowMaize_url <- paste0("http://www.sagis.org.za/Historic_-_SAFEX_(Yellow_Maize)_", i, ".xls")
  GET(YellowMaize_url, write_disk(tf <- tempfile(fileext= ".xls")))
  YellowMaize <- read_excel(tf, range = "A5:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
  colnames(YellowMaize)[colnames(YellowMaize)==c("Datum/", "Gemiddelde daaglikse pryse/Average daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
  YellowMaize <- YellowMaize[!is.na(strptime(YellowMaize$Date,format="%Y-%m-%d")),]
  YellowMaize$Spot_price <- ifelse(is.na(YellowMaize$Spot_price), YellowMaize$Future_price, YellowMaize$Spot_price)
  assign(paste0("YellowMaize", i), data.frame(YellowMaize))
}

for(i in 2004:2006) {
  YellowMaize_url <- paste0("http://www.sagis.org.za/Historic_-_SAFEX_(Yellow_Maize)_", i, ".xls")
  GET(YellowMaize_url, write_disk(tf <- tempfile(fileext= ".xls")))
  YellowMaize <- read_excel(tf, range = "A6:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
  colnames(YellowMaize)[colnames(YellowMaize)==c("Datum/", "Average  daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
  YellowMaize <- YellowMaize[!is.na(strptime(YellowMaize$Date,format="%Y-%m-%d")),]
  YellowMaize$Spot_price <- ifelse(is.na(YellowMaize$Spot_price), YellowMaize$Future_price, YellowMaize$Spot_price)
  assign(paste0("YellowMaize", i), data.frame(YellowMaize))
}


for(i in 2010:2013) {
  YellowMaize_url <- paste0("http://www.sagis.org.za/Historic_-_SAFEX_(Yellow_Maize)_", i, ".xls")
  GET(YellowMaize_url, write_disk(tf <- tempfile(fileext= ".xls")))
  YellowMaize <- read_excel(tf, range = "A4:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
  colnames(YellowMaize)[colnames(YellowMaize)==c("Datum/", "Gemiddelde daaglikse pryse/Average daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
  YellowMaize <- YellowMaize[!is.na(strptime(YellowMaize$Date,format="%Y-%m-%d")),]
  YellowMaize$Spot_price <- ifelse(is.na(YellowMaize$Spot_price), YellowMaize$Future_price, YellowMaize$Spot_price)
  assign(paste0("YellowMaize", i), data.frame(YellowMaize))
}

for(i in 2014:2015) {
  YellowMaize_url <- paste0("http://www.sagis.org.za/Historic_-_SAFEX_(Yellow_Maize)_", i, ".xls")
  GET(YellowMaize_url, write_disk(tf <- tempfile(fileext= ".xls")))
  YellowMaize <- read_excel(tf, range = "A5:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
  colnames(YellowMaize)[colnames(YellowMaize)==c("Datum/", "Gemiddelde daaglikse pryse/Average daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
  YellowMaize <- YellowMaize[!is.na(strptime(YellowMaize$Date,format="%Y-%m-%d")),]
  YellowMaize$Spot_price <- ifelse(is.na(YellowMaize$Spot_price), YellowMaize$Future_price, YellowMaize$Spot_price)
  assign(paste0("YellowMaize", i), data.frame(YellowMaize))
}

## The final years annoyingly don't keep the same URL format
url1 <- "http://www.sagis.org.za/Historic_-_SAFEX_(Yellow_Maize)_2016_01.xls"
GET(url1, write_disk(tf <- tempfile(fileext= ".xls")))
YellowMaize2016 <- read_excel(tf, range = "A5:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
colnames(YellowMaize2016)[colnames(YellowMaize2016)==c("Datum/", "Gemiddelde daaglikse pryse/Average daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
YellowMaize2016 <- YellowMaize2016[!is.na(strptime(YellowMaize2016$Date,format="%Y-%m-%d")),]
YellowMaize2016$Spot_price <- ifelse(is.na(YellowMaize2016$Spot_price), YellowMaize2016$Future_price, YellowMaize2016$Spot_price)

url1 <- "http://www.sagis.org.za/Historic_-_SAFEX_(Yellow_Maize)_201712_1.xls"
GET(url1, write_disk(tf <- tempfile(fileext= ".xls")))
YellowMaize2017 <- read_excel(tf, range = "A5:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
colnames(YellowMaize2017)[colnames(YellowMaize2017)==c("Datum/", "Gemiddelde daaglikse pryse/Average daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
YellowMaize2017 <- YellowMaize2017[!is.na(strptime(YellowMaize2017$Date,format="%Y-%m-%d")),]
YellowMaize2017$Spot_price <- ifelse(is.na(YellowMaize2017$Spot_price), YellowMaize2017$Future_price, YellowMaize2017$Spot_price)

## Creating a list of dataframes
rm(YellowMaize)
lst <- mget(ls(pattern='^YellowMaize\\d+'))

## Appending the dataframes together and deleting the future price column
YellowMaize <- rbindlist(lst)
YellowMaize$Future_price <- NULL

## Adjusting for inflation
cpiYellowMaize <- merge(transform(YellowMaize, Date = format(as.Date(Date), "%Y-%m")), 
                       transform(CPI, Date = format(as.Date(Date), "%Y-%m")))
cpiYellowMaize$Spot_price <- as.numeric(as.character(cpiYellowMaize$Spot_price))
cpiYellowMaize$adjusted <- cpiYellowMaize$Spot_price*(100/cpiYellowMaize$CPI)
cpiYellowMaize <- cpiYellowMaize[, -c(2:3)]
colnames(cpiYellowMaize)

## Stick the adjusted spot prices into the original so as to retain date information
YellowMaize$observation <- 1:nrow(YellowMaize) 
cpiYellowMaize$observation <- 1:nrow(cpiYellowMaize)
cpiYellowMaize <- merge(YellowMaize, cpiYellowMaize, by = "observation", all=T)
cpiYellowMaize <- cpiYellowMaize[, -c(1, 3, 4)] 
colnames(cpiYellowMaize) <- c("Date", "YELLOW_MAIZE")

## Getting rid of all dataframes except those needed
rm(list=setdiff(ls(), c("cpiWhiteMaize", "cpiYellowMaize", "CPI")))

########################     Wheat       ###########################################


for(i in 1998:2006) {
  # if(i==2000) next # 2000 has a slightly different format
  Wheat_url <- paste0("http://www.sagis.org.za/Historic_-_SAFEX_(Wheat)_", i, ".XLS")
  GET(Wheat_url, write_disk(tf <- tempfile(fileext= ".xls")))
  Wheat <- read_excel(tf, range = "A5:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
  colnames(Wheat)[colnames(Wheat)==c("Datum/", "Gemiddelde daaglikse pryse/Average daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
  Wheat <- Wheat[!is.na(strptime(Wheat$Date,format="%Y-%m-%d")),]
  Wheat$Spot_price <- ifelse(is.na(Wheat$Spot_price), Wheat$Future_price, Wheat$Spot_price)
  assign(paste0("Wheat", i), data.frame(Wheat))
}



for(i in 2007:2009) {
  Wheat_url <- paste0("http://www.sagis.org.za/Historic_-_SAFEX_(Wheat)_", i, ".xls")
  GET(Wheat_url, write_disk(tf <- tempfile(fileext= ".xls")))
  Wheat <- read_excel(tf, range = "A5:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
  colnames(Wheat)[colnames(Wheat)==c("Datum/", "Gemiddelde daaglikse pryse/Average daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
  Wheat <- Wheat[!is.na(strptime(Wheat$Date,format="%Y-%m-%d")),]
  Wheat$Spot_price <- ifelse(is.na(Wheat$Spot_price), Wheat$Future_price, Wheat$Spot_price)
  assign(paste0("Wheat", i), data.frame(Wheat))
}



for(i in 2010:2013) {
  Wheat_url <- paste0("http://www.sagis.org.za/Historic_-_SAFEX_(Wheat)_", i, ".xls")
  GET(Wheat_url, write_disk(tf <- tempfile(fileext= ".xls")))
  Wheat <- read_excel(tf, range = "A4:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
  colnames(Wheat)[colnames(Wheat)==c("Datum/", "Gemiddelde daaglikse pryse/Average daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
  Wheat <- Wheat[!is.na(strptime(Wheat$Date,format="%Y-%m-%d")),]
  Wheat$Spot_price <- ifelse(is.na(Wheat$Spot_price), Wheat$Future_price, Wheat$Spot_price)
  assign(paste0("Wheat", i), data.frame(Wheat))
}

for(i in 2014:2015) {
  Wheat_url <- paste0("http://www.sagis.org.za/Historic_-_SAFEX_(Wheat)_", i, ".xls")
  GET(Wheat_url, write_disk(tf <- tempfile(fileext= ".xls")))
  Wheat <- read_excel(tf, range = "A5:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
  colnames(Wheat)[colnames(Wheat)==c("Datum/", "Gemiddelde daaglikse pryse/Average daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
  Wheat <- Wheat[!is.na(strptime(Wheat$Date,format="%Y-%m-%d")),]
  Wheat$Spot_price <- ifelse(is.na(Wheat$Spot_price), Wheat$Future_price, Wheat$Spot_price)
  assign(paste0("Wheat", i), data.frame(Wheat))
}

## The final years annoyingly don't keep the same URL format
url1 <- "http://www.sagis.org.za/Historic_-_SAFEX_(Wheat)_2016_01.xls"
GET(url1, write_disk(tf <- tempfile(fileext= ".xls")))
Wheat2016 <- read_excel(tf, range = "A5:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
colnames(Wheat2016)[colnames(Wheat2016)==c("Datum/", "Gemiddelde daaglikse pryse/Average daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
Wheat2016 <- Wheat2016[!is.na(strptime(Wheat2016$Date,format="%Y-%m-%d")),]
Wheat2016$Spot_price <- ifelse(is.na(Wheat2016$Spot_price), Wheat2016$Future_price, Wheat2016$Spot_price)

url1 <- "http://www.sagis.org.za/Historic_-_SAFEX_(Wheat)_201712_1.xls"
GET(url1, write_disk(tf <- tempfile(fileext= ".xls")))
Wheat2017 <- read_excel(tf, range = "A5:C300", col_names = TRUE, col_types = c("date", "guess", "guess"))
colnames(Wheat2017)[colnames(Wheat2017)==c("Datum/", "Gemiddelde daaglikse pryse/Average daily prices (R/t)", "X__1") ] <- c("Date", "Spot_price", "Future_price")
Wheat2017 <- Wheat2017[!is.na(strptime(Wheat2017$Date,format="%Y-%m-%d")),]
Wheat2017$Spot_price <- ifelse(is.na(Wheat2017$Spot_price), Wheat2017$Future_price, Wheat2017$Spot_price)

## Creating a list of dataframes
rm(Wheat)
lst <- mget(ls(pattern='^Wheat\\d+'))

## Appending the dataframes together and deleting the future price column
Wheat <- rbindlist(lst)
Wheat$Future_price <- NULL

## Adjusting for inflation
cpiWheat <- merge(transform(Wheat, Date = format(as.Date(Date), "%Y-%m")), 
                        transform(CPI, Date = format(as.Date(Date), "%Y-%m")))
cpiWheat$Spot_price <- as.numeric(as.character(cpiWheat$Spot_price))
cpiWheat$adjusted <- cpiWheat$Spot_price*(100/cpiWheat$CPI)
cpiWheat <- cpiWheat[, -c(2:3)]
colnames(cpiWheat)

## Stick the adjusted spot prices into the original so as to retain date information
Wheat$observation <- 1:nrow(Wheat) 
cpiWheat$observation <- 1:nrow(cpiWheat)
cpiWheat <- merge(Wheat, cpiWheat, by = "observation", all=T)
cpiWheat <- cpiWheat[, -c(1, 3, 4)] 
colnames(cpiWheat) <- c("Date", "WHEAT")

## Getting rid of all dataframes except those needed
rm(list=setdiff(ls(), c("cpiWhiteMaize", "cpiYellowMaize", "cpiWheat")))



##################################################################################

# Some cleaning and fixing
# cpiWhiteMaize$DATE <- as.Date(cpiWhiteMaize$DATE, format = "%Y-%m-%d")
# cpiYellowMaize$DATE <- as.Date(cpiYellowMaize$DATE, format = "%Y-%m-%d")
# cpiWheat$DATE <- as.Date(cpiWheat$DATE, format = "%Y-%m-%d")
# cpiSunSeeds$DATE <- as.Date(cpiSunSeeds$DATE, format = "%Y-%m-%d")
# cpiSoya$DATE <- as.Date(cpiSoya$DATE, format = "%Y-%m-%d")
# cpiSorghum$DATE <- as.Date(cpiSorghum$DATE, format = "%Y-%m-%d")

cpiWhiteMaize <- cpiWhiteMaize[-c(1737),] ##Missing information

##Summary of data
summary(cpiWhiteMaize)
str(cpiWhiteMaize)

summary(cpiYellowMaize)
str(cpiYellowMaize)

summary(cpiWheat)
str(cpiWheat)


#After looking at the box plots and the later time series plots, we find that
#there are a few suspicious looking data entries. These will be corrected for:







##############################################################################
#Interlude wher we show the monthly seasonality 
##############################################################################


##Showing relationship between month and price
cpiWhiteMaize$Month <- format(as.Date(cpiWhiteMaize$DATE), "%m")
cpiYellowMaize$Month <- format(as.Date(cpiYellowMaize$DATE), "%m")
cpiWheat$Month <- format(as.Date(cpiWheat$DATE), "%m")
cpiSunSeeds$Month <- format(as.Date(cpiSunSeeds$DATE), "%m")
cpiSoya$Month <- format(as.Date(cpiSoya$DATE), "%m")
cpiSorghum$Month <- format(as.Date(cpiSorghum$DATE), "%m")


regWhiteMaize <- lm(WHITE_MAIZE ~ Month, data = cpiWhiteMaize)
explanatory = "Month"
summary(regWhiteMaize)
regWhiteMaize <- tidy(regWhiteMaize)
regWhiteMaize
write.csv(regWhiteMaize, "Monthly effects - white maize")




regYellowMaize <- lm(YELLOW_MAIZE ~ Month, data =cpiYellowMaize)
summary(regYellowMaize)
regYellowMaize <- tidy(regYellowMaize)
write.csv(regYellowMaize, "Monthly effects - yellow maize")
regYellowMaize

regWheat <- lm(WHEAT ~ Month, data = cpiWheat)
summary(regWheat)
regWheat <- tidy(regWheat)
regWheat
write.csv(regWheat, "Monthly effects - white maize")




#The regression results show we can expect higher prices in the months prior
#and during harvest for the respective crops. Shows that there is a definite
#seasonal effect.

##############################################################################


##Creating time series
WhiteMaize <- ts(cpiWhiteMaize$"WHITE_MAIZE", 
                 start=c(1998, 01, 05), end=c(2017, 12, 29), frequency = 242)
YellowMaize <- ts(cpiYellowMaize$"YELLOW_MAIZE", 
                  start=c(1998, 01, 05), end=c(2017, 12, 29), frequency = 242)
Wheat<- ts(cpiWheat$"WHEAT", 
           start=c(1998, 01, 05), end=c(2017, 12, 29), frequency = 242)

na.interp(c(WhiteMaize, YellowMaize, Wheat))

#Displaying the time series


plot.ts(cbind(WhiteMaize, YellowMaize, Wheat), 
        cex.lab = 0.7, main = "Untransformed Spot Prices", ylab = c("White Maize", "Yellow Maize", "Wheat"))


#There appear to be sharp spikes and these will be dealt with by using the Hampel
#filter. I assume that the spikes are anomalies that shouldnt be part of the sample.
#Find source to say why this is useful.

##Hampel filter
despikeWhiteMaize <- hampel(WhiteMaize, k = 20, t0 = 3)
WhiteMaize <- despikeWhiteMaize$y
plot(WhiteMaize)

despikeYellowMaize <- hampel(YellowMaize, k = 20, t0 = 3)
YellowMaize <- despikeYellowMaize$y
plot(YellowMaize)

despikeWheat <- hampel(Wheat, k = 20, t0 = 3)
Wheat <- despikeWheat$y
plot(Wheat)


plot.ts(cbind(WhiteMaize, YellowMaize, Wheat), main = "Despiked Spot Prices", ylab = c("White Maize", "Yellow Maize", "Wheat"))





##############################################################################
#Decomposing the data into its components and removing the seasonal component
##############################################################################



##Decomposing into the seasonal
##Decompose White Maize
decWhiteMaize <- stl(WhiteMaize,"periodic")  # decompose the TS
plot(decWhiteMaize, main="White Maize Decomposed")
saWhiteMaize <- seasadj(decWhiteMaize)  # de-seasonalize
par(mfrow = c(2, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)  # plot two graphs side by side and squash margins

plot(saWhiteMaize, main="White Maize deseasonalized", sub="deseasonalized",
     xlab="Time", ylab="Price")
plot(WhiteMaize, main="White Maize",
     xlab="Time", ylab="Price")
WhiteMaize <- saWhiteMaize


##Decompose Yellow Maize
decYellowMaize <- stl(YellowMaize,"periodic")  # decompose the TS
plot(decYellowMaize,  main="Yellow Maize Decomposed")
saYellowMaize <- seasadj(decYellowMaize)  # de-seasonalize
par(mfrow = c(2, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)  # plot two graphs side by side and squash margins

plot(saYellowMaize, main="Yellow Maize deseasonalized", sub="deseasonalized",
     xlab="Time", ylab="Price")
plot(YellowMaize, main="Yellow Maize",
     xlab="Time", ylab="Price")
YellowMaize <- saYellowMaize


##Decompose Wheat
decWheat <- stl(Wheat,"periodic")  # decompose the TS
plot(decWheat, main="Wheat Decomposed")
saWheat <- seasadj(decWheat)  # de-seasonalize
par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)  # plot two graphs side by side and squash margins

plot(saWheat, main="Wheat deseasonalized", sub="deseasonalized",
     xlab="Time", ylab="Price")
plot(Wheat, main="Wheat",
     xlab="Time", ylab="Price")
Wheat <- saWheat

plot.ts(cbind(WhiteMaize, YellowMaize, Wheat), cex.lab = 0.7, main = "Transformed Spot Prices", ylab = c("White Maize", "Yellow Maize", "Wheat"))









###############################################################################
#Testing the data for unit roots and nonstationarity
###############################################################################


##ACF plots
acWhiteMaize <- ac(WhiteMaize, main = "White Maize" )
acYellowMaize <- ac(YellowMaize, main = "Yellow Maize" )
acWheat<- ac(Wheat, main = "Wheat")




#Testing for non-stationarity using the Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
kpss.test(WhiteMaize)
kpss.test(YellowMaize)
kpss.test(Wheat)

#Tests suggest non-stationarity


##Testing for unit rootusing the Augmented Dickey-Fuller test

gts_ur(WhiteMaize)
gts_ur(YellowMaize) 
gts_ur(Wheat) 

#Results strongly suggest that we have unit root data

###############################################################################
##First differencing to remove the unit root
###############################################################################


#White Maize
diff.WhiteMaize=diff(WhiteMaize)

diff.WhiteMaize


#Yellow Maize
diff.YellowMaize=diff(YellowMaize)

#Wheat
diff.Wheat=diff(Wheat)




par(mfrow = c(3, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.8)  # plot two graphs side by side and squash margins
plot(diff.WhiteMaize, xlab = "Time", ylab = "Spot Price", main = "Differenced White Maize")
plot(diff.YellowMaize, xlab = "Time", ylab = "Spot Price", main = "Differenced Yellow Maize")
plot(diff.Wheat, xlab = "Time", ylab = "Spot Price", main = "Differenced Wheat")


##ADF test 
gts_ur(diff.WhiteMaize)
gts_ur(diff.YellowMaize)
gts_ur(diff.Wheat)




##KPSS test to further test stationarity after first differencing 
kpss.test(diff.WhiteMaize)
kpss.test(diff.YellowMaize)
kpss.test(diff.Wheat)

#Tests suggest stationarity after first differencing

##ACF plots
ac(diff.WhiteMaize)
ac(diff.YellowMaize)
ac(diff.Wheat)


#We find that after we have first differenced, the data is stationary.
#This suggests that all crop spot prices are integrated to the order 1.
#We should expect to find this applying Box Jenkins

###Displaying differenced data

plot.ts(cbind(diff.WhiteMaize, diff.YellowMaize, diff.Wheat), cex.lab = 0.7, main = "Differenced Grain Prices")


###############################################################################
##Application of Box Jenkins for ARIMA
##############################################################################

# White Maize

# #For ARIMA(p,d,q), d has been found to be 1 in the previous step
# #No need for Ljung Box because its used for the identification process
# #White Maize
#auto.arima(diff.WhiteMaize, trace = TRUE)

# #The results suggest that an ARMA(1, 0, 1) with zero mean would be most appropriate.
# #The orginal paper suggested an ARIMA(1, 1, 0). The results from this paper
# #put an ARIMA(1,1,0) as the 8th best model.
# 
armaWhiteMaize <- arima(diff.WhiteMaize, order = c(1, 0, 1))
# 
# #Yellow Maize

#auto.arima(diff.YellowMaize, trace = TRUE)
# #The results suggest that an ARIMA(1, 0, 1) would best fit the data.
# #The original paper suggested an ARIMA(1, 1, 2), which is the fourth
# #best fit in this data.
# 
armaYellowMaize <- arma(diff.YellowMaize, order = c(1, 0, 1))
# 
# #Wheat
#auto.arima(diff.Wheat, trace = TRUE )
# #The results suggest that an ARIMA(1, 0, 0) would best fit the data.
# #The original paper suggested an ARIMA(3, 1, 2).
# 
armaWheat <- arima(diff.Wheat, order = c(0, 0, 1))
#


##Finding the optimal ARIMA by finding the combination of p, d and q that minimizes
#the Akaike Information Criterion

##Checking the fit of the models using the Box-Ljung test

Box.test(resid(arimaWhiteMaize), lag=20, type="Ljung-Box")
Box.test(resid(arimaYellowMaize), lag=20, type="Ljung-Box")
Box.test(resid(arimaWheat), lag=20, type="Ljung-Box") 
ac(resid(arimaWhiteMaize)^2)

##The p-values show the models to have a good fit


##########Correlograms to show good fit
ac(resid(armaWhiteMaize), main = "White Maize ARMA Residuals")

ac(resid(armaYellowMaize), main = "Yellow Maize ARMA Residuals")

ac(resid(armaWheat), main = "Wheat ARMA Residuals")









############################################################################
#Beginning ARCH/GARCH analysis
############################################################################


# #https://medium.com/auquan/time-series-analysis-for-finance-arch-garch-models-822f87f1d755


## White Maize
## Differenced white maize has a ARMA(1, 1) process


plot(diff.WhiteMaize)
plot(diff.WhiteMaize^2)##Seems to have some degree of volatility clustering
ac(diff.WhiteMaize)  ##There is at least a bit of autocorrelation


plot(resid(armaWhiteMaize))
ac(resid(armaWhiteMaize)) 
ac(resid(armaWhiteMaize)^2, main = "White maize mean equation squared residuals")     


# We see a bit of evidence of serial correlation in the squared residuals, 
# which suggests conditional heteroscedasticity. 

##ARCH test
tidy(arch.test(armaWhiteMaize))
#The Portmanteau-Q test and LM test strongly suggest there to be serial correlation in 
#the residuals. 

##So we look at candidate GARCH models mean and variance equations


#GARCH(1,1)
garchWhiteMaize1 <- garchFit(diff.WhiteMaize ~ arma(1, 1) + garch(1, 1), data = diff.WhiteMaize)
summary(garchWhiteMaize1)    ##AIC of 10.60720 BIC of 10.61461


#GARCH(2,1)
garchWhiteMaize2 <- garchFit(diff.WhiteMaize ~ arma(1, 1) + garch(2, 1), data = diff.WhiteMaize)
summary(garchWhiteMaize2)    ##AIC of 10.60747 BIC of 10.61611

#GARCH(1,2)
garchWhiteMaize3 <- garchFit(diff.WhiteMaize ~ arma(1, 1) + garch(1, 2), data = diff.WhiteMaize)
summary(garchWhiteMaize3)    ##AIC of 10.60747 BIC of 10.61611

#GARCH(2,2)
garchWhiteMaize4 <- garchFit(diff.WhiteMaize ~ arma(1, 1) + garch(2, 2), data = diff.WhiteMaize)
summary(garchWhiteMaize4)    ##AIC of 10.56584 BIC of 10.57572


##P value strongly suggests that we have a good fit - the p-value means we fail
#to reject the null of no autocorrelation in the squared residuals

##GARCH(2,2) + ARIMA(1,1) as it has the best AIC and BIC of the models
##However, as forecasting important for this paper, want to look at parsimonious
#model, which the GARCH(1,1) is.

####Evaluating chosen model ARMA(1,1) +GARCH(1,1)
##The ar1 and ma1 also seem to be significant - all coefficients are significant.
##The standardized residual test is a bit problematic. The Jaques-Bera test shows
##rejects the null that the residuals are normally distributed. 
##The Box-jung statistics and ARCH LM tests suggests that the model fits well. 
## However, as these tests are targeted 
## for raw data rather than for a model dealing with conditional heteroscedasticity,
##use the Li-Mak test instead.
## So looking at the Li-Mak test:

Weighted.LM.test(garchWhiteMaize4@residuals, garchWhiteMaize4@h.t, lag = 20)


##Dealing with residuals that are non-Gaussian
#The Jarque-Bera test results suggest non-Guassian. Trying different distributions
#Unfortunately, other conditional distributions dont improve the fit. 


##Visually checking if the GARCH model fits the data well

ac(residuals(garchWhiteMaize4))  
ac(((residuals(garchWhiteMaize4, standardize = T))^2), main = "White maize GARCH squared residuals") ##Remarkably good fit
plot.ts(residuals(garchWhiteMaize4, standardize = T)) #Looks like white noise


plot.ts(cbind(residuals(armaWhiteMaize, standardize = T), residuals(garchWhiteMaize4, standardize = T)), ylab="GARCH and ARCH", cex.lab = 0.6, main = "White Maize Residuals")







#######################   Yellow Maize    #############################


## Differenced yellow maize has an ARMA(1, 1) process


plot(diff.YellowMaize) ##Seems to have some degree of volatility clustering, notably around the 2008 mark.
plot(diff.YellowMaize^2)
ac(diff.YellowMaize)  ##Model seems stationary



ac(resid(armaYellowMaize)^2, main = "Yellow maize mean equation squared residuals") 


# We see evidence of serial correlation in the squared residuals, 
# which suggests conditional heteroscedasticity. 

##ARCH test
tidy(arch.test(armaYellowMaize))
#The Portmanteau-Q test and LM test strongly suggest there to be serial correlation in 
#the residuals. 

##So we look at candidate GARCH models
#GARCH(1,1)

garchYellowMaize1 <- garchFit(diff.YellowMaize ~ arma(1, 1) + garch(1, 1), data = diff.YellowMaize)
summary(garchYellowMaize1)    ##AIC of 10.28670 BIC of 10.29411

#GARCH(2,1)
garchYellowMaize2 <- garchFit(diff.YellowMaize ~ arma(1, 1) + garch(2, 1), data = diff.YellowMaize, trace = F)
summary(garchYellowMaize2)    ##AIC of 10.28694 BIC of 10.29559

#GARCH(1,2)
garchYellowMaize3 <- garchFit(diff.YellowMaize ~ arma(1, 1) + garch(1, 2), data = diff.YellowMaize, trace = F)
summary(garchYellowMaize3)    ##AIC of 10.28695 BIC of 10.29559

#GARCH(2,2)
garchYellowMaize4 <- garchFit(diff.YellowMaize ~ arma(1, 1) + garch(2, 2), data = diff.YellowMaize, trace = F)
summary(garchYellowMaize4)    ##AIC of 10.28403 BIC of 10.29391

#GARCH(2,2) the best model
6



##Evaluating chosen model ARMA(1,1) GARCH(2,2)

# The Ljung-Box and LM Arch tests all suggest the model fits well. However, like white maize,
# the model doesnt suggest normally distributed residuals.
##Trying different distributions doesnt help.





Weighted.LM.test(garchYellowMaize4@residuals, garchYellowMaize4@h.t, lag = 20)
##Indicative of a good fit - the p-value means we fail to reject the null
#of no autocorrelation in the squared residuals 


##Visually checking if the GARCH model fits the data well

ac(residuals(garchYellowMaize4))  
ac(residuals(armaYellowMaize))
ac(((residuals(garchYellowMaize4, standardize = T))^2), main = "Yellow maize GARCH squared residuals") ##Remarkably good fit
plot.ts(residuals(garchYellowMaize4, standardize = T)^2) #Looks like white noise

plot.ts(cbind(residuals(armaYellowMaize, standardize = T), residuals(garchYellowMaize4, standardize = T)), ylab="GARCH and ARCH", cex.lab = 0.6, main = "Yellow Maize Residuals")




###############     Wheat   #############################


## Differenced wheat has an AR(1, 0) process


plot(diff.Wheat) ##Seems to have some degree of volatility clustering, notably around the 2008 mark.
plot(diff.Wheat^2)
ac(diff.Wheat^2)  ##Model seems stationary



armaWheat <- arima(diff.Wheat, order= c(0, 0, 1))
armaWheat ##Coefficient has a t-stat greater than 2 so it is significant
ac(resid(armaWheat)^2, main = "Wheat mean equation squared residuals") 


# We see evidence of serial correlation in the squared residuals, 
# which suggests conditional heteroscedasticity. 

##ARCH test
tidy(arch.test(armaWheat))
#The Portmanteau-Q test and LM test strongly suggest there to be serial correlation in 
#the residuals. 

##So we look at candidate GARCH models
#GARCH(1,1)
garchWheat1 <- garchFit(diff.Wheat ~ arma(0, 1) + garch(1, 1), data = diff.Wheat)
summary(garchWheat1)    ##AIC of 10.28213 BIC of 10.28856

#GARCH(2,1)
garchWheat2 <- garchFit(diff.Wheat ~ arma(0, 1) + garch(2, 1), data = diff.Wheat, trace = F)
summary(garchWheat2)    ##AIC of 10.28263 BIC of 10.29035

#GARCH(1,2)
garchWheat3 <- garchFit(diff.Wheat ~ arma(0, 1) + garch(1, 2), data = diff.Wheat, trace = F)
summary(garchWheat3)    ##AIC of 10.28104 BIC of 10.28876

#GARCH(2,2)
garchWheat4 <- garchFit(diff.Wheat ~ arma(0, 1) + garch(2, 2), data = diff.Wheat, trace = F)
summary(garchWheat4)    ##AIC of 10.28128 BIC of 10.29028


##WE FIND GARCH(1,2) TO BE BEST


##Evaluating chosen model ARMA(1,1) GARCH(1,2)
# The Ljung-Box and LM Arch tests all suggest the model fits well. However, like white maize,
# the model doesnt suggest normally distributed residuals.
Weighted.LM.test(garchWheat3@residuals, garchWheat3@h.t, lag = 20)
##Indicative of a good fit - the p-value means we fail to reject the null
#of no autocorrelation in the squared residuals 


##Visually checking if the GARCH model fits the data well

ac(residuals(garchWheat3))  
ac(((residuals(garchWheat3, standardize = T))^2), main = "Wheat GARCH squared residuals") ##Remarkably good fit
plot.ts(residuals(garchWheat3, standardize = T)) #Looks like white noise
plot.ts(residuals(armaWheat, standardize = T))

plot.ts(cbind(residuals(armaWheat, standardize = T), residuals(garchWheat3, standardize = T)), ylab="GARCH and ARCH", cex.lab = 0.6, main = "Wheat Residuals")




#############################################################################
##  Multivariate GARCH
#############################################################################

##  We expect there to be volatility covariance between white maize and yellow 
#   maize as they have the same seasons.  Wheat and the maizes should also
#   have some volatility covariance but it would be due to market effects rather
#   than seasonal reasons. 
#   We expect volatility spillovers/linkages, which means that the historical 
#   volatility of one of the grain's market not only affects its own market but
#   also that of the others.



##  Going to use Dynamic Conditional Correlation 
# First estimate the individual GARCH model which are then used to standardize
# the individual residuals. As a second step one then has to specify the 
# correlation dynamics of these standardised residuals.



##Going to have to go the long haul route of rugarch package


#####################
##Giving the time series the same dimensions
mgWhiteMaize <- window(diff.WhiteMaize, start = c(1996,03), end = c(2018, 07))
mgYellowMaize <- window(diff.YellowMaize, start = c(1996,03), end = c(2018, 07))
mgWheat<- window(diff.Wheat, start = c(1997,11), end = c(2018, 07))


#Converting numerics to dates
strWhiteMaize <- as.zoo(mgWhiteMaize)
strYellowMaize <- as.zoo(mgYellowMaize)
strWheat <- as.zoo(mgWheat)



xmgWhiteMaize <- as.xts(strWhiteMaize, order.by = date_decimal(index(strWhiteMaize)))
xmgYellowMaize <- as.xts(strYellowMaize, order.by = date_decimal(index(strYellowMaize)))
xmgWheat<- as.xts(strWheat, order.by = date_decimal(index(strWheat)))



##Creating correlation matrices



WMYM <- data.frame(xmgWhiteMaize,  xmgYellowMaize, xmgWheat)
str(diff.WhiteMaize)  
str(diff.YellowMaize)
str(diff.Wheat)


specWhiteMaize <- ugarchspec(mean.model = list(armaOrder = c(1,1), variance.model= list(garchOrder = c(2, 2))))
specYellowMaize <- ugarchspec(mean.model = list(armaOrder = c(1,1), variance.model= list(garchOrder = c(2, 2))))
specWheat <- ugarchspec(mean.model = list(armaOrder = c(0,1), variance.model= list(garchOrder = c(1, 2))))

grain.n = multispec(c(specWhiteMaize, specYellowMaize, specWheat))


multf1 = multifit(grain.n, WMYM)

spec1 = dccspec(uspec = grain.n, dccOrder = c(1, 1), distribution = 'mvnorm')


fit1 = dccfit(spec1, data = WMYM, fit.control = list(eval.se = TRUE), fit = multf1)
fit1

# Get the model based time varying covariance (arrays) and correlation matrices

cov_crops = rcov(fit1)  # extracts the covariance matrix
cor_crops = rcor(fit1)  # extracts the correlation matrix

## Taking a look at the dimension
dim(cor_crops) #   So 4878 3x3 correlation matrices

## Correlation matrix for the last day
cor_crops[,,dim(cor_crops)[3]]





par(mfrow=c(3,1))  # this creates a frame with 3 windows to be filled by plots
plot(as.xts(cor_crops[1,2,]),main="White Maize and Yellow Maize")
plot(as.xts(cor_crops[1,3,]),main="White Maize and Wheat")
plot(as.xts(cor_crops[2,3,]),main="Yellow Maize and Wheat")




###### Forecast
########################################################


dccf1 <- dccforecast(fit1, n.ahead = 10)
dccf1

Rf <- dccf1@mforecast$R    # use H for the covariance forecast
str(Rf)


##Correlation matrix forecasts

corf_WMYM <- Rf[[1]][1,2,]  # Correlation forecasts between yellow maize and white maize
corf_WMW <- Rf[[1]][1,3,]  # Correlation forecasts between white maize and wheat
corf_YMW <- Rf[[1]][2,3,]  # Correlation forecasts between yellow maize and wheat


par(mfrow=c(3,1))
c_WMYM <- c(tail(cor_crops[1,2,],20),rep(NA,10))  # gets the last 20 correlation observations

cf_WMYM <- c(rep(NA,20),corf_WMYM) # gets the 10 forecasts
plot(c_WMYM,type = "l",main="Correlation White Maize and Yellow Maize")
lines(cf_WMYM,type = "l", col = "orange")

c_WMW <- c(tail(cor_crops[1,3,],20),rep(NA,10))  # gets the last 20 correlation observations
cf_WMW <- c(rep(NA,20),corf_WMW) # gets the 10 forecasts
plot(c_WMW,type = "l",main="Correlation White Maize and Wheat")
lines(cf_WMW,type = "l", col = "orange")

c_YMW <- c(tail(cor_crops[2,3,],20),rep(NA,10))  # gets the last 20 correlation observations
cf_YMW <- c(rep(NA,20),corf_YMW) # gets the 10 forecasts
plot(c_YMW,type = "l",main="Correlation Yellow Maize and Wheat")
lines(cf_YMW,type = "l", col = "orange")



#########################################
##Comparison to asymmetric DCC-GARCH
#######################################

## Skewness to show asymmetry of volatility

skewness(xmgWhiteMaize)
skewness(xmgYellowMaize)
skewness(xmgWheat)

##

spec2 = dccspec(uspec = grain.n, model='aDCC', dccOrder = c(1, 1), distribution = 'mvnorm')
fit2 = dccfit(spec2, data = WMYM, fit.control = list(eval.se = TRUE), fit = multf1)


asym_cov_crops = rcov(fit2)  # extracts the covariance matrix
asym_cor_crops = rcor(fit2)  # extracts the correlation matrix

## Taking a look at the dimension
dim(asym_cor_crops) #   So 4878 3x3 correlation matrices

## Correlation matrix for the last day
asym_cor_crops[,,dim(asym_cor_crops)[3]]




par(mfrow=c(3,1))  # this creates a frame with 3 windows to be filled by plots
plot(as.xts(asym_cor_crops[1,2,]),main="White Maize and Yellow Maize")
plot(as.xts(asym_cor_crops[1,3,]),main="White Maize and Wheat")
plot(as.xts(asym_cor_crops[2,3,]),main="Yellow Maize and Wheat")



##Forecasts
#Can only do a one step ahead forecast

dccf2 <- dccforecast(fit2, n.ahead = 1)
dccf1

Rf2 <- dccf2@mforecast$R    # use H for the covariance forecast
str(Rf2)


##Correlation matrix forecasts

asym_corf_WMYM <- Rf2[[1]][1,2,]  # Correlation forecasts between yellow maize and white maize
asym_corf_WMW <- Rf2[[1]][1,3,]  # Correlation forecasts between white maize and wheat
asym_corf_YMW <- Rf2[[1]][2,3,]  # Correlation forecasts between yellow maize and wheat

asym_corf_WMYM
asym_corf_WMW
asym_corf_YMW

corf_WMYM
corf_WMW
corf_YMW







