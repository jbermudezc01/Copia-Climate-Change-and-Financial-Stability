####Climate change and financial stability
#______________________________________________
## Lines 35-37: list of countries for which we wanted to replicate the paper
## Lines 41-55: loop that creates xts series for Price for each country
## Lines 58-61: large xts with several countries xts
## Line 63: duplicates the base just for comparative purposes
## Line 66 - 71: for loop that can be omitted, it is just an example showing which weekend days have na-values in all
## but one market, as we can see all of this days are 01st january.In India's stock markets, 01st january is not usually
## a holiday. Still, since it was only one country I removed the whole row, but I can interpolate the NA values if 
## necessary. The rest of the removed days are weekends.
## Line 72-77: creates a for loop that concatenate values where there are all but one or two NA's in the row to erase the 
## whole row. 
## Line 79: restricts the dates by erasing the dates obtained by for loop in line 124
## Line 82-84: interpolates the NA values for the base
## Line 86: restricts the dates so the resulting base has 0 NA's
## Line 88: creates the series of returns


rm(list = ls())



## Libraries
library(lubridate)
library(xts)
library(tidyr)
library(timeDate)
library(zoo)
library(tempdisagg)
library(readxl)
library(tsbox)

## List of countries
countries <- c("Australia","Belgium", "Brazil", "Canada", "Chile", "Denmark", "Finland",
               "France", "Germany", "HongKong", "India", "Indonesia","Mexico","Netherlands","Poland","Russia","SouthAfrica",
               "SouthKorea", "Spain", "Sweden","Switzerland","Thailand","Turkey", "UnitedKingdom","USA1","USA2")


## Loop through countries
for (country in countries) {
  ## Read CSV file
  csv_file <- paste0("C:/Users/jpber/OneDrive/Documents/BanRep/Replicacion/Bases/Indices_completos/Stocks_", country, ".csv")
  csv <- read.csv(csv_file, header = TRUE, sep = ";", quote = "\"", col.names = c("Date","Price", "Open", 
                                                                                  "High","Low","Vol.","Change%"))
  colnames <- names(csv)
  for (colname in colnames[2:length(colnames)]) {
    csv[, colname] <- as.numeric(gsub(",","",csv[, colname]))
  }
  ## Convert date class
  csv$Date <- as.Date(csv$Date, "%m/%d/%Y")
  ## Convert to xts (time series)
  xts_file <- paste0(tolower(country), "_xts")
  assign(xts_file, xts(csv$Price, csv$Date))
}


base_test <- merge(australia_xts, belgium_xts,brazil_xts,canada_xts,chile_xts,denmark_xts,finland_xts,france_xts,
                   germany_xts,hongkong_xts,india_xts,indonesia_xts,mexico_xts,netherlands_xts,poland_xts,russia_xts,
                   southafrica_xts, southkorea_xts,spain_xts,sweden_xts,switzerland_xts,thailand_xts,turkey_xts,
                   unitedkingdom_xts, usa1_xts,usa2_xts)

base_test2 <- base_test


for (i in 1:nrow(base_test)) {
  row <- base_test[i, ]
  if (sum(is.na(row))==length(countries)-1){
    if(weekdays(index(row)) != "sábado" & weekdays(index(row)) != "domingo") print(index(row))
  }
}

navalues = c()
for (i in 1:nrow(base_test)) {
  row <- base_test[i, ]
  if (sum(is.na(row))>=length(countries)-2){
    navalues <- c(navalues, index(row))}
} 

for (day in navalues) 
  base_test2 <- subset(base_test2, subset = index(base_test2) != day, drop = TRUE)
 
base <- na.approx(base_test2[,1])
for(i in 2:length(countries)) 
  base <- merge(base,na.approx(base_test2[,i]))

base_precios <- base[complete.cases(base),]

base_retornos <- diff(log(base_precios))[2:nrow(base_precios),]



### Count the days per year =============================================================

counts <- list()

for (year in 2001:2019) {
  count <- 0
  for (i in index(base_retornos)) {
    if (substr(format(as.Date(i), "%Y"), start = 1, stop = 4) == as.character(year)) {
      count <- count + 1
    }
  }
  counts[[as.character(year)]] <- count
}
