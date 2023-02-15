rm(list = ls())

## Libraries
install.packages("tsbox")
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
               "France", "Germany", "HongKong", "India", "Indonesia","Mexico","Poland","Russia","SouthAfrica",
               "SouthKorea", "Spain", "Sweden","Switzerland","Thailand","Turkey", "UnitedKingdom","USA1","USA2")

## Loop through countries
for (country in countries) {
  ## Read CSV file
  csv_file <- paste0("C:/Users/jpber/OneDrive/Documents/BanRep/Replicacion/Bases/Indices_completos/Stocks_", country, ".csv")
  csv <- read.csv(csv_file)
  
  ## Convert date class
  csv$Date <- as.Date(csv$Date, "%m/%d/%Y")
  
  ## Convert to xts (time series)
  xts_file <- paste0(tolower(country), "_xts")
  assign(xts_file, xts(csv$Price, csv$Date))
}


base_test <- merge(australia_xts, belgium_xts,brazil_xts,canada_xts,chile_xts,denmark_xts,finland_xts,france_xts,
                   germany_xts,hongkong_xts,india_xts,indonesia_xts,mexico_xts,poland_xts,russia_xts,southafrica_xts,
                   southkorea_xts,spain_xts,sweden_xts,switzerland_xts,thailand_xts,turkey_xts,unitedkingdom_xts,
                   usa1_xts,usa2_xts)

weekends = c()
for (i in 1:nrow(base_test)) {
  row <- base_test[i, ]
  if (sum(is.na(row))>=23){
    weekends <- c(weekends, index(row))}
}


base_test2 <- base_test

for (day in weekends){
  base_test2 <- subset(base_test2, subset = index(base_test2) != day, drop = TRUE)
}

base <- na.locf(base_test$australia_xts)
for(i in 2:25){base <- merge(base,na.locf(base_test[,i]))}
#View(base)

base2 <- na.locf(base_test2$australia_xts)
for(i in 2:25){base2 <- merge(base2,na.locf(base_test2[,i]))}

base_precios <- base2[49:nrow(base2),] #To remove NA values

gdp <- read_excel("C:/Users/jpber/OneDrive/Documents/BanRep/Replicacion/Bases/GDP/GDP_AUSTRALIA.xlsx",sheet=1)

for(country in countries){}
gdp_australia <- td(gdp$Australia ~ 1, conversion = "sum", to = 365, method = "denton-cholette")


base_retornos <- diff(log(base2)) #Non numeric ?





