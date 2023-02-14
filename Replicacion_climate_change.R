####Climate change and financial stability
#______________________________________________
## Lines 36-38: list of countries for which we wanted to replicate the paper
## Lines 42-56: loop that creates xts series for Price for each country
## Lines 58 and 74: if statements can be omitted
## Lines 105-108: large xts with several countries xts
## Line 110: duplicates the base just for comparative purposes
## Line 112: if statement can be omitted
## Line 120 - 125: for loop that can be omitted, it is just an example showing which weekend days have na-values in all
## but one market, as we can see all of this days are 01st january.In India's stock markets, 01st january is not usually
## a holiday. Still, since it was only one country I removed the whole row, but I can interpolate the NA values if 
## necessary. The rest of the removed days are weekends.
## Line 127-132: creates a for loop that concatenate values where there are all but one or two NA's in the row to erase the 
## whole row. 
## Line 134: restricts the dates by erasing the dates obtained by for loop in line 124
## Line 138-140: interpolates the NA values for the base
## Line 142: restricts the dates so the resulting base has 0 NA's
## Line 144: creates the series of returns


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

if(0){
xts_list <- list(australia_xts, belgium_xts,brazil_xts,canada_xts,chile_xts,denmark_xts,finland_xts,france_xts,
                 germany_xts,hongkong_xts,india_xts,indonesia_xts,mexico_xts,netherlands_xts,poland_xts,russia_xts,
                 southafrica_xts,southkorea_xts,spain_xts,sweden_xts,switzerland_xts,thailand_xts,turkey_xts,
                 unitedkingdom_xts,usa1_xts,usa2_xts)  

for (i in 1:length(xts_list)) {
  xts_obj <- xts_list[[i]]
  colnames(xts_obj)[1] <- "Price"
  df <- as.data.frame(xts_obj)
  df$Price <- as.numeric(df$Price)
  xts_obj <- as.xts(df)
  xts_list[[i]] <- xts_obj
}
}

if(0){

australia_xts <- xts_list[[1]]
belgium_xts <- xts_list[[2]]
brazil_xts <- xts_list[[3]]
canada_xts <- xts_list[[4]]
chile_xts <- xts_list[[5]]
denmark_xts <- xts_list[[6]]
finland_xts <- xts_list[[7]]
france_xts <- xts_list[[8]]
germany_xts <- xts_list[[9]]
hongkong_xts <- xts_list[[10]]
india_xts <- xts_list[[11]]
indonesia_xts <- xts_list[[12]]
mexico_xts <- xts_list[[13]]
netherlands_xts <- xts_list[[14]]
poland_xts <- xts_list[[15]]
russia_xts <- xts_list[[16]]
southafrica_xts <- xts_list[[17]]
southkorea_xts <- xts_list[[18]]
spain_xts <- xts_list[[19]]
sweden_xts <- xts_list[[20]]
switzerland_xts <- xts_list[[21]]
thailand_xts <- xts_list[[22]]
turkey_xts <- xts_list[[23]]
unitedkingdom_xts <- xts_list[[24]]
usa1_xts <- xts_list[[25]]
usa2_xts <- xts_list[[26]]
}


base_test <- merge(australia_xts, belgium_xts,brazil_xts,canada_xts,chile_xts,denmark_xts,finland_xts,france_xts,
                   germany_xts,hongkong_xts,india_xts,indonesia_xts,mexico_xts,netherlands_xts,poland_xts,russia_xts,
                   southafrica_xts, southkorea_xts,spain_xts,sweden_xts,switzerland_xts,thailand_xts,turkey_xts,
                   unitedkingdom_xts, usa1_xts,usa2_xts)

base_test2 <- base_test

if(0){
colnames(base_test) <- c("australia_xts","belgium_xts","brazil_xts","canada_xts","chile_xts","denmark_xts","finland_xts",
                         "france_xts", "germany_xts","hongkong_xts","india_xts","indonesia_xts","mexico_xts",
                         "netherlands_xts", "poland_xts","russia_xts","southafrica_xts2", "southkorea_xts","spain_xts",
                         "sweden_xts","switzerland_xts", "thailand_xts","turkey_xts", "unitedkingdom_xts", "usa1_xts",
                         "usa2_xts")
}

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
 
base <- na.approx(base_test2$australia_xts)
for(i in 2:length(countries)) 
  base <- merge(base,na.approx(base_test2[,i]))

base_precios <- base[complete.cases(base),]

base_retornos <- diff(log(base_precios))


#PRODUCTO INTERNO BRUTO#######

gdp <- read_excel("C:/Users/jpber/OneDrive/Documents/BanRep/Replicacion/Bases/GDP/GDP_AUSTRALIA.xlsx",sheet=1)
#Revisar que GDP usan, cual base si es real.

#Poner en tipo ts 
gdp <- ts(gdp,start=c(2001,1), end = c(2019,4), frequency=4)
gdp_xts <- ts_xts(gdp)

gdp_australia <- td(gdp_xts[,"Australia"] ~ 1 , conversion = "sum", to = "day", method = "denton-cholette") 
#como sabe que es trimestral
#averiguar que tipo de serie de tiempo , usar tsbox , ver ejemplos

gdp_first_year <- gdp_xts[1:4,2]
gdp_australia_first_year <- td(gdp_xts[,"Australia"] ~ 1 , conversion = "sum", to = "day", method = "fast")

#Revisar  https://cran.r-project.org/web/packages/tempdisagg/vignettes/hf-disagg.html
