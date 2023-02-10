
rm(list = ls())

## Libraries
install.packages("base")
library(lubridate)
library(xts)
library(tidyr)
library(timeDate)

library(zoo)

## Read CSV files
#aus_csv <- read.csv("C:/Users/jpber/OneDrive/Documents/BanRep/Replicacion/Bases/Indices_completos/Stocks_Australia.csv")
bel_csv <- read.csv("C:/Users/jpber/OneDrive/Documents/BanRep/Replicacion/Bases/Indices_completos/Stocks_Belgium.csv")
bra_csv <- read.csv("C:/Users/jpber/OneDrive/Documents/BanRep/Replicacion/Bases/Indices_completos/Stocks_Brazil.csv")
can_csv <- read.csv("C:/Users/jpber/OneDrive/Documents/BanRep/Replicacion/Bases/Indices_completos/Stocks_Canada.csv")
chl_csv <- read.csv("C:/Users/jpber/OneDrive/Documents/BanRep/Replicacion/Bases/Indices_completos/Stocks_Chile.csv")
dnk_csv <- read.csv("C:/Users/jpber/OneDrive/Documents/BanRep/Replicacion/Bases/Indices_completos/Stocks_Denmark.csv")
fin_csv <- read.csv("C:/Users/jpber/OneDrive/Documents/BanRep/Replicacion/Bases/Indices_completos/Stocks_Finland.csv")
fra_csv <- read.csv("C:/Users/jpber/OneDrive/Documents/BanRep/Replicacion/Bases/Indices_completos/Stocks_France.csv")
deu_csv <- read.csv("C:/Users/jpber/OneDrive/Documents/BanRep/Replicacion/Bases/Indices_completos/Stocks_Germany.csv")
hkg_csv <- read.csv("C:/Users/jpber/OneDrive/Documents/BanRep/Replicacion/Bases/Indices_completos/Stocks_HongKong.csv") 
ind_csv <- read.csv("C:/Users/jpber/OneDrive/Documents/BanRep/Replicacion/Bases/Indices_completos/Stocks_India.csv")
  
## Date class
#aus_csv$Date <- as.Date(aus_csv$Date, "%m/%d/%Y")
bel_csv$Date <- as.Date(bel_csv$Date, "%m/%d/%Y")
bra_csv$Date <- as.Date(bra_csv$Date, "%m/%d/%Y")
can_csv$Date <- as.Date(can_csv$Date, "%m/%d/%Y")
chl_csv$Date <- as.Date(chl_csv$Date, "%m/%d/%Y")
dnk_csv$Date <- as.Date(dnk_csv$Date, "%m/%d/%Y")
fin_csv$Date <- as.Date(fin_csv$Date, "%m/%d/%Y")
fra_csv$Date <- as.Date(fra_csv$Date, "%m/%d/%Y")
deu_csv$Date <- as.Date(deu_csv$Date, "%m/%d/%Y")
hkg_csv$Date <- as.Date(hkg_csv$Date, "%m/%d/%Y")
ind_csv$Date <- as.Date(ind_csv$Date, "%m/%d/%Y")


## Convert to xts (time series)
#aus_xts <- xts(aus_csv$Price, aus_csv$Date)
bel_xts <- xts(bel_csv$Price, bel_csv$Date)
bra_xts <- xts(bra_csv$Price, bra_csv$Date)
can_xts <- xts(can_csv$Price, can_csv$Date)
chl_xts <- xts(chl_csv$Price, chl_csv$Date)
dnk_xts <- xts(dnk_csv$Price, dnk_csv$Date)
fin_xts <- xts(fin_csv$Price, fin_csv$Date)
fra_xts <- xts(fra_csv$Price, fra_csv$Date)
deu_xts <- xts(deu_csv$Price, deu_csv$Date)
hkg_xts <- xts(hkg_csv$Price, hkg_csv$Date)
ind_xts <- xts(ind_csv$Price, ind_csv$Date)

#base_test <- merge(aus_xts,belgium_xts,brazil_xts,can_xts)
base_test <- merge(bel_xts,bra_xts,can_xts,chl_xts,dnk_xts,fin_xts,fra_xts,deu_xts,hkg_xts,ind_xts)

#base <- na.locf(base_test$aus_xts)
base <- na.locf(base_test$bel_xts)

for(i in 2:10){base <- merge(base,na.locf(base_test[,i]))}
View(base)


##To check the date index respect to a series of all the dates removing weekends

date_seq <- seq(min(bel_csv$Date), max(bel_csv$Date), by = 1)
date_seq <- date_seq[!(weekdays(as.Date(date_seq)) %in% c("sábado", "domingo"))]

dates_list <- as.list(index(base))  
dates_sequence <- as.list(date_seq)

x1 <- dates_list[!(dates_list %in% dates_sequence)] #Problem
x2 <- dates_sequence[!(dates_sequence %in% dates_list)] #Check if all are international holidays            
