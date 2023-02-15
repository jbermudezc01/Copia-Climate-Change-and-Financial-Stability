#PRODUCTO INTERNO BRUTO#######

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
library(quantmod)
install.packages("bizdays")
library(timeSeries)

##Real QGDP, how to rebase the data?
gdp_countries <- read.csv("C:/Users/jpber/OneDrive/Documents/BanRep/Replicacion/Bases/GDP_COUNTRIES.csv",
                          header = TRUE, sep = ";") 

dates <- as.Date(as.yearqtr(gdp_countries$Time)) #date format 

gdp_australia_xts <- xts(gdp_countries["Australia"],order.by = dates) #xts
gdp_australia_five <- gdp_australia_xts[1:20,] #the first five years

##### Para 5 años

t1 <- system.time({ 
gdp_australia_daily <- td(gdp_australia_five ~ 1, conversion = "sum", to = "day", method = "fast") 
daily <- predict(gdp_australia_daily)
}) #time for "fast" method

t2 <- system.time({
gdp_australia_denton <- td(gdp_australia_five ~ 1 , conversion = "sum", to = "day", method = "denton-cholette") 
daily_denton <- predict(gdp_australia_denton)
}) #time for "denton-cholette" method

print(t1["elapsed"])  
print(t2["elapsed"]) 

plot(daily, main = "Daily GDP fast and denton-cholette",type = "l", col = "blue")
lines(daily_denton, col = "red")

difference <- daily - daily_denton
plot(difference)


#### Para toda la muestra

t3 <- system.time({
gdp_australia_daily2 <- td(gdp_australia_xts ~ 1, conversion = "sum", to = "day", method = "fast")
daily2 <- predict(gdp_australia_daily2)
}) #Time for fast method for the whole 19 years

t4 <- system.time({
gdp_australia_denton2 <- td(gdp_australia_xts ~ 1 , conversion = "sum", to = "day", method = "denton-cholette")
daily_denton2 <- predict(gdp_australia_denton2)
}) #Time for denton-cholette method for the whole 19 years

print(t3["elapsed"]) # 16.49 seg
print(t4["elapsed"]) # 1064.94 seg (18 min)

plot(daily2, main = "Daily GDP fast and denton-cholette",type = "l", col = "blue")
lines(daily_denton2, col = "red")


difference2 <- daily2 - daily_denton2
plot(difference2)

####====================================================================================

#Pasar de quarterly a daily segun el indice de la base de retornos de acciones

gdp_australia_daily2 <- td(gdp_australia_xts ~ 1, conversion = "sum", to = index(base_retornos), method = "fast")
##Error, length(to) debe ser 1, o sea un string o un numero

constant <- xts(rep(1,4893),order.by=index(base_retornos))
gdp_australia_daily3 <- td(gdp_australia_xts ~ 0 + constant, conversion = "sum", method = "fast")
## Error, right hand side contains NAs
sum(is.na(gdp_australia_xts))
sum(is.na(0+constant)) 
##ambos dan 0, no entiendo el error. segun elautor de la libreria, se puede omitir to si se tiene una indicadora

gdp_australia_daily3 <- td(gdp_australia_xts ~ 0 + base_retornos$australia_xts, conversion = "sum", method = "fast")
## Error

high_frequency <- xts(rep(1,260),order.by = index(base_retornos)[counts[["2001"]]+1:counts[["2002"]]])
gdp_second_year <- td(gdp_australia_xts[5:8,] ~ 0 + high_frequency , 
                      conversion = "sum",method = "fast")

###==================================================================================================

##Why not just use $values?
values <- gdp_australia_daily$values
identical(values,daily)

###=========================================

#Graficar

ts_plot(
  ts_scale(
    ts_c(daily2, gdp_australia_xts)
  ),
  title = "Daily disaggregated GDP",
  subtitle = "no indicator, fast method"
)

##sin scale

ts_plot(ts_c(daily2, gdp_australia_xts),
  title = "Daily disaggregated GDP",
  subtitle = "no indicator, fast method"
)

##ts_scale substracts the mean and divide by standard deviation to be able to compare them


###==================================================================================================


#Intento de hacer una xts grande
gdp_countries_xts <- xts(as.numeric(gdp_countries[,2:length(gdp_countries)]),
                         order.by = dates)

#Revisar  https://cran.r-project.org/web/packages/tempdisagg/vignettes/hf-disagg.html
#https://www.nesdc.go.th/nesdb_en/ewt_news.php?nid=4417&filename=national_account