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

##Real QGDP, how to rebase the data?
gdp_countries <- read.csv("C:/Users/jpber/OneDrive/Documents/BanRep/Replicacion/Bases/GDP_COUNTRIES.csv",
                          header = TRUE, sep = ";") 

dates <- as.Date(as.yearqtr(gdp_countries$Time))

gdp_australia_xts <- xts(gdp_countries["Australia"],order.by = dates)
gdp_australia_five <- gdp_australia_xts[1:20,]

##### Para 5 años

t1 <- system.time({ 
gdp_australia_daily <- td(gdp_australia_five ~ 1, conversion = "sum", to = "day", method = "fast") 
daily <- predict(gdp_australia_daily)
})

t2 <- system.time({
gdp_australia_denton <- td(gdp_australia_five ~ 1 , conversion = "sum", to = "day", method = "denton-cholette") 
daily_denton <- predict(gdp_australia_denton)
})

print(t1["elapsed"]) # 1.2 seg
print(t2["elapsed"]) # 22.13 seg

difference <- daily - daily_denton
plot(difference)

#### Para toda la muestra

t3 <- system.time({
gdp_australia_daily2 <- td(gdp_australia_xts ~ 1, conversion = "sum", to = "day", method = "fast")
daily2 <- predict(gdp_australia_daily2)
})

t4 <- system.time({
gdp_australia_denton2 <- td(gdp_australia_xts ~ 1 , conversion = "sum", to = "day", method = "denton-cholette")
daily_denton2 <- predict(gdp_australia_denton2)
})

print(t3["elapsed"]) # 16.49 seg
print(t4["elapsed"]) # 1064.94 seg (18 min)


difference2 <- daily2 - daily_denton2
plot(difference2)

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