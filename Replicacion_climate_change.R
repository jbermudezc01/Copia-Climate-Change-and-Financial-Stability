if(Sys.info()["sysname"]=='Windows') Sys.setlocale("LC_TIME","English")

rm(list = ls())
if (Sys.info()["sysname"]=='Windows')  setwd('C:/Users/jpber/OneDrive/Documents/Codigo_compartido_Melo/Climate_Change_and_Financial_Stability/Climate-Change-and-Financial-Stability')
if (Sys.info()["sysname"]!='Windows')  setwd('/Users/lumelo/archivos/Climate-Change-and-Financial-Stability/Github/Climate-Change-and-Financial-Stability')

cat("\014")

### Libraries ====

library(lubridate)
library(ggplot2)
library(xts)
library(tidyr)
library(timeDate)
library(zoo)
library(tempdisagg)
library(readxl)
library(tsbox)
library(quantmod)
library(timeSeries)
library(forecast)
library(nlme)
library(seasonal)   
library(openxlsx)
library(urca)
library(fable)
library(lmtest)
library(dplyr)
library(moments)
library(stargazer)
library(Hmisc)
library(scales)
library(vars)
library(smoots)
library(dynlm)
library(systemfit)
library(ks)
library(gridExtra)

#--- Carga de funciones ---#
source('Functions_Climate_change.r')

### Lectura de datos ======

# Se genera un vector con el nombre de los paises de los cuales se tiene datos de indice bursatil
countries <- c("Australia","Belgium", "Brazil", "Canada", "Chile", "Denmark", "Finland",
               "France", "Germany", "HongKong", "India", "Indonesia","Mexico","Netherlands","Norway","Poland","Russia",
               "SouthAfrica","SouthKorea", "Spain", "Sweden","Switzerland","Thailand","Turkey", 
               "UnitedKingdom","USA1","USA2") #<<<--- Lista de los paises analizados
Tipos.Desastres  <- c("Biological","Climatological","Geophysical","Hydrological","Meteorological")  #<<<--- Tipos de desastres considerados
no.rezagos.de.desatres <- 4  #<<<--- Numero de rezagos de los desastres <w> (i.e. t0, t1, ..., tw)

# Establecemos el directorio de los datos
Dir      = paste0(getwd(),'/Bases/') #Directorio de datos, se supone que el subdirectorio <Bases> existe

# Genera una base de datos de los indices en formato xts
xts_list     <- list() 
for (country in countries) {
  # generar el nombre del archivo csv, siguiendo el Directorio especificado, añadiendole /Stocks_ country.csv
  csv_file <- paste0(Dir,"Stocks_", country, ".csv")
  # genera un archivo csv para el país country
  csv      <- read.csv(csv_file, header = TRUE, sep = ";", quote = "\"", col.names = c("Date","Price", "Open", 
                                                                                       "High","Low","Vol.","Change%"))
  colnames <- names(csv)
  
  # Loop que corre por todas las columnas del archivo csv menos la primera (ya que es un dia), retirandole las
  # comas que podrían generar problemas para reconocerlo como formato número
  for (colname in colnames[2:length(colnames)]) {
    csv[, colname] <- as.numeric(gsub(",","",csv[, colname]))
  } ## Muestra warning() ya quehay una columna que contiene caracteres "M" 
  
  csv$Date <- as.Date(csv$Date, "%m/%d/%Y")
  
  #Generar la lista de los xts, solamente de la columna "Price", teniendo en cuenta los índices incluidos en "Date"
  xts_list[[country]] <- xts(csv$Price, csv$Date)
}

#Generar una base de datos que junte todos los indices bursatiles en formato xts.
base_test <- do.call(merge,xts_list)


# Generar un vector de fechas en los que solo se tiene valores para uno o dos mercados

min.dias.stock <- ceiling((length(countries)/2)) #<<<--- minimo numero de paises en donde debe haber datos para stocks

navalues = c()
for (i in 1:nrow(base_test)) {
  row <- base_test[i, ]
  if (sum(is.na(row))>=length(countries)- min.dias.stock){
    navalues <- c(navalues, index(row))}
} 

# Eliminar aquellos dias que hacen parte del vector navalues, dejando solo los dias en los que se tiene datos para al
# menos 3 mercados. Se utilizó la función weekdays() para comprobar que ningún dia fuese sábado o domingo.
for (day in navalues) 
  base_test <- subset(base_test, subset = index(base_test) != day, drop = TRUE)
if ((any(weekdays(index(base_test))=='Sunday' | weekdays(index(base_test))=='Saturday')) == TRUE) 
  warning("En la base de datos hay sabados o domingos")

# Interpolacion lineal de los datos faltantes
base <- na.approx(base_test[,1])
for(i in 2:length(countries)) 
  base <- merge(base,na.approx(base_test[,i]))

#Eliminar las filas en las que hay valores NA, en el caso de la base de datos sucede en la primera y ultima fila
base_precios <- base[complete.cases(base),]

#Genera la base de retornos. Se coloca [2:nrow(base_precios)] porque de no hacerlo toda la primera fila serian valores
#NA, por lo que se perdio un dato. El operador diff se realizo para toda la base de precios,pero el[2:nrow(base_precios)]
#lo que hace es solamente quitar la primera fila de NA
base_retornos <- 100*diff(log(base_precios))[2:nrow(base_precios),]

### Otra variable importante es la media de los promedios moviles, por lo cual se genera el promedio movil de cada
### retorno de orden 22, ya que hay aproximadamente 22 dias para cada mes, usando la funcion moving_average.

orden <- 22 #<<<---  Orden del promedio movil del indice global de largo plazo de los indices accionarios 
#mov_average_base <- moving_average(base_retornos,orden)
mov_average_base <- apply(base_retornos, MARGIN=2, FUN=rollmean, k=orden, align="right")

# Media de los anteriores promedios. 
mean_mov_average = apply(mov_average_base, MARGIN=1, FUN=mean)
mean_mov_average = xts(mean_mov_average, order.by=index(base_retornos)[-c(1:(orden-1))]) #-> XTS
colnames(mean_mov_average) = c("Mean_Returns_Moving_Averages") # Nombre de la variable

#Hay que tener en cuenta que la muestra que se utiliza en el paper no es la misma que la que se tiene en las bases,
#por lo que se reduce la base para los datos del febrero 08 2001 a diciembre 30 2019.
#La funcion muestra_paper va a seleccionar desde un cierto dia, el cual elegimos 08 febrero 2001
#siguiendo el paper.

dia.inicial <- "2001-02-08"   #<<<--- Dia inicial de la muestra

# Se obliga que la muestra comience en <dia.inicial>
Retornos       = base_retornos[paste0(dia.inicial,"/"),]
Promedio_movil = mean_mov_average[paste0(dia.inicial,"/")]

### Table 1 de Pagnottoni: Estadistica descriptivas ===============
## Generar (skewness, kurtosis, mean, max, min, sd) de los retornos de los indices acc. 
skewness <- moments::skewness(Retornos)
kurtosis <- moments::kurtosis(Retornos)
mean     <- apply(Retornos, MARGIN=2, FUN=mean)
max      <- apply(Retornos, MARGIN=2, FUN=max)
min      <- apply(Retornos, MARGIN=2, FUN=min)
sd       <- apply(Retornos, MARGIN=2, FUN=sd)

Stats = cbind(min,max,mean,sd,skewness,kurtosis )
print(Stats, digits=3)

### ------------------------------------------   Desagregacion temporal ---------------------------------------------
# Las variables a desagregar diariamente son el crecimiento del GDP trimestral y el crecimiento del FDI anual.

### Datos para GDP trimestral ====
if(1){
  #Leer la base de datos, establecer el formato fecha y generar la base de datos en xts y la lista a ser desagregada
  gdp_countries      <- read_xlsx(paste0(Dir,"GDP_countries_corregida.xlsx"), sheet="GDP") #<<<--- Base de datos con GDPs
  dates.low.freq     <- as.Date(as.yearqtr(gdp_countries$Time),frac=1) #date format, se supone que existe una col llamada <Time>
  quarterly_series   <- as.list(gdp_countries[,-1]) #Se quita la columna de fechas y se genera una lista de sus columnas
  
  ### ----- Matriz de agregacion GDP trimestral <qtr_agr> ------
  # El metodo de chow-lin requiere una matriz de agregacion. Sin embargo, en la version fast se tiene en cuenta las 
  # diferencias de dias que puede haber en cada mes. Por tanto, es necesario crear una matriz de agregacion que 
  # tenga lo anterior en cuenta. 
  
  nrows <- length(quarterly_series[[1]])   ## Hay 76 trimestres en la base de datos, numero de datos trimestrales
  ncols <- nrow(base_precios)   ## No. de dias, con los retornos se pierde un dato, al tomarle diferencias se pierde otro dato
  
  qtr_agr <- matrix(0, nrow = nrows, ncol = ncols) #matriz de agregacion 
  dates.high.freq   <- as.character(index(base_precios)) #Fechas de freq alta
  cat('\nHigh frequency range:',range(dates.high.freq))
  cat('\nLow frequency range:'); print(range(dates.low.freq))
  
  ## WARNING si los rangos son distintos, primero pasar dates.high.freq al ultimo dia de su trimestre
  quarter.high.freq <- ceiling_date(as.Date(dates.high.freq), unit = "quarter") - 1
  cat('\nHigh frequency range in trimesters:');print(range(quarter.high.freq))
  
  if ((range(quarter.high.freq)[1] == range(dates.low.freq)[1] & range(quarter.high.freq)[2] == range(dates.low.freq)[2]) == FALSE)
    warning("Los rangos de baja y alta frecuencia son distintos")
  
  ## Extrae los meses de alta freq. en formato yyyy-mm sin repeticiones.
  meses <- unique(substr(dates.high.freq,1,7))
  
  #Realizamos la matriz de agregacion usando la funcion days. En este caso los enteros i iran de 0 a 75, 
  # y de acuerdo con la función esto generara el primer a tercer mes de cada trimestre. (Explicar mejor)
  for(i in 0:(nrows-1))
    qtr_agr <- days(x=i, m=qtr_agr, months=meses, dates=dates.high.freq)
  
  ### Definicion de parametros usados en Chow Lin. 
  # <alpha>:            Coef. de persitencia del error del modelo  
  # <matriz_var_cov_0>: La matriz de var-cov del error del modelo
  
  vec_cte <- c(rep(1, ncols)) ## vector constante usado como variable indicadora, dado que no tenemos otra
  alpha_fast = 0.99999        #<<---- Parametro de la version fast de chow-Lin
  
  #generar la matriz de var-cov de acuerdo al paper analizado (falta multiplicarla por sigma^2/(1-alpha^2) )
  matriz_var_cov_0 <- matrix(1, nrow=ncols, ncol=ncols)
  for(i in 1:ncols){
    for(j in 1:ncols)
      if(j != i) matriz_var_cov_0[i, j] = alpha_fast^(abs(j-i))
  } ##Crear la matriz de var-cov
  
  ## SE ejecuta la funcion de chow_lin, que da como resultado una base de datos de las series desagregadas.
  daily_series    = chow_lin(time_Series_list=quarterly_series, c=qtr_agr, w=vec_cte, var_covar=matriz_var_cov_0,base_indice = base_precios)
  gdp_growth_base = apply(daily_series, MARGIN=2, function(x) diff(log(x))) #diff(log(series))
  gdp_growth_base = as.xts(gdp_growth_base, order.by=index(base_precios[-1,]))
  
  #Colocamos los nombres de las series
  names                     <- colnames(base_retornos)
  colnames(gdp_growth_base) <- names[1:ncol(gdp_growth_base)]
  #Para diferenciar los nombres de los retornos le agregamos un prefijo gdp
  colnames(gdp_growth_base) <- paste("gdp",colnames(gdp_growth_base),sep="_")
}

### DESAGREGACION TEMPORAL FDI - Datos ===============================================================================
if(1){
  #Ahora para hacer la desagregacion temporal del FDI necesitaremos los mismos cinco argumentos: una lista de las 
  # series a desagregar, un vector constante, una matriz de agregación y una matriz de varianzas covarianzas-
  # el alpha puede seguir siendo el mismo, ya que para el metodo fast debe ser 0.99999.
  fdi_countries    = read_xlsx(paste0(Dir,"FDI_anual.xlsx"), sheet="FDI") #<<--- Base datos de los FDI
  fdi_countries_ts = as.ts(fdi_countries[,-1],start=2001,frequency=1)     #<<--- Fecha y freq inicial de los datos 
  fdi_series       = as.list(fdi_countries_ts)
  
  ##Matriz de agregacion anual FDI 
  fdi_rows <- nrow(fdi_countries_ts)
  ##El numero de columnas es igual al de gdp porque se quiere desagregar en esa cantidad de dias (ncols)
  fdi_agregacion_matriz  <- matrix(0, nrow = fdi_rows, ncol = ncols)
  
  #Se genera la matriz de agregacion, colocando uno a los dias que pertenezcan al año correspondiente.
  #Por ejemplo en la primera fila tendran uno aquellos dias que pertenezcan al 2001 (primer anho)
  i1 = 0
  for(i in as.numeric(unique(substr(dates.high.freq,1,4)))){
    i1 = i1 + 1
    for(date in dates.high.freq){
      if(substr(date,1,4)==i){
        pos <- which(dates.high.freq == date)
        fdi_agregacion_matriz[i1, pos] <- 1
      }
    }
  }
  
  # <vec_cte> corresponde a la serie indicadora, al igual que la matriz de var-cov permanecen iguales a las usadas en los GDPs
  fdi_daily_series = chow_lin(fdi_series, fdi_agregacion_matriz, vec_cte, matriz_var_cov_0,base_indice = base_precios)
  fdi_growth_base  = apply(fdi_daily_series, MARGIN=2, function(x) diff(log(x))) #diff(log(series))
  fdi_growth_base  = as.xts(fdi_growth_base, order.by=index(base_precios[-1,]))
  
  #Colocamos los mismos nombres que la base de retornos pero le agregamos un prefijo fdi
  colnames(fdi_growth_base) <- names[1:ncol(fdi_growth_base)]
  colnames(fdi_growth_base) <- paste("fdi",colnames(fdi_growth_base),sep="_")
}

#Por otro lado, tambien es necesario reducir la muestra a las bases para que concuerden con la muestra de paper
#Podemos usar la funcion que estaba anteriormente especificada
Crecimiento_PIB <- gdp_growth_base[paste0(dia.inicial,"/"),]
Crecimiento_FDI <- fdi_growth_base[paste0(dia.inicial,"/"),]

### Dummies corregidas =====

# Corremos la función <create_dummies> sobre el archivo que contiene las fechas de las dummies
dummies <- create_dummies(excel_file=paste0(Dir,"emdata_dummies_arregladas.xlsx"), 
                          Retornos, no.rezagos=no.rezagos.de.desatres, first.calendar.days.tobe.evaluated = 10 ) 

# Calculo de interacciones entre D y Rmt
names.int    = paste0('Int_D_', dimnames(dummies)[[1]])
interactions = matrix( NA, nrow(Retornos), length(dimnames(dummies)[[1]]), dimnames=list(as.character(index(Retornos)), names.int))
for (tip.desast in 1:ncol(interactions))
  interactions[,tip.desast] =  as.numeric(Promedio_movil) * dummies[tip.desast,,'D']

### Version de JP de las Dummies (no dio buenos resultados) =====
if(0){
  dummies <- create_dummies_xts(paste0(Dir,"EMDATA_dummies.xlsx"))
  
  climatological_dummies <- xts(dummies$`Climatological_dummies_xts}`,order.by = index(Retornos))
  meteorological_dummies <- xts(dummies$`Meteorological_dummies_xts}`,order.by = index(Retornos))
  hydrological_dummies <- xts(dummies$`Hydrological_dummies_xts}`,order.by = index(Retornos))
  geophysical_dummies <- xts(dummies$`Geophysical_dummies_xts}`,order.by = index(Retornos))
  biological_dummies <- xts(dummies$`Biological_dummies_xts}`, order.by = index(Retornos))
  
  colnames(biological_dummies) <- paste("biological",colnames(biological_dummies),sep="_")
  colnames(meteorological_dummies) <- paste("meteorological",colnames(meteorological_dummies),sep="_")
  colnames(hydrological_dummies) <- paste("hydrological",colnames(hydrological_dummies),sep="_")
  colnames(geophysical_dummies) <- paste("geophysical",colnames(geophysical_dummies),sep="_")
  colnames(climatological_dummies) <- paste("climatological",colnames(climatological_dummies),sep="_")
  
  interaction_climatological <- interaction_function(climatological_dummies)
  interaction_meteorological <- interaction_function(meteorological_dummies)
  interaction_hydrological <- interaction_function(hydrological_dummies)
  interaction_geophysical <- interaction_function(geophysical_dummies)
  interaction_biological <- interaction_function(biological_dummies) ### hacer for
}

### Generacion de base de datos con todas las variables que serán usadas para la estimación ====
base_datos <- merge(Retornos,Promedio_movil, as.xts(interactions[,paste0('Int_D_',Tipos.Desastres)],order.by= index(Retornos)))
for (desas in 1:length(Tipos.Desastres)){
  dummies.desas           = as.xts(dummies[desas,,paste0('t',0:no.rezagos.de.desatres)], order.by= index(Retornos))
  colnames(dummies.desas) = paste0(Tipos.Desastres[desas],'_',colnames(dummies.desas))
  base_datos              = merge(base_datos, dummies.desas)
}
base_datos <- merge(base_datos, Crecimiento_PIB, Crecimiento_FDI)

## Tambien es posible exportar a excel, en cuyo caso se sigue este codigo:
if(0){
  Date <- as.character(index(Retornos))
  # Cambiar a dataframe
  base_df <- as.data.frame(base_datos)
  #Agregar el indice, no se habia agregado antes porque la funcion merge ponia problema dada la clase de los objetos
  base_final <- cbind(Date,base_df)
  #Para crear un archivo excel con la base de datos
  write.xlsx(base_final,"Base_datos_final.xlsx",row.names= FALSE)
}

##### Revisar autocorrelacion serial ============
#El siguiente codigo es para revisar la autocorrelacion serial de la serie de retornos de cada indice, con 
#50, 100 y n/4 rezagos. Al 5% para todos los indices se viola la hipótesis nula para al menos un rezago
if(0){
  lags.test = round(nrow(base_retornos)/4)
  correlacionados <- c()
  no_correlacionados <- c()
  
  #Generamos un loop for, que crea dos vectores, en el primero incluye aquellos paises con un p-valor menor al 5%
  #para un test Ljung-Box con lags.test rezagos; es decir, incluye a los paises que se puede rechazar la no autocorrelacion
  #Mientras que en el segundo vector incluye a los paises que no tienen evidencia para rechazar la no autocorrelacion
  
  for (i in 1:ncol(base_retornos)) {
    result <- Box.test(base_retornos[, i], lag = lags.test, type = "Ljung-Box")
    if(result$p.value < 0.05){
      correlacionados <- c(correlacionados,colnames(base_retornos[,i]))
    }else{
      no_correlacionados <- c(no_correlacionados,colnames(base_retornos[,i]))
    }
  }
}


##### Agregar rezagos a las ecuaciones ===========

# Como se encontró correlacion serial para casi todas las series usando el test de Ljung - Box con 20 rezagos y n/4
# rezagos. Modelamos cada retorno siguiendo un modelo AR(p), siendo p = 0 a 20, y elegimos el modelo segun el 
# criterio de Akaike


## Loop para obtener las matrices de rezagos para cada pais
for(country in countries){
  var_name <- paste0("lags_",country)
  Lags     <- lag_function(base_retornos,country,AR.m=20, MA.m=0, d=0, bool=TRUE, metodo="CSS",dia.inicial)
  assign(var_name,Lags)
}


#### Estimacion del modelo SUR ==========

### Realizar for loop a lo largo de todos los paises para obtener las ecuaciones a estimar. 
# Tambien a lo largo de los 5 tipos de desastres

# El siguiente for genera una estimacion para cada uno de los 5 tipos de desastres, los cuales tomaran
# los nombres de fitdes_bio, fitdes_cli, fitdes_hyd, fitdes_geo, fitdes_met.
# Por otro lado, en la lista fitted_models generamos el nombre de losmodelos estimados, que necesitaremos 
# mas adelante.

eqsystem      = list()
fitted_models = c()
for(disaster in Tipos.Desastres){
  for(country in countries){
    var.exo                =  c( 'Mean_Returns_Moving_Averages', c(paste0('Int_D_', disaster), paste0(disaster,'_t', 0:no.rezagos.de.desatres)) )
    eqsystem[[country]]    =  model_equation.LF(database=base_datos, country, 
                                                var.exo=var.exo,  var.exo.pais=c('gdp','fdi'),  Lags='lags')
  }
  name          = paste0("fitdes_", substr(disaster,1,3) )
  fitted_models = c(fitted_models, name)
  assign(name, systemfit(eqsystem, method="SUR"))
} 



## ---------------------- SEGUNDA REGRESION, AHORA ES POR PAISES, NO POR TIPO DE DESASTRE. ---------- ##

##falta terminar
if(0){
  ## Regresion con dummies de paises ====
  
  dummies_countries <- create_dummies(excel_file=paste0(Dir,"emdata_dummies_countries.xlsx"),Retornos)  ## Genera un array de dimensiones 104, 4828, 6
  
  ## Generamos un vector de los nombres de los paises
  paises <- dimnames(dummies_countries)[[1]]
  
  # Calculo de interacciones entre D y Rmt
  names.countries.int    = paste0('Int_D_', dimnames(dummies_countries)[[1]])
  interactions.countries = matrix( NA, nrow(Retornos), length(dimnames(dummies_countries)[[1]]), dimnames=list(as.character(index(Retornos)), names.countries.int))
  for (tip.desast in 1:ncol(interactions.countries))
    interactions.countries[,tip.desast] =  as.numeric(Promedio_movil) * dummies_countries[tip.desast,,'D']
  
  ### Generacion de base de datos con todas las variables que serán usadas para la estimación ====
  base_datos <- merge(base_datos, as.xts(interactions.countries,order.by= index(Retornos)))
  # length(dimnames(dummies_countries)[[1]]) indica el total de paises que hay
  for (pais in 1:length(dimnames(dummies_countries)[[1]])){
    dummies.pais           = as.xts(dummies_countries[pais,,paste0('t',0:no.rezagos.de.desatres)], order.by= index(Retornos))
    colnames(dummies.pais) = paste0(paises[pais],'_',colnames(dummies.pais))
    base_datos             = merge(base_datos, dummies.pais)
  }
  
  ## Regresion con las nuevas dummies. Es importante resaltar que en este caso paises indica el pais en el que sucedio el desastre, mientras que 
  #  countries indica el pais donde esta el indice (Ejemplo: Brazil-Bovespa) 
  eqsystem2      = list()
  fitted_models2 = c()
  for(pais in paises){
    for(country in countries[1:2]){
      var.exo2                =  c('Mean_Returns_Moving_Averages', c(paste0('Int_D_', pais), paste0(pais,'_t', 0:no.rezagos.de.desatres)))
      eqsystem2[[country]]    =  model_equation.LF(database=base_datos, country, 
                                                  var.exo=var.exo2,  var.exo.pais=c('gdp','fdi'),  Lags='lags')
    }
    name2          = paste0("fitcoun_", substr(pais,1,11)) #revisar si solo poner pais en vez de substr(pais,1,11)
    fitted_models2 = c(fitted_models2, name2)
    assign(name2, systemfit(eqsystem2, method="SUR"))
  } 
} 
