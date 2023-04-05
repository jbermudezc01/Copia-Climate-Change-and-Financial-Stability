if(Sys.info()["sysname"]=='Windows') Sys.setlocale("LC_TIME","English")

rm(list = ls())
if (Sys.info()["sysname"]=='Windows')  setwd('C:/Users/jpber/OneDrive/Documents/Codigo_compartido_Melo/Climate_Change_and_Financial_Stability/Climate-Change-and-Financial-Stability')
if (Sys.info()["sysname"]!='Windows')  setwd('/Users/lumelo/archivos/Climate-Change-and-Financial-Stability/Github/Climate-Change-and-Financial-Stability')

cat("\014")

# Cargar librerias --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(xts)
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
#library(xlsx)
library(stringr)
library(maps)
library(mapproj)
library(ggthemes)
library(tmap)
library(sf)
library(ggsci)

# Cargar funciones --------------------------------------------------------

source('Functions_Climate_change.r')

# Lectura de datos --------------------------------------------------------

# Se genera un vector con el nombre de los paises de los cuales se tiene datos de indice bursatil

indexes   <- c("S.PASX200","BEL20","Bovespa","S.PTSXComposite","S.PCLXIPSA","OMXCopenhagen20","OMXHelsinki25","CAC40",
               "DAX","HangSeng","Nifty50","JakartaStockExchange","S.PBMVIPC","AEX","OSEBenchmark","WIG20","MOEXRussia",
               "SouthAfricaTop40","KOSPI","IBEX35","OMXStockholm30","SMI","SETIndex","BIST100","FTSE100","NASDAQComposite",
               "Nasdaq100") #<<<--- Lista de los indices analizados sin caracteres especiales por motivos de la regresion

countries <- c("Australia","Belgium", "Brazil", "Canada", "Chile", "Denmark", "Finland",
               "France", "Germany", "HongKong", "India", "Indonesia","Mexico","Netherlands","Norway","Poland","Russia",
               "SouthAfrica","SouthKorea", "Spain", "Sweden","Switzerland","Thailand","Turkey", 
               "UnitedKingdom","USA1","USA2") #<<<--- Lista de los paises de cada indice, con el proposito de leer los excel con los datos

Tipos.Desastres  <- c("Biological","Climatological","Geophysical","Hydrological","Meteorological")  #<<<--- Tipos de desastres considerados
no.rezagos.de.desatres <- 4  #<<<--- Numero de rezagos de los desastres <w> (i.e. t0, t1, ..., tw)

# Establecemos el directorio de los datos
Dir  = paste0(getwd(),'/Bases/') #Directorio de datos, se supone que el subdirectorio <Bases> existe

# Genera una lista de los codigos bursatiles en formato xts.
# La longitud de la lista es igual al numero de archivos con nombre stocks_<country> que existe en <Dir>.

# Para cada pais que se encuentra en <countries>, vamos a tener una base de datos del indice bursatil que le corresponde. Cada una tiene 7 columnas:
# la primera, <Date>, corresponde a los dias en los cuales tenemos datos para el indice; la segunda, <Price>, corresponde a los precios de cierre 
# para cada dia; la tercera, <Open>, corresponde a los precios de apertura para cada dia; la cuarta, <High>, corresponde al mayor precio registrado
# en el dia; la quinta, <Low>, corresponde al menor precio registrado en el dia; la sexta, <Vol.>, corresponde al volumen de acciones que se tranzaron;
# la septima, <Change%>, corresponde al cambio porcentual en el precio de cierre. 
# Dado que de las bases de datos nos interesan sobre todo <Date> y <Price>, al final generaremos una base que contenga todos los precios de cierre de
# todos los paises que esta en <countries>. Cabe recalcar que en el vector <indexes> tenemos los nombres de los indices bursatiles en el orden 
# correspondiente a los paises en <countries>.
xts_list     <- list() 
for (country in countries) {
  # Genera el nombre del archivo csv, siguiendo el Directorio especificado, añadiendole /Stocks_ country.csv
  csv_file <- paste0(Dir,"Stocks_", country, ".csv")
  # Genera un archivo csv para el país country
  csv      <- read.csv(csv_file, header = TRUE, sep = ";", quote = "\"", col.names = c("Date","Price", "Open", 
                                                                                       "High","Low","Vol.","Change%"))
  colnames <- names(csv)
  
  # Loop que corre por todas las columnas del archivo <csv> menos la primera (ya que es un dia), retirandole las
  # comas que podrían generar problemas para reconocerlo como formato número
  for (colname in colnames[2:length(colnames)]) {
    csv[, colname] <- as.numeric(gsub(",","",csv[, colname]))
  } # Muestra warning() ya que hay una columna que contiene caracteres "M" 
  
  csv$Date <- as.Date(csv$Date, "%m/%d/%Y")
  
  # Generar la lista de los xts, solamente de la columna "Price", teniendo en cuenta los índices incluidos en "Date"
  xts_list[[country]] <- xts(csv$Price, csv$Date)
}

# Generar una base de datos que junte todos los indices bursatiles en formato xts.
# Las matriz <base_test> tiene las siguientes dimensiones:
#     columnas: numero de <indices> analizados
#     filas   : total de dias en el que hay al menos un dato para cualquier <indice>
base_test <- do.call(merge,xts_list)
# Cambiar nombres de las columnas por los nombres de los indices
colnames(base_test) <- indexes

# Generar un vector de fechas en los que solo se tiene valores para n < <min.dias.stock> mercados.

min.dias.stock <- ceiling((length(indexes)/2)) #<<<--- minimo numero de <indices> en donde debe haber datos
navalues = c()
for (i in 1:nrow(base_test)) {
  row <- base_test[i, ]
  if (sum(is.na(row))>=length(indexes)- min.dias.stock){
    navalues <- c(navalues, index(row))}
} 

# Eliminar aquellos dias que hacen parte del vector <navalues>, dejando solo los dias en los que se tiene datos para al
# menos <min.dias.stock> mercados. Se utilizó la función weekdays() para comprobar que ningún dia fuese sábado o domingo.
for (day in navalues) 
  base_test <- subset(base_test, subset = index(base_test) != day, drop = TRUE)
if ((any(weekdays(index(base_test))=='Sunday' | weekdays(index(base_test))=='Saturday')) == TRUE) 
  warning("En la base de datos hay sabados o domingos")

# Interpolacion lineal de los datos faltantes. Las dimensiones de <base> son:
#       columnas: las mismas que <base_test>
#       filas   : las de <base_test> menos la longitud de <navalues>, que son las fechas eliminadas
base <- na.approx(base_test[,1])
for(i in 2:length(indexes)) 
  base <- merge(base,na.approx(base_test[,i]))
# Asegurarnos que quede con los nombres de columnas correctos
colnames(base) <- colnames(base_test)

# Eliminar las filas en las que hay valores NA. Las dimensiones de <base_precios> son:
#       columnas: las mismas que <base_test>
#       filas   : dependiendo de los valores en <base>, <base_precios> puede tener o el mismo numero de filas que <base> o una fila menos, 
#                 o dos filas menos.
base_precios <- base[complete.cases(base),]

# Genera la base de retornos. Se coloca [2:nrow(base_precios)] porque de no hacerlo toda la primera fila serian valores
# NA, por lo que se perdio un dato. El operador diff se realizo para toda la base de precios,pero el[2:nrow(base_precios)]
# lo que hace es solamente quitar la primera fila de NA.
# La dimension de <base_retornos> es la siguiente:
#       columnas: las mismas que <base_test> y las demás bases anteriores
#       filas   : una fila menos que <base-precios>
base_retornos <- 100*diff(log(base_precios))[2:nrow(base_precios),]

# Otra variable importante es la media de los promedios moviles, por lo cual se genera el promedio movil de cada
# retorno de orden 22, ya que hay aproximadamente 22 dias para cada mes, usando la funcion moving_average.

orden <- 22 #<<<---  Orden del promedio movil del indice global de largo plazo de los indices accionarios 
# mov_average_base <- moving_average(base_retornos,orden)
# La siguiente funcion apply genera una base de datos (<mov_average_base>). Las dimensiones de la matriz son:
#       columnas: las mismas que las demas bases anteriores
#       filas   : aquellas de <base_retornos> menos <orden>-1
mov_average_base <- apply(base_retornos, MARGIN=2, FUN=rollmean, k=orden, align="right")

# Media de los anteriores promedios. Genera vector con una longitud igual al numero de filas de <mov_average_base>
mean_mov_average = apply(mov_average_base, MARGIN=1, FUN=mean)
mean_mov_average = xts(mean_mov_average, order.by=index(base_retornos)[-c(1:(orden-1))]) #-> XTS
colnames(mean_mov_average) = c("Mean_Returns_Moving_Averages") # Nombre de la variable

# Hay que tener en cuenta que la muestra que se utiliza en el paper no es la misma que la que se tiene en las bases,
# por lo que se reduce la base para los datos del febrero 08 2001 a diciembre 30 2019.
# La funcion muestra_paper va a seleccionar desde un cierto dia, el cual elegimos 08 febrero 2001
# siguiendo el paper.
dia.inicial <- "2001-02-08"   #<<<--- Dia inicial de la muestra

# Se obliga que la muestra comience en <dia.inicial>
# La matriz <Retornos> conserva el mismo numero de columnas.
Retornos       = base_retornos[paste0(dia.inicial,"/"),]
Media_Promedio_movil = mean_mov_average[paste0(dia.inicial,"/")]

# Tabla 1 Pagnottoni: Estadistica descriptiva -----------------------------

## Generar (skewness, kurtosis, mean, max, min, sd) de los retornos de los <indices> acc. 
skewness <- moments::skewness(Retornos)
kurtosis <- moments::kurtosis(Retornos)
mean     <- apply(Retornos, MARGIN=2, FUN=mean)
max      <- apply(Retornos, MARGIN=2, FUN=max)
min      <- apply(Retornos, MARGIN=2, FUN=min)
sd       <- apply(Retornos, MARGIN=2, FUN=sd)

# La matriz <Stats> tiene por numero de columnas a aquellas medidas de estadística descriptiva, y las filas son igual al numero de <indices>
Stats = cbind(min,max,mean,sd,skewness,kurtosis )
print(Stats, digits=3)

# Desagregacion temporal --------------------------------------------------

# Las variables a desagregar diariamente son el crecimiento del GDP trimestral y el crecimiento del FDI anual.

## Datos para GDP trimestral ===
if(1){
  # La base de datos que se lee a continuacion tiene las siguientes columnas:
  #        <Time>      : indice trimestral del 2001 al 2019
  #        <countries> : El nombre de las demas columnas es el mismo que los paises que estan en <countries>
  # Por otro lado, a cada fila le corresponde una observacion trimestral del producto interno bruto.
  # Por tanto, si nos encontramos en la fila 2001Q1 y la columna "Australia", seria el PIB de Australia en el primer trimestre del 
  # 2001.
  
  # Leer la base de datos, establecer el formato fecha y generar la base de datos en xts y la lista a ser desagregada
  # De este modo se genera una lista <quarterly_series>, de longitud igual al numero de <indices>.
  # Cada elemento de <quarterly_series> es un vector numerico con la misma longitud de los datos en los archivos excel
  gdp_countries      <- read_xlsx(paste0(Dir,"GDP_countries_corregida.xlsx"), sheet="GDP") #<<<--- Base de datos con GDPs
  dates.low.freq     <- as.Date(as.yearqtr(gdp_countries$Time),frac=1) #date format, se supone que existe una col llamada <Time>
  quarterly_series   <- as.list(gdp_countries[,-1]) #Se quita la columna de fechas y se genera una lista de sus columnas
  
  ## -- Matriz de agregacion GDP trimestral <qtr_agr> ---
  # El metodo de chow-lin requiere una matriz de agregacion. Sin embargo, en la version fast se tiene en cuenta las 
  # diferencias de dias que puede haber en cada mes. Por tanto, es necesario crear una matriz de agregacion que 
  # tenga lo anterior en cuenta. 
  
  nrows <- length(quarterly_series[[1]])   ## Hay 76 trimestres en la base de datos, numero de datos trimestrales
  ncols <- nrow(base_precios)   ## No. de dias, con los retornos se pierde un dato, y al tomarle diferencias se pierde un dato, por lo que
  ## al final las matrices <Retornos> y la de desagregacion tendran el mismo numero de filas
  
  # La matriz de agregacion <qtr_agr> tiene entonces dimensiones <nrows>x<ncols>
  qtr_agr <- matrix(0, nrow = nrows, ncol = ncols) #matriz de agregacion 
  dates.high.freq   <- as.character(index(base_precios)) #Fechas de freq alta
  cat('\nHigh frequency range:',range(dates.high.freq))
  cat('\nLow frequency range:'); print(range(dates.low.freq))
  
  ## WARNING si los rangos son distintos, primero pasar <dates.high.freq> al ultimo dia de su trimestre
  quarter.high.freq <- ceiling_date(as.Date(dates.high.freq), unit = "quarter") - 1
  cat('\nHigh frequency range in trimesters:');print(range(quarter.high.freq))
  if ((range(quarter.high.freq)[1] == range(dates.low.freq)[1] & range(quarter.high.freq)[2] == range(dates.low.freq)[2]) == FALSE)
    warning("Los rangos de baja y alta frecuencia son distintos")
  
  ## Extrae los meses de alta freq. en formato yyyy-mm sin repeticiones.
  meses <- unique(substr(dates.high.freq,1,7))
  
  #Realizamos la matriz de agregacion usando la funcion days.
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
  
  # Se ejecuta la funcion de chow_lin, que da como resultado una base de datos de las series desagregadas.
  # La base <gdp_growth_base> tiene las siguientes dimensiones: columnas - el numero de <indices>, filas - el mismo de <base_retornos> 
  daily_series    = chow_lin(time_Series_list=quarterly_series, c=qtr_agr, w=vec_cte, var_covar=matriz_var_cov_0,base_indice = base_precios)
  gdp_growth_base = apply(daily_series, MARGIN=2, function(x) diff(log(x))) #diff(log(series))
  gdp_growth_base = as.xts(gdp_growth_base, order.by=index(base_precios[-1,]))
  
  #Colocamos los nombres de las series
  names                     <- (countries)
  colnames(gdp_growth_base) <- names[1:ncol(gdp_growth_base)]
  #Para diferenciar los nombres de los retornos le agregamos un prefijo gdp
  colnames(gdp_growth_base) <- paste("gdp",colnames(gdp_growth_base),sep="_")
}

### DESAGREGACION TEMPORAL FDI - Datos ===
if(1){
  # La base de datos que se lee a continuacion tiene las siguientes columnas:
  #       <Year>      : Año del cual tenemos datos
  #       <countries> : El nombre de las demas columnas es el mismo que los paises que estan en <countries>
  # A cada fila le corresponde una observacion anual  del indice de desarrollo financiero.
  # Por tanto, si nos encontramos en la fila 2001 y la columna "Australia", seria el FDI de Australia en el 2001.
  
  #Ahora para hacer la desagregacion temporal del FDI necesitaremos los mismos cinco argumentos: una lista de las 
  # series a desagregar, un vector constante, una matriz de agregación y una matriz de varianzas covarianzas-
  # el alpha puede seguir siendo el mismo, ya que para el metodo fast debe ser 0.99999.
  # De este modo se genera una lista <fdi_series>, de longitud igual al numero de <indices>.
  # Cada elemento de <fdi_series> es un vector numerico con la misma longitud de los datos en los archivos excel
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
  # La base <fdi_growth_base> tiene las siguientes dimensiones: columnas - el numero de <indices>, filas - el mismo de <base_retornos> 
  fdi_daily_series = chow_lin(fdi_series, fdi_agregacion_matriz, vec_cte, matriz_var_cov_0,base_indice = base_precios)
  fdi_growth_base  = apply(fdi_daily_series, MARGIN=2, function(x) diff(log(x))) #diff(log(series))
  fdi_growth_base  = as.xts(fdi_growth_base, order.by=index(base_precios[-1,]))
  
  # Colocamos los mismos nombres que la base de retornos pero le agregamos un prefijo fdi
  colnames(fdi_growth_base) <- names[1:ncol(fdi_growth_base)]
  colnames(fdi_growth_base) <- paste("fdi",colnames(fdi_growth_base),sep="_")
}

# Por otro lado, tambien es necesario reducir la muestra a las bases para que concuerden con la muestra de paper
# Podemos usar la funcion que estaba anteriormente especificada
# Ambas matrices (<Crecimiento_PIB> y <Crecimiento_FDI>) conservan el mismo numero de columnas, pero sus filas se ven reducidas a las mismas de <Retornos> 
Crecimiento_PIB <- gdp_growth_base[paste0(dia.inicial,"/"),]
Crecimiento_FDI <- fdi_growth_base[paste0(dia.inicial,"/"),]

# Dummies corregidas ------------------------------------------------------

# Corremos la función <create_dummies> sobre el archivo que contiene las fechas de las dummies
# El archivo excel al cual se hace referencia enseguida tiene 5 hojas: una por cada tipo de desastre (Biological, Climatological, Geophysical, Hydrological y 
# meteorological). En cada una de las hojas tenemos cuatro columnas:
#                  <t0>       : corresponde al dia en el que sucedio un desastre
#                  <na.start> : dummy que toma el valor de 1 si se supuso que el dia del evento fue el primer dia del mes y 0 en otro caso
#                  <end>      : corresponde al ultimo dia del desastre
#                  <na.end>   : dummy que toma el valor de 1 si se supuso que el ultimo dia del evento fue el ultimo dia del mes y 0 en otro caso
dummies <- create_dummies(excel_file=paste0(Dir,"emdata_dummies_arregladas.xlsx"), 
                          Retornos, no.rezagos=no.rezagos.de.desatres, first.calendar.days.tobe.evaluated = 10 ) 

# Calculo de interacciones entre D y Rmt
names.int    = paste0('Int_D_', dimnames(dummies)[[1]])
# <interactions> sera una matriz con el mismo numero de filas que <Retornos> y su numero de columnas es length(dimnames(<dummies>)[[1]])
interactions = matrix( NA, nrow(Retornos), length(dimnames(dummies)[[1]]), dimnames=list(as.character(index(Retornos)), names.int))
for (tip.desast in 1:ncol(interactions))
  interactions[,tip.desast] =  as.numeric(Media_Promedio_movil) * dummies[tip.desast,,'D']

### Version de JP de las Dummies (no dio buenos resultados) ===
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

# Base de datos con todas las variables usadas en la estimacion -----------

# El siguiente codigo junta las bases de datos principales en una sola. De este modo tenemos que <base_datos> tiene el mismo numero
# de filas que <Retornos>, y sus columnas son todas las series de indices bursatiles que se encuentran en <Retornos>, <Media_Promedio_movil>,
# todas las interacciones generadas en el codigo anterior, las 5 dummies por cada tipo de desastre, las serie de crecimiento de producto interno
# bruto y del indice de desarrollo financiero para todos los paises de <countries>
base_datos <- merge(Retornos,Media_Promedio_movil, as.xts(interactions[,paste0('Int_D_',Tipos.Desastres)],order.by= index(Retornos)))
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

# Revisar autocorrelacion serial ------------------------------------------

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

# Agregar rezagos a las ecuaciones ----------------------------------------

# Como se encontró correlacion serial para casi todas las series usando el test de Ljung - Box con 20 rezagos y n/4
# rezagos. Modelamos cada retorno siguiendo un modelo AR(p), siendo p = 0 a 20, y elegimos el modelo segun el 
# criterio de Akaike

## Loop para obtener las matrices de rezagos para cada indice
for(indice in indexes){
  var_name <- paste0("lags_",indice)
  Lags     <- lag_function(base_retornos,indice,AR.m=20, MA.m=0, d=0, bool=TRUE, metodo="CSS",dia.inicial)
  assign(var_name,Lags)
}

# Estimacion del modelo SUR -----------------------------------------------

### Realizar for loop a lo largo de todos los paises para obtener las ecuaciones a estimar. 
# Tambien a lo largo de los 5 tipos de desastres

# El siguiente for genera una estimacion para cada uno de los 5 tipos de desastres, los cuales tomaran
# los nombres de fitdes_bio, fitdes_cli, fitdes_hyd, fitdes_geo, fitdes_met.
# Por otro lado, en la lista fitted_models generamos el nombre de losmodelos estimados, que necesitaremos 
# mas adelante.

# if(0) dado que se utilizo el comando save para guardar los modelos. Los elementos guardados seran models_disasters_list y 
# resid_disasters_list, que incluyen por un lado los modelos estimados y por el otro los residuales.
# ----COLOCAR <if(1)> SI SE DESEA ESTIMAR EL MODELO ----#
load.SUR  = 1            #<<<<-- 1 si se carga el SUR inicial, 0 si se corre y salva el SUR inicial 
saved.day = "2023-04-05" #<<<--- fecha del save() en formato yyyy-mm-dd
if(!load.SUR){
  eqsystem              = list()
  fitted_models         = c()
  #models_disasters_list = list()
  coefficients_disasters_list = list()
  resid_disasters_list  = list() 
  for(disaster in Tipos.Desastres){
    for(i in 1:length(countries)){
      var.exo                =  c( 'Mean_Returns_Moving_Averages', c(paste0('Int_D_', disaster), paste0(disaster,'_t', 0:no.rezagos.de.desatres)) )
      eqsystem[[indexes[i]]]    =  model_equation.LF(database=base_datos, countries[i], indexes[i],
                                                     var.exo=var.exo,  var.exo.pais=c('gdp','fdi'),  Lags='lags')
    }
    name          = paste0("fitsur_", substr(disaster,1,3) )
    fitted_models = c(fitted_models, name)
    assign(name, systemfit(eqsystem, method="SUR"))
    coefficients_disasters_list[[name]]   <- summary(get(name))$coefficients
    resid_disasters_list[[name]]          <- resid(get(name))
    #models_disasters_list[[name]] <- get(name)
  } 
  # Guardar datos --------------------------------------------------------------#
  #--- Guardado de los modelos por tipo de desastre , mas la base de retornos---#
  saved.day = today() #<<<--- dia del <save>,  en formato yyyy-mm-dd
  # 1. En el objeto Resultados_Desastres_today() se guardan elementos claves para poder graficar, incluyendo
  # los resultados de las regresion SUR
  save(coefficients_disasters_list, resid_disasters_list, fitted_models, Retornos,
       file=paste0(paste0('Resultados_Desastres_',saved.day),'.RData')) 
} else load(paste0(paste0('Resultados_Desastres_',saved.day),'.RData')) #del save 1.


# Segunda regresion, por paises en vez de por tipo de desastre ------------

## Regresion con dummies de paises ===

# El archivo excel que se esta cargando a continuacion tiene 104 hojas, donde cada una hace referencia a un pais analizado por Pagnottoni.
# Cada hoja tiene 3 columnas que nos interesan:
#         <Country>  : el nombre del pais
#         <na_start> : dummy que toma el valor de 1 si se supuso que el dia del evento era el primero del mes y 0 en otro caso
#         <t0>       : dia del desastre

excel_countries   <- paste0(Dir,"emdata_dummies_countries.xlsx")     #<<<--- base de datos con las dummies de cada pais donde ocurrio un desastre
# La base de datos <excel_countries> tiene 2 columnas que interesan, <Country>, que indica el pais, y <t0> que indica el dia de los desastres en ese pais
# Sin embargo, con la funcion create_dummies se genera un array de dimensiones 104, 4828, 6; 104 hace referencia al numero de hojas del archivo
# excel (cada hoja representa un pais), 4828 es el mismo numero de filas que <Retornos> y 6 hace referencia a las 6 dummies (t0,...,t4,D)
dummies_countries <- create_dummies(excel_file=excel_countries,Retornos, no.rezagos=4,
                                    first.calendar.days.tobe.evaluated = 10)  ## Genera un array de dimensiones 104, 4828, 6

## Generamos un vector de los nombres de los paises
paises <- dimnames(dummies_countries)[[1]]

# Calculo de interacciones entre D y Rmt (Rmt:<Media_Promedio_movil>)
names.countries.int    = paste0('Int_D_', dimnames(dummies_countries)[[1]])
interactions.countries = matrix( NA, nrow(Retornos), length(dimnames(dummies_countries)[[1]]), dimnames=list(as.character(index(Retornos)), names.countries.int))
for (tip.desast in 1:ncol(interactions.countries))
  interactions.countries[,tip.desast] =  as.numeric(Media_Promedio_movil) * dummies_countries[tip.desast,,'D']


# Adicion de variables a la base de datos ---------------------------------

# En el siguiente codigo se agregan las interacciones por paises a la <base_datos>, y posteriormente se agregan las dummies por cada pais.
base_datos <- merge(base_datos, as.xts(interactions.countries,order.by= index(Retornos)))
for (pais in 1:length(paises)){
  dummies.pais           = as.xts(dummies_countries[pais,,paste0('t',0:no.rezagos.de.desatres)], order.by= index(Retornos))
  colnames(dummies.pais) = paste0(paises[pais],'_',colnames(dummies.pais))
  base_datos             = merge(base_datos, dummies.pais)
}

## REGRESION POR PAISES. Los coeficientes, errores estandar, t_Values, p_values y residuales de la estimacion fueron guardados usando el comando
#  save() con el fin de no tener que correr siempre esta estimacion, por lo cual se usa el if(0).
# ----COLOCAR <if(1)> SI SE DESEA ESTIMAR EL MODELO por paises ----#
load.SURpaises = 1        #<<<--- 1 si se carga el SUR paises, 0 si se corre y salva el SUR paises 
saved.day = '2023-04-05'  #<<<--- dia del <save>, formato yyyy-mm-dd
if(!load.SURpaises){
  ## Regresion con las dummies por pais. Es importante resaltar que en este caso <paises> indica el pais en el que sucedio el desastre, 
  #  mientras que <countries> indica el pais donde esta el indice (Ejemplo de <countries>: 'Brazil' que corresponde a 'Bovespa') 
  
  # Con el objetivo de guardar los objetos en un archivo .RData se crean las listas <resid_countries_list>, que contiene los residuales del 
  # modelo de cada <pais>, y <coefficients_countries_list>, que contiene los coeficientes del modelo de cada <pais>. La lista <coefficients_countries_list>
  # se creo para buscar guardar objetos que pesaran menos, si se guardara <models_countries_list> esta pesaria 13 Gb..
  
  fitted_models2              = c()   # inicializar un vector que guardara los nombres de los modelos
  #models_countries_list      = list()
  coefficients_countries_list = list() 
  resid_countries_list        = list() 
  for(pais in paises){ #Loop de paises
    eqsystem2                 = list()
    for(i in 1:length(countries)){ #Loop de stock indexes + paises del indice
      var.exo2                =  c('Mean_Returns_Moving_Averages', c(paste0('Int_D_', pais), paste0(pais,'_t', 0:no.rezagos.de.desatres)))
      eqsystem2[[indexes[i]]]    =  model_equation.LF(database=base_datos, countries[i], indexes[i], 
                                                      var.exo=var.exo2,  var.exo.pais=c('gdp','fdi'),  Lags='lags')
    }
    name2          = paste0("fitcoun_", pais) # genera el nombre de cada modelo depediendo del <pais>
    fitted_models2 = c(fitted_models2, name2)
    assign(name2, systemfit(eqsystem2, method="SUR"))
    #models_countries_list[[name2]] <- get(name2)
    coefficients_countries_list[[name2]]   <- summary(get(name2))$coefficients
    resid_countries_list[[name2]]          <- resid(get(name2))
  }
  # Guardar datos --------------------------------------------------------------#
  saved.day = today()  #<<<--- dia del <save>, formato yyyy-mm-dd
  # 1. En el objeto Resultados_Desastres_today() se guardan elementos claves para poder graficar, incluyendo
  # los resultados de las dos regresiones SUR
  save( coefficients_countries_list,file=paste0(paste0('Resultados_Desastres_Paises_',saved.day),'.RData')) 
  # 2. En el objeto Residuos_paises_today() se guardan los residuos de la segunda regresion (por pais), los cuales son muy 
  # pesados y no se pueden cargar
  save(resid_countries_list, file=paste0(paste0('Residuos_Paises_',saved.day),'.RData'))
} else{
 load(paste0(paste0('Resultados_Desastres_Paises_',saved.day),'.RData')) #del save 1. #Descomentar!!!
 #load(paste0(paste0('Residuos_Paises_',saved.day),'.RData'))  ## del save 2. Solo puede correrlo JP, ya que los residuos estsn en su PC y  pesan demasiado para mandarlos por github
}

# Manejo de los datos para graficar ---------------------------------------

### Para generar las graficas 4 y A.8 de Pagnottoni es necesario saber a que continente pertenece cada pais

# Lectura de la base de datos EMDAT. En excel, 1) se filtraron los 104 paises usados en el paper 
#  y 2) se dejaron los desastres entre el 8-feb-2001 y 31-dic-2019 (fechas usadas en el paper).
# Cada fila de la base de datos hace referencia a un desastre natural, y sus columnas dan informacion acerca del desastre. Las columnas que nos interesan
# son <Country>, que indica el pais donde sucedio el desastre y <Continent> continente donde sucedio el desastre, 
emdat     <- openxlsx::read.xlsx(paste0(Dir,"BASE_EMDAT.xlsx"),sheet = "Table1") #<<<---cargar base emdat
# Se transforma en un objeto <tbl>, usando la libreria <dplyr>
emdat_tbl <- tibble::as_tibble(emdat) 

# Para poder generar el vector por cada continente que incluya los paises de dicho continente selecciono de la base de datos solamente las columnas 
# Country y Continent
emdat_country_continent <- emdat_tbl %>%
  dplyr::select(Country,Continent)

# En el dataframe se repiten tanto el continente como el pais, por lo que solamente se toman los valores unicos
emdat_continents <- emdat_country_continent %>%  
  distinct()

# En el vector <continents> se tienen  los valores unicos para el continente: Asia, Europe, Americas, Oceania, Africa
continents <- sort(unique(emdat_continents$Continent))

# Generar un vector por cada continente que incluya los paises que lo conforman
for (continent in continents){
  paises_continent <- c()
  varname          <- paste0("countries_",continent)
  emdat_countries  <- emdat_continents %>% 
    dplyr::filter(Continent == continent)
  paises_continent <- unique(emdat_countries$Country)
  # Por otro lado, en la estimacion SUR los paises no pueden tener caracteres especiales. 
  paises_continent <- gsub(" ","_",paises_continent) # Cambiar los espacios de <paises_continent> por '_'
  paises_continent <- str_remove_all(paises_continent, "[(),'^]") # quitar los caracteres especificados de <paises_continent>
  paises_continent <- gsub("[\u2018\u2019']", "", paises_continent) # Asegurar que todos los caracteres <'> hayan sido removidos de <paises_continent>
  paises_continent <- iconv(paises_continent, to = "ASCII//TRANSLIT") #Cambiar caracteres acentuados a caracteres sin acento. 
  paises_continent <- str_remove_all(paises_continent, "[(),'^]") # quitar los caracteres especificados de <paises_continent>
  assign(varname, paises_continent)
}

# El siguiente codigo se utilizaba para revisar los modelos por continentes, pero fue mejorado por el codigo que sigue, ya que
# usa el objeto <coefficients_countries_list>, y no <models_countries_list>.
if(0){
  max.length <- max(sapply(fitted_models2,FUN = nchar)) ## Problema, systemfit solo genero maximo 39 caracteres
  for (continent in continents){
    varname          <- paste0("fitted_models2_",continent)
    countries_models <- paste0("fitcoun_",get(paste0("countries_",continent)))
    countries_models <- substr(countries_models,1,max.length)  ## Como systemfit creo objetos de maximo max.length, toca cortar los nombres en esta linea 
    # para que tengan el mismo numero de caracteres
    assign(varname, mget(countries_models)) 
  }
}

# maxima longitud de los nombres de la lista <coefficients_countries_list> [<fitcount_pais>]
max.length <- max(sapply(names(coefficients_countries_list),FUN = nchar))## Problema, systemfit solo genero maximo 39 caracteres

# El siguiente codigo genera una lista para cada continente: <fitted_coefficients_Africa> .. <fitted_coefficients_Oceania> 
# La longitud de cada lista anterior es el no. de paises del <continente> correspondiente. Su elto <i> corresponde al SUR 
# que incluye dummies del i-esimo <pais> del <continente> analizado
#   Y es una matriz de orden Nx4:
#      N: No.coeff.stock1 + No.coeff.stock2 + ... + No.coeff.stockFINAL (Ecuaciones que hace parte de cada SUR)
#      4: c(Estimate, Std. Error, t value, Pr(>|t|) ) 
# Por ultimo, se genera una lista <coefficients_continents_list>, que guarda las listas de cada continente, para poder guardarlas usando save()
coefficients_continents_list <- list()
for (continent in continents){
  varname                <- paste0("fitted_coefficients_",continent)
  countries_coefficients <- paste0("fitcoun_",get(paste0("countries_",continent)))
  countries_coefficients <- substr(countries_coefficients,1,max.length)  
  assign(varname, subset(coefficients_countries_list, names(coefficients_countries_list) %in% countries_coefficients)) 
  coefficients_continents_list[[varname]] <- get(varname)
}

# prueba fallida de mejora del codigo anterior
if(0){
  Coeff.por.pais = list(); ii = 0
  for (continent in continents){
    ii = ii +10
    Coeff.por.pais[[ii]]        <- paste0("fitcoun_",get(paste0("countries_",continent)))
    names(Coeff.por.pais[[ii]]) <- paste0("fitted_coefficients_",continent)
    #countries_coefficients      <- substr(countries_coefficients,1,max.length) 
    assign(varname, subset(coefficients_countries_list, names(coefficients_countries_list) %in% countries_coefficients)) 
  }
}

## ---- Graficas 1, A.1, A.6 y A.7 de Pagnottoni(2022) --- ##

# Lectura de la base de datos EMDAT,  en excel, se dejaron los desastres entre el 8-feb-2001 y 31-dic-2019 (fechas usadas en el paper).
emdat_completa     <- openxlsx::read.xlsx(paste0(Dir,"EMDAT  COMPLETA.xlsx"),sheet = "emdat data") #<<<--- base de datos 
# Se transforma en <tibble> para poder manejar con las funciones de <dplyr> 
emdat_tbl_completa <- tibble::as_tibble(emdat_completa) 
# las columnas que nos interesan son <Country>, pais del desastre, <Continent>, continente del desastre, <Disaster.subgroup>, tipo de desastre
# <Start.Year>, anho en que inicio el desastre, <Start.Month>, mes en que inicio el desastre, <Start.Day>, dia en que inicio el desastre
# <End.Year>, anho en que termino el desastre, <End.Month>, mes en que termino el desastre, <End.Day> dia en que termino el desastre

# Se  seleccionan  las variables que interesan y se renombra <Country> como region, para adjuntarla con otra base 
# de coordenadas para graficar.
emdat_interest_variables <- emdat_tbl_completa %>% 
  dplyr::select(Disaster.Subgroup, region=Country, Continent, Start.Year, Start.Month, Start.Day, End.Year, End.Month, End.Day)

#Se eliminan las filas (desastres) que contengan <NA> en el mes de inicio, ya que no se puede suponer el mes en que empezo el desastre.
emdat_final <- emdat_interest_variables %>% 
  dplyr::filter(!is.na(Start.Month))

# Cargo los datos del mapamundi del paquete <ggplot2>
world <- map_data("world")

# Los datos en <world> tienen un inconveniente: Hong Kong y Macao aparecen como regiones de China, no como "pais" propio.
# Es por tanto que tenemos que volverlos una region en la base <world>
hong_kong = which(world$subregion=="Hong Kong") # <x$subregion> equivale a una region de un pais de <x> y <x$region> a un pais de <x>
world[hong_kong,"region"] <- "Hong Kong"
macao = which(world$subregion == "Macao")
world[macao, "region"] <- "Macao"

# En la base de desastres consideraron a Antigua y Barbuda como un solo pais, mientras que en <world> los separan,
# por lo cual tenemos que unificar esas dos regiones en la base <world>.
antigua = which(world$region=="Antigua")
barbuda = which(world$region=="Barbuda")
world[c(antigua,barbuda),"region"] <- "Antigua and Barbuda"

# Lo mismo con Trinidad y Tobago
trinidad = which(world$region=="Trinidad")
tobago   = which(world$region=="Tobago")
world[c(trinidad,tobago),"region"] <- "Trinidad and Tobago"

# Con las islas virgenes tenemos dos nombres distintos en <emdata_final> mientras que en <world> solo uno, por lo 
# que se coloca el mismo nombre para ambos en <emdata_final>
virgin_british = which(emdat_final$region=="Virgin Island (British)" )
virgin_us      = which(emdat_final$region=="Virgin Island (U.S.)"  )
emdat_final[c(virgin_british,virgin_us),"region"] <- "Virgin Islands"

# En la base de datos <world> no existe Tokelau, territorio dependiente de Nueva Zelanda, por lo cual
# se le cambiara el nombre a nueva zelanda en la base <emdat_final>
tokelau = which(emdat_final$region == "Tokelau")
emdat_final[tokelau,"region"] <- "New Zealand"

# Tuvalu no existe en <world>, por lo que se quita.
tuvalu = which(emdat_final$region == "Tuvalu")
emdat_final <- emdat_final %>% 
  slice(-tuvalu)

## Por ultimo, En la base <emdat_final> hay datos para Montenegro, Serbia y aparte Serbia Montenegro, 
#  mientras que para <world> solamente hay datos para Serbia y Montenegro.
#  Por lo tanto, se agregan las regiones Serbia y Montenegro como Serbia Montenegro
serbia     = which(emdat_final$region=="Serbia" )
montenegro = which(emdat_final$region=="Montenegro"  )
emdat_final[c(serbia,montenegro),"region"] <- "Serbia Montenegro"

serbia2     = which(world$region == "Serbia")
montenegro2 = which(world$region == "Montenegro")
world[c(serbia2,montenegro2),"region"] <- "Serbia Montenegro"

## Se verifica si hay diferencias en los paises que estan en la columna region para ambas bases
diff <- sort(setdiff(emdat_final$region, world$region))

## Se cambian los nombres de <emdat_final> para que coincidan con los de <world>.
diff.world <- c("Bahamas","Bolivia", "Cape Verde", "Canary Islands","Cayman Islands", "Comoros", 
                "Democratic Republic of the Congo","Republic of Congo", "Cook Islands", "Ivory Coast", "Czech Republic",
                "Dominican Republic","Swaziland", "Gambia", "Iran", "North Korea" ,"South Korea", "Laos", "North Macedonia",
                "Marshall Islands", "Micronesia", "Moldova","Netherlands", "Niger" , "Northern Mariana Islands" ,"Palestine",
                "Philippines","Reunion","Russia" ,"Saint Barthelemy","Saint Helena", "Saint Kitts","Saint Martin" ,"Saint Vincent",
                "Sint Maarten","Sudan","Syria", "Taiwan","Tanzania", "Turks and Caicos Islands","United Arab Emirates",
                "UK","USA","Venezuela","Vietnam")

for (i in 1:length(diff)){
  indexes2 <- which(emdat_final$region == diff[i])
  emdat_final[indexes2,"region"] <- diff.world[i]
}

# En el primer grafico (Fig.1) los desastres se agrupan por pais para contar (<tally()>) cuantos hubo (<emdat_country$n>)
emdat_country <- emdat_final %>% 
  dplyr::group_by(region) %>% 
  tally()

# Se juntan las dos bases, <world> y <emdat_country> por pais, i.e. <region>
merged_data <- inner_join(world, emdat_country) #<inner_join> es intereseccion y <outer_join> es union
deciles     <- quantile(unique(merged_data$n), probs=seq(0, 1, by = 0.1))
deciles2    <- quantile(emdat_country$n, probs=seq(0,1, by=0.1)) 
## No estoy seguro de si ponerlo con valores unicos, ya que si no le quito los valores duplicados el mapa se ve muy diferente a aquel
#  de Pagnottoni.

# La funcion cut clasifica cada elemento de <merged_data$n> segun el decil al cual corresponda
merged_data$decile <- cut(merged_data$n, breaks=deciles, labels=FALSE,include.lowest=TRUE) # A cada <region> le asigan un numero (decil) entre 1 y 10
merged_data$decile <- factor(merged_data$decile) ## Dejar claro que es variable discreta, no continua


# Falta terminar los demas mapamundi
if(0){
  # Para realizar el resto de mapamundis primero es necesario ordenar la columna <Disaster.subgroup> alfabeticamente
  emdat_final_sorted <- emdat_final %>% arrange(Disaster.Subgroup)
  
  # El siguiente codigo generara una lista de objetos tipo dataframe. group_split divide el dataframe <emdat_final_sorted> en dataframes
  # dependiendo del valor de la columna <Disaster.subgroup>, por lo que la longitud de la lista sera igual a los elementos unicos en 
  # <Disaster.subgroup>. Cada dataframe de la lista contiene todos los desastres que pertenezcan a un valor de <Disaster.Subgroup>.
  subgroup_splits <- emdat_final_sorted %>% 
    group_split(Disaster.Subgroup)
  # Por otro lado, como anteriormente organizamos <emdat_final> alfabeticamente, los nombres de la lista <subgroup_splits> corresponde
  # a <Tipos.Desastres>, que tambien esta organizado alfabeticamente.
  names(subgroup_splits) <- Tipos.Desastres
  
  # Por otro lado, por cada elemento de <subgroup_splits>, vamos a generar un dataframe que de cuenta del numero de desastres ocurridos por
  # <region>. <results> es una lista que guarda dichos dataframes. Por tanto, cada elemento de results contiene un dataframe que indica 
  # cuantos desastres de cierto tipo de desastre ocurrieron en cada pais.
  results <- lapply(subgroup_splits, function(df) {
    df %>%
      group_by(region) %>% 
      tally()
  })
  
  # Para cada elemento de la lista <results> tenemos que juntarla con los datos de <world> para poder graficarla
  
  merged_data_disasters <- lapply(results, function(x) {
    inner_join(world, x ,by = "region", all.x = TRUE)
  })
  
  obj_names <- paste0("merged_data", names(merged_data_disasters))
  list2env(setNames(merged_data_disasters, obj_names), envir = .GlobalEnv)
  
  deciles <- quantile(unique(merged_data$n), probs = seq(0, 1, by = 0.1), include.lowest = TRUE)  
}

if(0){
# Guardar datos --------------------------------------------------------------#
#--- Guardado de los modelos por tipo de desastre , mas la base de retornos---#
saved.day = "2023-04-02" #<<<--- dia en que se utilizo por ultima vez save() en formato yyyy-mm-dd
# 1. En el objeto Resultados_Desastres_today() se guardan elementos claves para poder graficar, incluyendo
# los resultados de las dos regresiones SUR

#save(coefficients_disasters_list,resid_disasters_list,fitted_models, Retornos, paises,coefficients_countries_list,
#     coefficients_continents_list,file=paste0(paste0('Resultados_Desastres_',today()),'.RData')) 

# 2. En el objeto Residuos_paises_today() se guardan los residuos de la segunda regresion (por pais), los cuales son muy 
# pesados y no se pueden cargar
#save(resid_countries_list, file=paste0(paste0('Residuos_Paises_',today()),'.RData'))

load(paste0(paste0('Resultados_Desastres_',saved.day),'.RData')) #del save 1.
#load(paste0(paste0('Residuos_Paises_',saved.day),'.RData'))  ## del save 2. Solo puede correrlo JP, ya que los residuos estsn en su PC y  pesan demasiado para mandarlos por github
}