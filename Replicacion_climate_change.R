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
kurtosis <- kurtosis(Retornos)
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
  cat('\nLow frequency range:'); range(dates.low.freq)
  
  ## WARNING si los rangos son distintos, primero pasar dates.high.freq al ultimo dia de su trimestre
  quarter.high.freq <- ceiling_date(as.Date(dates.high.freq), unit = "quarter") - 1
  cat('\nHigh frequency range in trimesters:');range(quarter.high.freq)
  
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
# los nombres de fitsur_bio, fitsur_cli, fitsur_hyd, fitsur_geo, fitsur_met.
# Por otro lado, en la lista fitted_models generamos el nombre de losmodelos estimados, que necesitaremos 
# mas adelante.

eqsystem      = list()
fitted_models = c()
for(disaster in Tipos.Desastres){
  for(country in countries){
    var.exo                =          c( 'Mean_Returns_Moving_Averages', c(paste0('Int_D_', disaster), paste0(disaster,'_t', 0:no.rezagos.de.desatres)) )
    eqsystem[[country]]    =  model_equation.LF(database=base_datos, country, 
                                                var.exo=var.exo,  var.exo.pais=c('gdp','fdi'),  Lags='lags')
  }
  name          = paste0("fitsur_", substr(disaster,1,3) )
  fitted_models = c(fitted_models, name)
  assign(name, systemfit(eqsystem, method="SUR"))
} 

# De acuerdo con la notacion de los modelos estimados, los coeficientes el dia del evento terminan en t_0, 
# el dia siguiente en t_1, dos dias despues t_2, y asi hasta llegar a t_4. Por otro lado, tambien 
# generamos una lista con los modelos estimados que tenemos

steps <- c("t0","t1","t2","t3","t4")  #<<<--- vector con los días adelante del evento, hace referencia a como termina el nombre de las dummies

## El siguiente ciclo genera la densidad Kernel de los coeficientes para cada tipo de desastre 
## y para todos los t_0, t_1 ...


for(step in steps){
  for(model in fitted_models){
    dens_name <- paste("dens",model,step,sep="_")
    assign(dens_name,dens(model,step))
  }
}

##Por otro lado, necesitamos hacer la gráfica de los CAR, que es la suma de los retornos anormales.
##Con el ciclo for estamos haciendo el mismo proceso para cada uno de los 5 modelos estimados.
##Al final tendremos un vector para cada modelo que incluye los coeficientes relacionados para las 5 
##dummies temporales para todos los paises. Lo anterior para posteriormente ser sumadas por cada país para 
##generar el retorno anormal acumulado t_0+t_1+t_2+t_3+t_4

for(model in fitted_models){
  #Vamos a generar una lista para cada modelo
  var_name <- paste0("coef_vec_",model)
  coef_vec <- c()
  for(step in steps){
    #reunimos los coeficientes en coefs
    coefs <- coef(get(model))
    
    #seleccionamos solamente los coeficientes que acaben con step y lo añadimos a coef_vec
    interest_indices <- grep(step,names(coefs))
    interest_coefficients <- coefs[interest_indices]
    coef_vec <- c(coef_vec, interest_coefficients)
  }
  # al final asignamos coef_vec al nombre especifico por modelo.
  assign(var_name, coef_vec)
}

# Generamos la densidad de los retornos anormales acumulados para cada tipo de desastre
densidad_CAR_bio <- densidad_CAR(coef_vec_fitsur_Bio,countries)
densidad_CAR_cli <- densidad_CAR(coef_vec_fitsur_Cli,countries)
densidad_CAR_geo <- densidad_CAR(coef_vec_fitsur_Geo,countries)
densidad_CAR_hyd <- densidad_CAR(coef_vec_fitsur_Hyd,countries)
densidad_CAR_met <- densidad_CAR(coef_vec_fitsur_Met,countries)

### =============================== Graficas de retornos anormales ==================================

#Ya con las densidades de los retornos acumulados y de las dummies t_0, t_1, ..., t_4 podemos graficarlas


labels <- c("Biological","Climatological","Geophysical","Hydrological","Meteorological")  #<<<--- leyendas del grafico
colors <- c("blue", "tomato", "orange", "darkorchid4", "green")   #<<<--- colores que usara la grafica

# Para los CAR el vector sería
main_car   <- "Kernel denisty of CAR"  #<<<--- título para la gráfica
vector_car <- c("densidad_CAR_bio","densidad_CAR_cli","densidad_CAR_geo","densidad_CAR_hyd", 
                "densidad_CAR_met") #<<<---vector de elementos a graficar
grafico_densidad(vector_car,main_car,labels,colors)


#Para los AR_t_0 sería
main_t_0   <- "Kernel density of AR t_0"  #<<<--- título para la gráfica
vector_t_0 <- c("dens_fitsur_Bio_t0","dens_fitsur_Cli_t0","dens_fitsur_Geo_t0",
                "dens_fitsur_Hyd_t0","dens_fitsur_Met_t0") #<<<---vector de elementos a graficar
grafico_densidad(vector_t_0,main_t_0,labels,colors)


#Para los AR_t_1 sería
main_t_1   <- "Kernel density of AR t_1"  #<<<--- título para la gráfica
vector_t_1 <- c("dens_fitsur_Bio_t1","dens_fitsur_Cli_t1","dens_fitsur_Geo_t1",
                "dens_fitsur_Hyd_t1","dens_fitsur_Met_t1") #<<<---vector de elementos a graficar
grafico_densidad(vector_t_1,main_t_1,labels,colors)


#Para los AR_t_2 sería
main_t_2   <- "Kernel density of AR t_2"  #<<<--- título para la gráfica
vector_t_2 <- c("dens_fitsur_Bio_t2","dens_fitsur_Cli_t2","dens_fitsur_Geo_t2",
                "dens_fitsur_Hyd_t2","dens_fitsur_Met_t2") #<<<---vector de elementos a graficar
grafico_densidad(vector_t_2,main_t_2,labels,colors)

#Para los AR_t_3 sería
main_t_3   <- "Kernel density of AR t_3"  #<<<--- título para la gráfica
vector_t_3 <- c("dens_fitsur_Bio_t3","dens_fitsur_Cli_t3","dens_fitsur_Geo_t3",
                "dens_fitsur_Hyd_t3","dens_fitsur_Met_t3") #<<<---vector de elementos a graficar
grafico_densidad(vector_t_3,main_t_3,labels,colors)

#Para los AR_t_4 sería
main_t_4   <- "Kernel density of AR t_4"  #<<<--- título para la gráfica
vector_t_4 <- c("dens_fitsur_Bio_t4","dens_fitsur_Cli_t4","dens_fitsur_Geo_t4",
                "dens_fitsur_Hyd_t4","dens_fitsur_Met_t4") #<<<---vector de elementos a graficar
grafico_densidad(vector_t_4,main_t_4,labels,colors)

## ============================== Graficos A.3 de densidad de los retornos ========================================

colores <- c("blue","tomato","orange","darkorchid4","green","cyan","firebrick") #<<<--- colores para grafica de retornos

densidad_retornos <- apply(Retornos, MARGIN = 2, FUN = density)

# Para utilizar la función grafico_retornos necesitamos un objeto a graficar, mas un vector con los paises de cada continente, mas un
# titulo para el grafico y un vector de leyendas, el cual será el nombre del índice de cada país. Además necesitamos los colores.

# Para America seria:
main_America      <- "Densidad retornos America" #<<<--- título para la gráfica
countries_America <- c("Brazil","Chile","USA1","USA2","Canada","Mexico")  #<<<--- vector de paises que pertencen al continente
labels_America    <- c("Bovespa","S&P CLX IPSA","NASDAQ Composite","Nasdaq 100","S&P TSX Composite",
                       "S&P BMV IPC") #<<<--- leyendas, indices de cada pais del continente
grafico_retornos(densidad_retornos,countries_America,main_America,labels_America,colores)

# Para Europa del Este:
main_Europa_Este      <- "Densidad retornos Europa del Este" #<<<--- título para la gráfica
countries_Europa_Este <- c("Russia","Denmark","Turkey","Norway","Poland","Finland","Sweden") #<<<--- vector de paises que pertencen al continente
labels_Europa_Este    <- c("Moex Russia","OMX Copenhagen 20","BIST 100","OSE Benchmark","WIG20",
                           "OMX Helsinki 25","OMX Stockholm 30") #<<<--- leyendas, indices de cada pais del continente
grafico_retornos(densidad_retornos,countries_Europa_Este,main_Europa_Este,labels_Europa_Este,colores)

# Para Europa del Oeste
main_Europa_Oeste      <- "Densidad retornos Europa del Oeste"  #<<<--- título para la gráfica
countries_Europa_Oeste <- c("UnitedKingdom","Switzerland","Germany","Spain","Netherlands","Belgium","France") #<<<--- vector de paises que pertencen al continente
labels_Europa_Oeste    <- c("FTSE 100","SMI","DAX","IBEX 35","AEX","BEL 20","CAC 40") #<<<--- leyendas, indices de cada pais del continente
grafico_retornos(densidad_retornos,countries_Europa_Oeste,main_Europa_Oeste,labels_Europa_Oeste,colores)

#Para Asia
main_Asia      <- "Densidad retornos Asia" #<<<--- título para la gráfica
countries_Asia <- c("Thailand","SouthKorea","India","Indonesia","HongKong") #<<<--- vector de paises que pertencen al continente
labels_Asia    <- c("SET Index","KOSPI","Nifty 50","Jakarta Stock Exchange","Hang Seng") #<<<--- leyendas, indices de cada pais del continente
grafico_retornos(densidad_retornos,countries_Asia,main_Asia,labels_Asia,colores)

#Para Africa y Oceania
main_Africa_Oceania      <- "Densidad retornos Africa y Oceania" #<<<--- título para la gráfica
countries_Africa_Oceania <- c("SouthAfrica","Australia")  #<<<--- vector de paises que pertencen al continente
labels_Africa_Oceania    <- c("South Africa Top 40","S&P ASX 200") #<<<--- leyendas, indices de cada pais del continente
grafico_retornos(densidad_retornos,countries_Africa_Oceania,main_Africa_Oceania,labels_Africa_Oceania,colores)

### ============================ Grafico 3 Pagnottoni. AR estimates ============================================================

# Para el grafico, Pagnottoni tiene un orden específico, por lo cual toca especificarlo

pagn_orden <- c("Thailand",	"Russia", "SouthKorea", "India", "Indonesia", "Brazil", "Chile", "HongKong", 
                "USA1", "USA2", "Canada", "Mexico", "SouthAfrica", "Denmark", "Turkey", "Norway", 
                "Poland", "Finland", "UnitedKingdom", "Australia", "Sweden", "Switzerland", "Germany", 
                "Spain", "Netherlands", "Belgium", "France") #<<<--- países en el orden que aparece en la gráfica #3

labels_grafico <- c("SET Index", "MOEX Russia", "KOSPI", "Nifty 50", "Jakarta Stock Exchange", "Bovespa", 
                    "S&P CLX IPSA", "Hang Seng", "NASDAQ Composite", "Nasdaq 100", "S&P TSX Composite", "S&P BMV IPC", 
                    "South Africa Top 40", "OMX Copenhagen 20", "BIST 100", "OSE Benchmark", "WIG20", "OMX Helsinki 25", 
                    "FTSE 100", "S&P ASX 200", "OMX Stockholm 30", "SMI", "DAX", 
                    "IBEX 35", "AEX", "BEL 20", "CAC 40") #<<<--- indices en el orden que aparece en la gráfica #3

group         <- rep(labels_grafico,each=5) ## Variable que va a agrupar en grupos de a 5 los datos (porque cada 5 es un indice distinto)
colores.ar = c("#1964C4", "#C9675A", "#D5B259","darkorchid4","#709E3D")


## Para biological
ar_data_Bio       <- coef_vec_fitsur_Bio[order_coef(names(coef_vec_fitsur_Bio),pagn_orden)] ##ordenar

ar_data_frame_Bio <- data.frame(values = ar_data_Bio, 
                            group=group,
                            subgroup =steps)
ar_data_frame_Bio$group <- factor(ar_data_frame_Bio$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica grupo

plot_Bio <- grafico_estimates(ar_data_frame_Bio,"Abnormal return","Biological",colors = colores.ar)

## Para climatological
ar_data_Cli       <- coef_vec_fitsur_Cli[order_coef(names(coef_vec_fitsur_Cli),pagn_orden)] ##ordenar

ar_data_frame_Cli <- data.frame(values = ar_data_Cli, 
                                group=group,
                                subgroup =steps)
ar_data_frame_Cli$group <- factor(ar_data_frame_Cli$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica grupo

plot_Cli <- grafico_estimates(ar_data_frame_Cli,"Abnormal return","Climatological",colors = colores.ar)

## Para geophysical

ar_data_Geo       <- coef_vec_fitsur_Geo[order_coef(names(coef_vec_fitsur_Geo),pagn_orden)] ##ordenar

ar_data_frame_Geo <- data.frame(values = ar_data_Geo, 
                                group=group,
                                subgroup =steps)
ar_data_frame_Geo$group <- factor(ar_data_frame_Geo$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica grupo

plot_Geo <- grafico_estimates(ar_data_frame_Geo,"Abnormal return","Geophysical",colors = colores.ar)

## Para hydrological

ar_data_Hyd       <- coef_vec_fitsur_Hyd[order_coef(names(coef_vec_fitsur_Hyd),pagn_orden)] ##ordenar

ar_data_frame_Hyd <- data.frame(values = ar_data_Hyd, 
                                group=group,
                                subgroup =steps)
ar_data_frame_Hyd$group <- factor(ar_data_frame_Hyd$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica grupo

plot_Hyd <- grafico_estimates(ar_data_frame_Hyd,"Abnormal return","Hydrological",colors = colores.ar)

## Para meteorological

ar_data_Met       <- coef_vec_fitsur_Met[order_coef(names(coef_vec_fitsur_Met),pagn_orden)] ##ordenar

ar_data_frame_Met <- data.frame(values = ar_data_Met, 
                                group=group,
                                subgroup =steps)
ar_data_frame_Met$group <- factor(ar_data_frame_Met$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica grupo

plot_Met <- ggplot(ar_data_frame_Met, aes(x=group,y=values,fill=subgroup))+
  geom_bar(stat="identity", position="dodge", width=0.7) +
  scale_fill_manual(values=c("#1964C4", "#C9675A","#D5B259","#7C63CF","#709E3D")) +
  labs(x="index",y="Abnormal return",title="Meteorological") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10))


#graficas juntas

complete_plot <- grid.arrange(plot_Bio,plot_Cli,plot_Geo,plot_Hyd,plot_Met,nrow=5,ncol=1,heights = c(1,1,1,1,1.7))
ggsave("abnormal_returns.pdf",plot=complete_plot,device="pdf", width = 8.27, height = 11.69)

### ===========================  Grafico A.4 Pagnottoni, t-tests =======================================================

#Primero necesitamos el valor de los estadísticos t

for(model in fitted_models){
  #Vamos a generar una lista para cada modelo
  tests <- summary(get(model))$coefficients[, "t value"]
  var_name <- paste0("t_test_",model)
  t_test <- c()
  for(step in steps){
    #reunimos los coeficientes en coefs
    
    #seleccionamos solamente los coeficientes que acaben con step y lo añadimos a t_test
    interest_indices <- grep(step,names(tests))
    interest_tests <- tests[interest_indices]
    t_test <- c(t_test, interest_tests)
  }
  # al final asignamos t_test al nombre especifico por modelo.
  assign(var_name, t_test)
}

#Para biological

t_data_Bio       <- t_test_fitsur_Bio[order_coef(names(t_test_fitsur_Bio),pagn_orden)] ##ordenar
t_data_frame_Bio <- data.frame(values = t_data_Bio, 
                            group=group,
                            subgroup =steps)
t_data_frame_Bio$group <- factor(t_data_frame_Bio$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica grupo

plot_t_Bio <- grafico_estimates(t_data_frame_Bio, "t_test", "Biological",colors = colores.ar)


#Para climatological

t_data_Cli       <- t_test_fitsur_Cli[order_coef(names(t_test_fitsur_Cli),pagn_orden)] ##ordenar
t_data_frame_Cli <- data.frame(values = t_data_Cli, 
                               group=group,
                               subgroup =steps)
t_data_frame_Cli$group <- factor(t_data_frame_Cli$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica grupo

plot_t_Cli <- grafico_estimates(t_data_frame_Cli, "t_test", "Climatological",colors = colores.ar)

## Para geophysical

t_data_Geo       <- t_test_fitsur_Geo[order_coef(names(t_test_fitsur_Geo),pagn_orden)] ##ordenar
t_data_frame_Geo <- data.frame(values = t_data_Geo, 
                               group=group,
                               subgroup =steps)
t_data_frame_Geo$group <- factor(t_data_frame_Geo$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica grupo

plot_t_Geo <- grafico_estimates(t_data_frame_Geo, "t_test", "Geophysical",colors = colores.ar)


## Para hydrological

t_data_Hyd       <- t_test_fitsur_Hyd[order_coef(names(t_test_fitsur_Hyd),pagn_orden)] ##ordenar
t_data_frame_Hyd <- data.frame(values = t_data_Hyd, 
                               group=group,
                               subgroup =steps)
t_data_frame_Hyd$group <- factor(t_data_frame_Hyd$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica grupo

plot_t_Hyd <- grafico_estimates(t_data_frame_Hyd, "t_test", "Hydrological",colors = colores.ar)


## Para meteorological

t_data_Met       <- t_test_fitsur_Met[order_coef(names(t_test_fitsur_Met),pagn_orden)] ##ordenar
t_data_frame_Met <- data.frame(values = t_data_Met, 
                               group=group,
                               subgroup =steps)
t_data_frame_Met$group <- factor(t_data_frame_Met$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica grupo

plot_t_Met <- ggplot(t_data_frame_Met, aes(x=group,y=values,fill=subgroup))+
  geom_bar(stat="identity", position="dodge", width=0.7) +
  scale_fill_manual(values=c("#1964C4", "#C9675A", "#D5B259","#7C63CF","#709E3D")) +
  labs(x="index",y="t-test",title="Meteorological") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

#graficas juntas

complete_t_plot <- grid.arrange(plot_t_Bio,plot_t_Cli,plot_t_Hyd,plot_t_Geo,plot_t_Met,nrow=5,ncol=1,heights = c(1,1,1,1,1.7))
ggsave("t_tests.pdf",plot=complete_t_plot,device="pdf", width = 8.27, height = 11.69)

## ---------------------- SEGUNDA REGRESION, AHORA ES POR PAISES, NO POR TIPO DE DESASTRE. ---------- ##

