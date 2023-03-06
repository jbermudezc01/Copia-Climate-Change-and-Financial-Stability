
rm(list = ls())
setwd('C:/Users/jpber/OneDrive/Documents/Codigo_compartido_Melo/Climate_Change_and_Financial_Stability/Climate-Change-and-Financial-Stability/Bases')
#setwd('/Users/lumelo/archivos/Climate-Change-and-Financial-Stability/Github/Climate-Change-and-Financial-Stability/Bases')

cat("\014")

### Libraries ====

library(lubridate)
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

### Lectura de datos ======

## Se crea una funcion que va a leer los csv para los indices bursatiles, genera una lista de los indices bursatiles
## en formato xts. 
read_csv_files <- function(countries) {
  xts_list     <- list()
  for (country in countries) {
    csv_file <- paste0("Stocks_", country, ".csv")
    csv      <- read.csv(csv_file, header = TRUE, sep = ";", quote = "\"", col.names = c("Date","Price", "Open", 
                                                                                         "High","Low","Vol.","Change%"))
    colnames <- names(csv)
    for (colname in colnames[2:length(colnames)]) {
      csv[, colname] <- as.numeric(gsub(",","",csv[, colname]))
    } ## Muestra warning() ya quehay una columna que contiene caracteres "M" 
    csv$Date <- as.Date(csv$Date, "%m/%d/%Y")
    xts_list[[country]] <- xts(csv$Price, csv$Date)
  }
  return(xts_list)
}


# Se genera un vector con el nombre de los paises de los cuales se tiene datos de indice bursatil
countries <- c("Australia","Belgium", "Brazil", "Canada", "Chile", "Denmark", "Finland",
               "France", "Germany", "HongKong", "India", "Indonesia","Mexico","Netherlands","Norway","Poland","Russia",
               "SouthAfrica","SouthKorea", "Spain", "Sweden","Switzerland","Thailand","Turkey", 
               "UnitedKingdom","USA1","USA2")

# Corre la funcion dando una lista con los indices bursatiles
xts_list <- read_csv_files(countries)

# Genera una base de datos de los indices en formato xts
base_test <- do.call(merge, xts_list)

# Generar un vector de fechas en los que solo se tiene valores para uno o dos mercados
navalues = c()
for (i in 1:nrow(base_test)) {
  row <- base_test[i, ]
  if (sum(is.na(row))>=length(countries)-2){
    navalues <- c(navalues, index(row))}
} 

# Eliminar aquellos dias que hacen parte del vector navalues, dejando solo los dias en los que se tiene datos para al
# menos 3 mercados.
for (day in navalues) 
  base_test <- subset(base_test, subset = index(base_test) != day, drop = TRUE)

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
### retorno de orden 22, ya que hay aproximadamente 22 dias para cada mes, utilizando la siguiente funcion

moving_average <- function(x){
  mov_average_list <- list()
  for(column in 1:ncol(x)){
    rolling_average <- rollmean(x[, column], k = 22, align = "right")
    mov_average_list[[length(mov_average_list)+1]] <- rolling_average
  }
  return(mov_average_list)
}

#La funcion nos genera una lista con el promedio movil de los retornos, por lo cual las uno en formato xts
mov_average_list <- moving_average(base_retornos)
mov_average_base <- do.call(merge,mov_average_list)

#La variable que se usa en el paper es la media de los promedios. El procedimiento es generar un vector al cual
#se le ira concatenando la media por cada dia, y luego generar un objeto xts con estos valores
mean_mov_average <- c()
for(row in 1:nrow(mov_average_base)){mean_mov_average <- c(mean_mov_average,mean(mov_average_base[row,]))}

mean_mov_average_xts <- xts(mean_mov_average,order.by = index(mov_average_base))
colnames(mean_mov_average_xts) <- c("Mean moving average")

#Hay que tener en cuenta que la muestra que se utiliza en el paper no es la misma que la que se tiene en las bases,
#por lo que se reduce la base para los datos del febrero 08 2001 a diciembre 30 2019.
#La funcion muestra_paper va a seleccionar desde un cierto dia, el cual elegimos 08 febrero 2001
#siguiendo el paper.

dia <- "2001-02-08"

muestra_paper <- function(obj,x){
  indice           <- index(obj)
  base_start       <- which(indice == x)
  obj              <- obj[base_start:nrow(obj),]
  return(obj)
}

#Utilizamos la funcion para las bases
Retornos <- muestra_paper(base_retornos,dia)
Promedio_movil <- muestra_paper(mean_mov_average_xts,dia)

### Table 1 de Pagnottoni: Estadistica descriptivas ===============

## Generar skewness, kurtosis, mean, max, min, sd

skewness <- moments::skewness(Retornos)
kurtosis <- kurtosis(Retornos)
mean     <- apply(Retornos, MARGIN=2, FUN=mean)
max      <- apply(Retornos, MARGIN=2, FUN=max)
min      <- apply(Retornos, MARGIN=2, FUN=min)
sd       <- apply(Retornos, MARGIN=2, FUN=sd)

Stats = cbind(min,max,mean,sd,skewness,kurtosis )

print(Stats, digits=3)

### Desagregacion temporal ====
# Las siguientes variables a agregar es el crecimiento del PIB y el crecimiento del FDI diarios, por lo que es 
# necesario hacer desagregacion temporal.

## Funciones. La primera es la funcion de Chow lin,la segunda es una funcion que genera una lista ====

#La siguiente función toma cinco argumentos: el primero es una lista con las series de tiempo a las cuales se les
# quiere realizar la desagregacion temporal, el segundo es la matriz de agregacion, el tercero es un vector
# constante con valores 1, el cuarto es el valor de alpha, que corresponde al valor del coeficiente del AR(1) 
# siguiendo a Chow-Lin. Por ultimo, el quinto argumento es la matriz de varianzas covarianzas. 
# Tanto la matriz de varianzas covarianzas como el procedimiento para realizar la desagregacion temporal fueron
# extraidas del articulo de Hurtado y Melo (2015).

chow_lin <- function(time_Series_list,c,w,alpha,var_covar){
  return_list <- list()
  for(series in time_Series_list){
    CW <- c%*%w
    model_gls <- gls(series ~ CW - 1, correlation = corAR1(value = alpha, fixed = TRUE)) 
    sigma_sq <- (model_gls$sigma)^2
    V = (sigma_sq/1-alpha^2)*var_covar
    CVC <- solve(c%*%(V%*%t(c)))
    beta <- (solve(t(CW)%*%(CVC%*%CW))%*%t(CW))%*%(CVC%*%series)
    y <- w%*%beta + ((V%*%t(c))%*%CVC)%*%(series-CW%*%beta)
    y <- xts(y,order.by= index(base_precios)) 
    gdp_growth <- diff(log(y))[2:nrow(y),]
    return_list[[length(return_list)+1]] <- gdp_growth
  }
  return(return_list)
} ## Argumentos: 1, lista con las series de tiempo, 2: matriz de agregacion, 3. vector constante, 4. alpha, 
## 5. matriz var covar 

## La siguiente funcion genera una lista de las series de tiempo que queremos desagregar. Necesitamos la lista ya que
# la funcion chowlin pide de primer argumento la lsita con las series de tiempo a desagregar.

series_list_function <- function(ts1){
  series_list <- list()
  for(country in colnames(ts1)){
    func_series <- as.vector(ts1[,country])
    series_list[[tolower(country)]] <- func_series
  }
  return(series_list)
}


### DATOS para GDP trimestral ====

#Leer la base de datos, establecer el formato fecha y generar la base de datos en xts y la lista a ser desagregada
gdp_countries     <- read.csv("GDP_COUNTRIES.csv", header = TRUE, sep = ";") 
dates             <- as.Date(as.yearqtr(gdp_countries$Time)) #date format 
gdp_countries_xts <- xts(gdp_countries[-1], order.by = dates)
quarterly_series  <- series_list_function(gdp_countries_xts)

### Matriz de agregacion GDP trimestral =============================================================================

# El metodo de chow-lin requiere una matriz de agregacion. Sin embargo, en el metodo fast se tiene en cuenta las 
# diferencias de dias que puede haber en cada mes. Por tanto, es necesario crear una matriz de agregacion que 
# tenga lo anterior en cuenta. 


nrows <- length(quarterly_series$australia)   ## Hay 76 trimestres en la base de datos, numero de datos trimestrales
ncols <- nrow(base_precios)   ## Dias de los cuales tenemos precios, el retorno sera un dato menos, pero a este
## al tomarle diferencia para el crecimiento tambien perdera un dato

qtr_agr <- matrix(0, nrow = nrows, ncol = ncols) #matriz de agregacion 

dates <- as.character(index(base_precios))

## Extrae los meses en formato yyyy-mm sin repeticiones.
months <- unique(substr(dates,1,7))

## La siguiente funcion sera usada para generar la matriz de agregacion, añadiendo uno a los dias que correspondan 
## a cada trimestre.
days <- function(x, m){
  first_month <- months[3*x+1]
  second_month <- months[3*x+2]
  third_month <- months[3*x+3]
  for(date in dates){
    if(substr(date,1,7)==first_month |substr(date,1,7)==second_month|substr(date,1,7)==third_month){
      pos <- which(dates == date)
      #print(c(date,pos))
      m[x+1,pos] <- 1
    }
  }
  return(m)
} ## argumentos x va a ser enteros, m la matriz de agregacion

#Realizamos la matriz de agregacion usando la funcion
for(i in 0:(nrows-1)){
  qtr_agr <- days(i, qtr_agr)
}


### Ahora sigamos con la solucion de Chow Lin. 
#Para poder usar la funcion, necesitamos el vector, alpha y la matriz de var covar

vec_cte <- c(rep(1, ncols)) ## vector constante
alpha_fast = 0.99999

#generar la matriz de var- cov de acuerdo al paper analizado
matriz_var_cov_0 <- matrix(0,nrow = ncols,ncol=ncols)
for(i in 1:ncols){
  matriz_var_cov_0[i, i] <- 1
  for(j in 1:ncols){
    if(j != i){
      exp <- abs(j-i)
      matriz_var_cov_0[i, j] = alpha_fast^exp
    }
  }
} ##Crear la matriz de var-cov

## Usamos la funcion, lo que da una lista de las series desagregadas, y posteriormente las juntamos en una base

gdp_growth_list <- chow_lin(quarterly_series,qtr_agr,vec_cte,alpha_fast,matriz_var_cov_0)
gdp_growth_base <- do.call(merge,gdp_growth_list)

#Colocamos de nombres de las columnas los paises
names <- colnames(base_retornos)
colnames(gdp_growth_base) <- names[1:ncol(gdp_growth_base)]
#Para diferenciar los nombres de los retornos le agregamos un prefijo gdp
colnames(gdp_growth_base) <- paste("gdp",colnames(gdp_growth_base),sep="_")


### DESAGREGACION TEMPORAL FDI - Datos ===============================================================================

#Ahora para hacer la desagregacion temporal del FDI necesitaremos los mismos cinco argumentos: una lista de las 
# series a desagregar, un vector constante, una matriz de agregación y una matriz de varianzas covarianzas-
# el alpha puede seguir siendo el mismo, ya que para el metodo fast debe ser 0.99999.

fdi_countries <- read_xlsx("FDI_anual.xlsx", sheet="FDI")
fdi_countries_ts <- as.ts(fdi_countries[,-1],start=2001,frequency=1)
fdi_series <- series_list_function(fdi_countries_ts)


##Matriz de agregacion anual FDI 


fdi_rows <- nrow(fdi_countries_ts)
##El numero de columnas es igual al de gdp porque se quiere desagregar en esa cantidad de dias (ncols)

fdi_agregacion_matriz  <- matrix(0, nrow = fdi_rows, ncol = ncols)

#con este ciclo generamos la matriz de agregacion, colocando uno a los dias que pertenezcan al año correspondiente.
#Por ejemplo en la primera fila tendran uno aquellos dias que pertenezcan al 2001

for(i in as.numeric(unique(substr(dates,1,4)))){
  for(date in dates){
    if(substr(date,1,4)==i){
      pos <- which(dates == date)
      fdi_agregacion_matriz[i-2000,pos] <- 1
    }
  }
}


# El vector constante depende del numero de dias a los cuales se quiere desagregar, que son los mismos por lo que se
# puede utilizar la variable w. para alfa sirve alpha_fast. la matriz de varianzas y covarianzas tambien 
# permaece igual

fdi_growth_list <- chow_lin(fdi_series,fdi_agregacion_matriz,vec_cte,alpha_fast,matriz_var_cov_0)
fdi_growth_base <- do.call(merge,fdi_growth_list)
#Colocamos los mismos nombres que la base de retornos pero le agregamos un prefijo gfdi
colnames(fdi_growth_base) <- names[1:ncol(fdi_growth_base)]
colnames(fdi_growth_base) <- paste("gfdi",colnames(fdi_growth_base),sep="_")

#Por otro lado, tambien es necesario reducir la muestra a las bases para que concuerden con la muestra de paper
#Podemos usar la funcion que estaba anteriormente especificada

Crecimiento_PIB <- muestra_paper(gdp_growth_base,dia)
Crecimiento_FDI <- muestra_paper(fdi_growth_base,dia)

### Dummies =====

dummies_database <- read_excel("EMDATA_dummies.xlsx",sheet="Biological")
biological_disasters <- dummies_database$t0
dummies_biological <- c(rep(0,nrow(Retornos)))

##Para generar las dummies, se utiliza un for loop que va mirando si el dia del desastre esta dentro de indice de
# retornos. Si el dia esta, establece 1 en la posicion de ese dia siguiendo el indice de retornos.
# Si no se encuentra entonces mira si el dia calendario siguiente está en Retornos, si se encuentra establece 1
# en el dia siguiente. Así sucesivamente hasta encontrar el dia de intercambio mas cercano al dia del desastre.

dummies_biological0 <- c(rep(0,nrow(Retornos)))
for(i in 1:length(biological_disasters)){
  for(j in 0:10){
    if((as.Date(biological_disasters[i])+j) %in% index(Retornos)){
      index_f <- which(index(Retornos) == (as.Date(biological_disasters[i])+j)) 
      dummies_biological0[index_f] <- 1
      break
    }
  }
}

## Para crear las dummies t1, t2, t3, t4 se rezaga las dummies t0. Se completan los NA con 0 porque no hay desastres
#  antes a tener en cuenta
dummies_biological1 <- lag(dummies_biological0,1)
dummies_biological2 <- lag(dummies_biological0,2)
dummies_biological3 <- lag(dummies_biological0,3)
dummies_biological4 <- lag(dummies_biological0,4)
dummies_biological  <- cbind(dummies_biological0,dummies_biological1,dummies_biological2,dummies_biological3,
                             dummies_biological4)
dummies_biological_c <- ifelse(is.na(dummies_biological),0,dummies_biological)

#se genera la dummy D
D <- c()
for(i in 1:nrow(dummies_biological_c)){
  if(sum(dummies_biological_c[i,]) == 0){
    D <- c(D,sum(dummies_biological_c[i,]))
  }else if(sum(dummies_biological_c[i,]) != 0){
    D <- c(D,sum(dummies_biological_c[i,])/sum(dummies_biological_c[i,])) 
  }
}

dummies_biological_complete <- cbind(dummies_biological_c, D)  

dummies_biological_xts <- xts(dummies_biological_complete,order.by=index(Retornos))


## Creo una funcion que va a generar las dummies por los tipos de desastres

create_dummies_xts <- function(excel_file){
  sheet_names <- excel_sheets(excel_file)
  xts_dummies_list <- list()
  
  #Loop para todas las hojas
  for(sheet_name in sheet_names) {
    current_sheet    <- read.xlsx(excel_file, sheet = sheet_name, detectDates = TRUE)
    dummies_t0       <- current_sheet$t0
    t_0 <- c(rep(0,nrow(Retornos)))
    ##Para generar las dummies, se utiliza un for loop que va mirando si el dia del desastre esta dentro de indice de
    # retornos. Si el dia esta, establece 1 en la posicion de ese dia siguiendo el indice de retornos.
    # Si no se encuentra entonces mira si el dia calendario siguiente está en Retornos, si se encuentra establece 1
    # en el dia siguiente. Así sucesivamente hasta encontrar el dia de intercambio mas cercano al dia del desastre.
    
    for(i in 1:length(dummies_t0)){
      for(j in 0:10){
        if((as.Date(dummies_t0[i])+j) %in% index(Retornos)){
          index_f <- which(index(Retornos) == (as.Date(dummies_t0[i])+j)) 
          t_0[index_f] <- 1
          break
        }
      }
    }
    t_1 <- lag(t_0,1)
    t_2 <- lag(t_0,2)
    t_3 <- lag(t_0,3)
    t_4 <- lag(t_0,4)
    dummies_df       <- cbind(t_0,t_1,t_2,t_3,t_4)
    dummies_df_c     <- ifelse(is.na(dummies_df),0,dummies_df)
    
    #se genera la dummy D
    D <- c()
    for(i in 1:nrow(dummies_df_c)){
      if(sum(dummies_df_c[i,]) == 0){
        D <- c(D,sum(dummies_df_c[i,]))
      }else if(sum(dummies_df_c[i,]) != 0){
        D <- c(D,sum(dummies_df_c[i,])/sum(dummies_df_c[i,])) 
      }
    }
    
    dummies_complete <- cbind(dummies_df_c, D)  
    dummies_xts <- xts(dummies_complete,order.by=index(Retornos))
    
    #Crear objetos distintos para cada hoja
    xts_name <- paste0(sheet_name, "_dummies_xts}")
    xts_dummies_list[[xts_name]] <- dummies_xts
  }
  return(xts_dummies_list)
}

dummies <- create_dummies_xts("EMDATA_dummies.xlsx")

biological_dummies           <- dummies$`Biological_dummies_xts}`
colnames(biological_dummies) <- paste("biological",colnames(biological_dummies),sep="_")

meteorological_dummies       <- dummies$`Meteorological_dummies_xts}`
colnames(meteorological_dummies) <- paste("meteorological",colnames(meteorological_dummies),sep="_")

hydrological_dummies         <- dummies$`Hydrological_dummies_xts}`
colnames(hydrological_dummies) <- paste("hydrological",colnames(hydrological_dummies),sep="_")

geophysical_dummies          <- dummies$`Geophysical_dummies_xts}`
colnames(geophysical_dummies) <- paste("geophysical",colnames(geophysical_dummies),sep="_")

climatological_dummies       <- dummies$`Climatological_dummies_xts}`
colnames(climatological_dummies) <- paste("climatological",colnames(climatological_dummies),sep="_")

# Tambien es necesario crear la interaccion entre D y Rmt

interaction_function <- function(df){
  interaction <- c()
  for(i in 1:nrow(df)){
    interaction <- c(interaction, as.numeric(mean_mov_average_xts[i])*as.numeric((df[,ncol(df)])[i]))
  }
  interaction_xts <- xts(interaction, order.by = index(df))
  return(interaction_xts)
}

interaction_climatological <- interaction_function(climatological_dummies)
interaction_meteorological <- interaction_function(meteorological_dummies)
interaction_hydrological <- interaction_function(hydrological_dummies)
interaction_geophysical <- interaction_function(geophysical_dummies)
interaction_biological <- interaction_function(biological_dummies) 

### Generacion de base de datos exportada a excel ====

Date <- as.character(index(Retornos))


base_datos <- merge(Retornos,Promedio_movil,interaction_biological,interaction_climatological,
                    interaction_meteorological, interaction_hydrological,interaction_geophysical,
                    biological_dummies[,1:(ncol(biological_dummies)-1)],
                    climatological_dummies[,1:(ncol(climatological_dummies)-1)],
                    meteorological_dummies[,1:(ncol(meteorological_dummies)-1)],
                    hydrological_dummies[,1:(ncol(hydrological_dummies)-1)],
                    geophysical_dummies[,1:(ncol(geophysical_dummies)-1)],
                    Crecimiento_PIB, Crecimiento_FDI) 
# Cambiar a dataframe
base_df <- as.data.frame(base_datos)
#Agregar el indice, no se habia agregado antes porque la funcion merge ponia problema dada la clase de los objetos
base_final <- cbind(Date,base_df)
#Para crear un archivo excel con la base de datos
#write.xlsx(base_final,"Base_datos_final.xlsx",row.names= FALSE)


#### Variables exogenas por tipo de desastre =========

# El siguiente codigo genera las variables exogenas que seran tenidas en cuenta en cada regresion para cada tipo de 
# desastre
###

bio_exo <- with(base_datos, cbind(Mean.moving.average,interaction_biological,biological_t_0,
                                  biological_t_1,biological_t_2,biological_t_3,
                                  biological_t_4))
  
  
cli_exo <- with(base_datos, cbind(Mean.moving.average, interaction_climatological, climatological_t_0,
                                  climatological_t_1, climatological_t_2, climatological_t_3,
                                  climatological_t_4))

hyd_exo <- with(base_datos, cbind(Mean.moving.average, interaction_hydrological, hydrological_t_0,
                                  hydrological_t_1, hydrological_t_2, hydrological_t_3,
                                  hydrological_t_4))

geo_exo <- with(base_datos, cbind(Mean.moving.average, interaction_geophysical, geophysical_t_0,
                                  geophysical_t_1, geophysical_t_2, geophysical_t_3,
                                  geophysical_t_4))

met_exo <- with(base_datos, cbind(Mean.moving.average,interaction_meteorological,meteorological_t_0,
                                  meteorological_t_1,meteorological_t_2,meteorological_t_3,
                                  meteorological_t_4))





##### Revisar autocorrelacion serial ============


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

##### Agregar rezagos a las ecuaciones ===========

# Como se encontró correlacion serial para casi todas las series usando el test de Ljung - Box con 20 rezagos y n/4
# rezagos. Modelamos cada retorno siguiendo un modelo AR(p), siendo p = 0 a 20, y elegimos el modelo segun el 
# criterio de Akaike

#La siguiente funcion toma como argumento la serie de tiempo, los rezagos p y q maximos, el orden de diferencia
#un booleano para incluir la media y el metodo de estimacion. El rezago q maximo será 0 ya que solo queremos ver
#modelos AR(p).

arma_seleccion_df = function(ts_object, AR.m, MA.m, d, bool, metodo){
  index = 1
  df = data.frame(p = double(), d = double(), q = double(), AIC = double(), BIC = double())
  for (p in 0:AR.m) {
    for (q in 0:MA.m)  {
      fitp <- arima(ts_object, order = c(p, d, q), include.mean = bool, 
                    method = metodo)
      T.model = length(resid(fitp))
      Aic = T.model*log(sum(resid(fitp)^2))+ 2*(p+q+1)   ## De acuerdo con Enders 2014.
      Bic = T.model*log(sum(resid(fitp)^2)) + T.model*(p+q+1)  
      df[index,] = c(p, d, q, Aic, Bic)
      index = index + 1
    }
  }  
  return(df)
}

## Posteriormente, generamos una funcion que encuentre los rezagos optimos para cada pais utilizando el
# criterio de Akaike. Además, la función tambien va a generar los rezagos y agregandolos a un dataframe
# Vamos a usar un for loop para generar un dataframe de rezagos para cada pais 

lag_function <- function(country){
  
  #Utilizamos las funciones arma_seleccion_df y arma_min_AIc para obtener el rezago para incluir en la ecuacion
  #segun el criterio de Akaike
  mod <- arma_seleccion_df(ts_object=base_retornos[,country], AR.m=20, MA.m=0, d=0, bool=TRUE, metodo="CSS")
  p   <- mod[which.min(mod$AIC),'p']
  
  #generamos una base de datos que genere columnas de rezagos, el numero de columnas sera el mismo que el orden 
  #obtenido en el procedimiento anterior
  if(p>0)lags_df <- timeSeries::lag((base_retornos[,country]),c(1:p))
  
  #Lo colocamos desde el 8 de febrero para cuadrar con el indice de la base de datos principal
  lags_reduced <- muestra_paper(lags_df,dia)
  
  #Nombres del dataframe de rezagos
  colnames(lags_reduced) <- paste0(country,'.l',1:p) 
  
  return(lags_reduced)
}

##Realizar for loop para obtener las matrices de rezagos

for(country in countries){
  var_name <- paste0("lags_reduced_",country)
  lags <- lag_function(country)
  assign(var_name,lags)
}


#### Funcion para la estimacion del modelo ==========

# Ya teniendo los rezagos, la variable dependiente y las variables exogenas, generaremos una funcion
# que genere una formula para cada pais para cada uno de los 5 tipos de desastres.Lo anterior dado 
# que vamos a estimar por el metodo SUR, el cual necesitara las 27 ecuaciones siguiendo el paper
# de Pagnottoni.

model_equation <- function(country,exo){
  
  #Busca el dataframe con los rezagos
  lags_name <- paste0("lags_reduced_", country)
  lags_df <- get(lags_name)
  
  #Busca las variables para el gdp y el fdi
  gdp_variable <- paste("gdp",country,sep="_")
  fdi_variable <- paste("gfdi",country,sep="_")
  
  #Genera la ecuacion n.4 por el país country
  eq  <- base_datos[,country]  ~ exo + base_datos[,gdp_variable]  + base_datos[,fdi_variable] + lags_df
  return(eq)
}

### Realizar for loop a lo largo de todos los paises para obtener las ecuaciones a estimar. 
# Tambien a lo largo de los 5 tipos de desastres

eqsystem = list()

disasters_exo = c("bio_exo","cli_exo","hyd_exo","geo_exo","met_exo")

# El siguiente for genera una estimacion para cada uno de los 5 tipos de desastres, los cuales tomaran
# los nombres de fitsur_bio, fitsur_cli, fitsur_hyd, fitsur_geo, fitsur_met.
# Por otro lado, en la lista fitted_models generamos el nombre de losmodelos estimados, que necesitaremos 
# mas adelante.

fitted_models <- c()
for(disaster in disasters_exo){
  for(country in countries){
    eqsystem[[country]] <- model_equation(country,get(disaster))
  }
  three_l = substr(disaster,1,3)
  name = paste0("fitsur_",three_l)
  fitted_models <- c(fitted_models,name)
  assign(name,systemfit(eqsystem,method="SUR"))
} 

##Generamos una funcion que genere la densidad de los coeficientes dependiendo cuantos pasos 
# adelante este

dens <- function(fit, step){
  coefs <- coef(get(fit))
  interest_indices <- grep(step,names(coefs))
  interest_coefficients <- coefs[interest_indices]
  densidad <- density(as.numeric(interest_coefficients))
  return(densidad)
}

#De acuerdo con la notacion de arriba, los coeficientes el dia del evento terminan en t_0, el dia sig
# en t_1, dos dias despues t_2, y asi hasta llegar a t_4. Por otro lado, tambien generamos una 
# lista con los modelos estimados que tenemos

steps <- c("t_0","t_1","t_2","t_3","t_4")

## El siguiente ciclo genera la densidad Kernel de los coeficientes para cada tipo de desastre 
## y para todos los t_0, t_1 ...


for(step in steps){
  for(model in fitted_models){
    dens_name <- paste("dens",model,step,sep="_")
    assign(dens_name,dens(model,step))
  }
}


##Ya con lo anterior podemos hacer las graficas para los 5 tipos de desastres para todos los t pasos
# adelante

limite <- max(max(dens_fitsur_bio_t_0$y),max(dens_fitsur_cli_t_0$y),max(dens_fitsur_geo_t_0$y),
              max(dens_fitsur_hyd_t_0$y),max(dens_fitsur_met_t_0$y))
X11()
plot(dens_fitsur_bio_t_0, main = "Kernel density of AR t_0", col ="blue",lwd=2,ylim=c(0,limite))
lines(dens_fitsur_cli_t_0,col="red",lwd = 2)
lines(dens_fitsur_geo_t_0,col="orange",lwd = 2)
lines(dens_fitsur_hyd_t_0,col="purple",lwd = 2)
lines(dens_fitsur_met_t_0,col="green",lwd = 2)


limite_2 <- max(max(dens_fitsur_bio_t_1$y),max(dens_fitsur_cli_t_1$y),max(dens_fitsur_geo_t_1$y),
              max(dens_fitsur_hyd_t_1$y),max(dens_fitsur_met_t_1$y))
X11()
plot(dens_fitsur_bio_t_1, main = "Kernel density of AR t_1", col ="blue",lwd=2,ylim=c(0,limite_2))
lines(dens_fitsur_cli_t_1,col="red",lwd = 2)
lines(dens_fitsur_geo_t_1,col="orange",lwd = 2)
lines(dens_fitsur_hyd_t_1,col="purple",lwd = 2)
lines(dens_fitsur_met_t_1,col="green",lwd = 2)

limite_3 <- max(max(dens_fitsur_bio_t_2$y),max(dens_fitsur_cli_t_2$y),max(dens_fitsur_geo_t_2$y),
                max(dens_fitsur_hyd_t_2$y),max(dens_fitsur_met_t_2$y))
X11()
plot(dens_fitsur_bio_t_2, main = "Kernel density of AR t_2", col ="blue",lwd=2,ylim=c(0,limite_3))
lines(dens_fitsur_cli_t_2,col="red",lwd = 2)
lines(dens_fitsur_geo_t_2,col="orange",lwd = 2)
lines(dens_fitsur_hyd_t_2,col="purple",lwd = 2)
lines(dens_fitsur_met_t_2,col="green",lwd = 2)

limite_4 <- max(max(dens_fitsur_bio_t_3$y),max(dens_fitsur_cli_t_3$y),max(dens_fitsur_geo_t_3$y),
                max(dens_fitsur_hyd_t_3$y),max(dens_fitsur_met_t_3$y))
X11()
plot(dens_fitsur_bio_t_3, main = "Kernel density of AR t_3", col ="blue",lwd=2,ylim=c(0,limite_4))
lines(dens_fitsur_cli_t_3,col="red",lwd = 2)
lines(dens_fitsur_geo_t_3,col="orange",lwd = 2)
lines(dens_fitsur_hyd_t_3,col="purple",lwd = 2)
lines(dens_fitsur_met_t_3,col="green",lwd = 2)

limite_5 <- max(max(dens_fitsur_bio_t_4$y),max(dens_fitsur_cli_t_4$y),max(dens_fitsur_geo_t_4$y),
                max(dens_fitsur_hyd_t_4$y),max(dens_fitsur_met_t_4$y))
X11()
plot(dens_fitsur_bio_t_4, main = "Kernel density of AR t_4", col ="blue",lwd=2,ylim=c(0,limite_5))
lines(dens_fitsur_cli_t_4,col="red",lwd = 2)
lines(dens_fitsur_geo_t_4,col="orange",lwd = 2)
lines(dens_fitsur_hyd_t_4,col="purple",lwd = 2)
lines(dens_fitsur_met_t_4,col="green",lwd = 2)

## Falta CAR y manera mas facil de graficar.


cdf <- ecdf(as.numeric(coefficients))
x11()
plot(dens_bio_t_0,col="red")
lines(cdf, col="blue")