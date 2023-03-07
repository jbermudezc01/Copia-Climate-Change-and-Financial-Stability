
rm(list = ls())
setwd('C:/Users/jpber/OneDrive/Documents/Codigo_compartido_Melo/Climate_Change_and_Financial_Stability/Climate-Change-and-Financial-Stability')
#setwd('/Users/lumelo/archivos/Climate-Change-and-Financial-Stability/Github/Climate-Change-and-Financial-Stability')


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

#--- Carga de funciones ---#
source('Functions_Climate_change.r')

### Lectura de datos ======

# Se genera un vector con el nombre de los paises de los cuales se tiene datos de indice bursatil
countries <- c("Australia","Belgium", "Brazil", "Canada", "Chile", "Denmark", "Finland",
               "France", "Germany", "HongKong", "India", "Indonesia","Mexico","Netherlands","Norway","Poland","Russia",
               "SouthAfrica","SouthKorea", "Spain", "Sweden","Switzerland","Thailand","Turkey", 
               "UnitedKingdom","USA1","USA2")

# Establecemos el directorio de los datos
Dir      = paste0(getwd(),'/Bases/') #Directorio de datos, se supone que el subdirectorio <Bases> existe

# Genera una base de datos de los indices en formato xts, llamando a la función read_csv
base_test <- read_csv(Dir,countries)

# Generar un vector de fechas en los que solo se tiene valores para uno o dos mercados
navalues = c()
for (i in 1:nrow(base_test)) {
  row <- base_test[i, ]
  if (sum(is.na(row))>=length(countries)-2){
    navalues <- c(navalues, index(row))}
} 

# Eliminar aquellos dias que hacen parte del vector navalues, dejando solo los dias en los que se tiene datos para al
# menos 3 mercados. Se utilizó la función weekdays() para comprobar que ningún dia fuese sábado o domingo.
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
### retorno de orden 22, ya que hay aproximadamente 22 dias para cada mes, usando la funcion moving_average.

orden <- 22
mov_average_base <- moving_average(base_retornos,orden)

#La variable que se usa en el paper es la media de los promedios. El procedimiento es generar un vector al cual
#se le ira concatenando la media por cada dia, y luego generar un objeto xts con estos valores
mean_mov_average <- c()
for(row in 1:nrow(mov_average_base)){mean_mov_average <- c(mean_mov_average,mean(mov_average_base[row,]))}

# El vector que incluye la media de los promedios moviles se convierte en xts
mean_mov_average_xts <- xts(mean_mov_average,order.by = index(mov_average_base))
colnames(mean_mov_average_xts) <- c("Mean_Returns_Moving_Averages")

#Hay que tener en cuenta que la muestra que se utiliza en el paper no es la misma que la que se tiene en las bases,
#por lo que se reduce la base para los datos del febrero 08 2001 a diciembre 30 2019.
#La funcion muestra_paper va a seleccionar desde un cierto dia, el cual elegimos 08 febrero 2001
#siguiendo el paper.

dia <- "2001-02-08"

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


### DATOS para GDP trimestral ====

#Leer la base de datos, establecer el formato fecha y generar la base de datos en xts y la lista a ser desagregada
gdp_countries     <- read.csv(paste0(Dir,"GDP_COUNTRIES.csv"), header = TRUE, sep = ";") 
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
meses <- unique(substr(dates,1,7))

#Realizamos la matriz de agregacion usando la funcion days. En este caso los enteros i iran de 0 a 75, 
# y de acuerdo con la función esto generara el primer a tercer mes de cada trimestre. (Explicar mejor)
for(i in 0:(nrows-1)){
  qtr_agr <- days(i, qtr_agr,meses)
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

gdp_growth_base <- chow_lin(quarterly_series,qtr_agr,vec_cte,alpha_fast,matriz_var_cov_0)

#Colocamos de nombres de las columnas los paises
names <- colnames(base_retornos)
colnames(gdp_growth_base) <- names[1:ncol(gdp_growth_base)]
#Para diferenciar los nombres de los retornos le agregamos un prefijo gdp
colnames(gdp_growth_base) <- paste("gdp",colnames(gdp_growth_base),sep="_")


### DESAGREGACION TEMPORAL FDI - Datos ===============================================================================

#Ahora para hacer la desagregacion temporal del FDI necesitaremos los mismos cinco argumentos: una lista de las 
# series a desagregar, un vector constante, una matriz de agregación y una matriz de varianzas covarianzas-
# el alpha puede seguir siendo el mismo, ya que para el metodo fast debe ser 0.99999.

fdi_countries <- read_xlsx(paste0(Dir,"FDI_anual.xlsx"), sheet="FDI")
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

fdi_growth_base <- chow_lin(fdi_series,fdi_agregacion_matriz,vec_cte,alpha_fast,matriz_var_cov_0)

#Colocamos los mismos nombres que la base de retornos pero le agregamos un prefijo gfdi
colnames(fdi_growth_base) <- names[1:ncol(fdi_growth_base)]
colnames(fdi_growth_base) <- paste("gfdi",colnames(fdi_growth_base),sep="_")

#Por otro lado, tambien es necesario reducir la muestra a las bases para que concuerden con la muestra de paper
#Podemos usar la funcion que estaba anteriormente especificada

Crecimiento_PIB <- muestra_paper(gdp_growth_base,dia)
Crecimiento_FDI <- muestra_paper(fdi_growth_base,dia)

### Dummies =====

# Corremos la función create_dummies en el archivo que contiene las fechas de las dummies
dummies <- create_dummies(paste0(Dir,"EMDATA_dummies.xlsx"))

#Para cada tipo de desastre lo guardamos en un xts distinto
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

interaction_climatological <- interaction_function(climatological_dummies)
interaction_meteorological <- interaction_function(meteorological_dummies)
interaction_hydrological <- interaction_function(hydrological_dummies)
interaction_geophysical <- interaction_function(geophysical_dummies)
interaction_biological <- interaction_function(biological_dummies) 

### Generacion de base de datos con las variables que serán usadas para la estimación ====

Date <- as.character(index(Retornos))


base_datos <- merge(Retornos,Promedio_movil,interaction_biological,interaction_climatological,
                    interaction_meteorological, interaction_hydrological,interaction_geophysical,
                    biological_dummies[,1:(ncol(biological_dummies)-1)],
                    climatological_dummies[,1:(ncol(climatological_dummies)-1)],
                    meteorological_dummies[,1:(ncol(meteorological_dummies)-1)],
                    hydrological_dummies[,1:(ncol(hydrological_dummies)-1)],
                    geophysical_dummies[,1:(ncol(geophysical_dummies)-1)],
                    Crecimiento_PIB, Crecimiento_FDI) 

## Tambien es posible exportar a excel, en cuyo caso se sigue este codigo:
if(0){
# Cambiar a dataframe
base_df <- as.data.frame(base_datos)
#Agregar el indice, no se habia agregado antes porque la funcion merge ponia problema dada la clase de los objetos
base_final <- cbind(Date,base_df)
#Para crear un archivo excel con la base de datos
write.xlsx(base_final,"Base_datos_final.xlsx",row.names= FALSE)
}

#### Variables exogenas por tipo de desastre =========

# El siguiente codigo genera las variables exogenas que seran tenidas en cuenta en cada regresion para cada tipo de 
# desastre
###

bio_exo <- with(base_datos, cbind(interaction_biological,biological_t_0,
                                  biological_t_1,biological_t_2,biological_t_3,
                                  biological_t_4))
  
  
cli_exo <- with(base_datos, cbind(interaction_climatological, climatological_t_0,
                                  climatological_t_1, climatological_t_2, climatological_t_3,
                                  climatological_t_4))

hyd_exo <- with(base_datos, cbind(interaction_hydrological, hydrological_t_0,
                                  hydrological_t_1, hydrological_t_2, hydrological_t_3,
                                  hydrological_t_4))

geo_exo <- with(base_datos, cbind(interaction_geophysical, geophysical_t_0,
                                  geophysical_t_1, geophysical_t_2, geophysical_t_3,
                                  geophysical_t_4))

met_exo <- with(base_datos, cbind(interaction_meteorological,meteorological_t_0,
                                  meteorological_t_1,meteorological_t_2,meteorological_t_3,
                                  meteorological_t_4))


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


##Realizar for loop para obtener las matrices de rezagos

for(country in countries){
  var_name <- paste0("lags_reduced_",country)
  lags <- lag_function(country)
  assign(var_name,lags)
}


#### Funcion para la estimacion del modelo ==========

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

# De acuerdo con la notacion de los modelos estimados, los coeficientes el dia del evento terminan en t_0, 
# el dia siguiente en t_1, dos dias despues t_2, y asi hasta llegar a t_4. Por otro lado, tambien 
# generamos una lista con los modelos estimados que tenemos

steps <- c("t_0","t_1","t_2","t_3","t_4")

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

car_coefficients <- c()

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
densidad_CAR_bio <- densidad_CAR(coef_vec_fitsur_bio,countries)
densidad_CAR_cli <- densidad_CAR(coef_vec_fitsur_cli,countries)
densidad_CAR_geo <- densidad_CAR(coef_vec_fitsur_geo,countries)
densidad_CAR_hyd <- densidad_CAR(coef_vec_fitsur_hyd,countries)
densidad_CAR_met <- densidad_CAR(coef_vec_fitsur_met,countries)

### Graficas de retornos anormales =======

#Ya con las densidades de los retornos acumulados y de las dummies t_0, t_1, ..., t_4 podemos graficarlas

#Cree una función para poder graficar evitando repeticiones en el codigo. La función toma tres argumentos.
#El primero de ellos es muy especifico: un vector que en la primera posición tiene el título del gráfico, 
#y luego 5 argumentos que son las densidades a estimar en el mismo orden que este en el objeto labels.
#Por ejemplo en labels el primero seria la densidad biological, ya sea densidad_CAR_bio o dens_fitsur_bio_t_0
#El segundo argumento será labels, que indica las leyendas
#El tercero es un vector con cinco colores.

labels <- c("Biological","Climatological","Geophysical","Hydrological","Meteorological")
colors <- c("blue", "tomato", "orange", "purple", "green")

# Para los CAR el vector sería
vector_a_graficar <- c("Kernel density of CAR","densidad_CAR_bio","densidad_CAR_cli","densidad_CAR_geo",
                       "densidad_CAR_hyd", "densidad_CAR_met")

grafico(vector_a_graficar,labels,colors)

#Para los AR_t_0 sería

vector_t_0 <- c("Kernel density of AR t_0", "dens_fitsur_bio_t_0","dens_fitsur_cli_t_0","dens_fitsur_geo_t_0",
                 "dens_fitsur_hyd_t_0","dens_fitsur_met_t_0")

grafico(vector_t_0,labels,colors)

#Para los AR_t_1 sería

vector_t_1 <- c("Kernel density of AR t_1", "dens_fitsur_bio_t_1","dens_fitsur_cli_t_1","dens_fitsur_geo_t_1",
                "dens_fitsur_hyd_t_1","dens_fitsur_met_t_1")

grafico(vector_t_1,labels,colors)

#Para los AR_t_2 sería

vector_t_2 <- c("Kernel density of AR t_2", "dens_fitsur_bio_t_2","dens_fitsur_cli_t_2","dens_fitsur_geo_t_2",
                "dens_fitsur_hyd_t_2","dens_fitsur_met_t_2")

grafico(vector_t_2,labels,colors)

#Para los AR_t_3 sería

vector_t_3 <- c("Kernel density of AR t_3", "dens_fitsur_bio_t_3","dens_fitsur_cli_t_3","dens_fitsur_geo_t_3",
                "dens_fitsur_hyd_t_3","dens_fitsur_met_t_3")

grafico(vector_t_3,labels,colors)

#Para los AR_t_4 sería

vector_t_4 <- c("Kernel density of AR t_4", "dens_fitsur_bio_t_4","dens_fitsur_cli_t_4","dens_fitsur_geo_t_4",
                "dens_fitsur_hyd_t_4","dens_fitsur_met_t_4")

grafico(vector_t_4,labels,colors)


## La función de distribución acumulada de la densidad meterologica t_0 si es de la forma que deberia ser

coef <- coef(get("fitsur_met"))[grep("t_0",names(coef(get("fitsur_met"))))]
cdf <- ecdf(as.numeric(coef))
plot(cdf,col="blue")

