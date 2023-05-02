if(Sys.info()["sysname"]=='Windows') Sys.setlocale("LC_TIME","English")

rm(list = ls())
if (Sys.info()["sysname"]=='Windows')  setwd('C:/Users/jpber/OneDrive/Documents/Codigo_compartido_Melo/Natural_disasters_CDS')
#if (Sys.info()["sysname"]!='Windows')  setwd('/Users/lumelo/archivos/Climate-Change-and-Financial-Stability/Github/Climate-Change-and-Financial-Stability')

cat("\014")

# Se establece el directorio para los datos
Dir <- paste0(getwd(),'/BasesCDS/') # Se supone que el subdirectorio <Bases> existe.


# Librerias ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(stringr)
library(ggplot2)
library(xts)
library(stats)

# Datos -------------------------------------------------------------------

paises <- c('Austria','Belgium','Croatia','CzechRepublic','France','Germany','Greece','Ireland','Italy','Netherlands','Norway','Poland',
            'Portugal','Russia','Slovenia','Spain') #<<<--- Paises de los que se tienen indices, falta Rumania

# Para cada pais que se encuentra en <paises>, vamos a tener una base de datos del indice bursatil que le corresponde. Cada una tiene 7 columnas:
# la primera, <Date>, corresponde a los dias en los cuales tenemos datos para el indice; la segunda, <Price>, corresponde a los precios de cierre 
# para cada dia; la tercera, <Open>, corresponde a los precios de apertura para cada dia; la cuarta, <High>, corresponde al mayor precio registrado
# en el dia; la quinta, <Low>, corresponde al menor precio registrado en el dia; la sexta, <Vol.>, corresponde al volumen de acciones que se tranzaron;
# la septima, <Change%>, corresponde al cambio porcentual en el precio de cierre. 

xts_list     <- list() 
for (country in paises) {
  # Genera el nombre del archivo csv, siguiendo el Directorio especificado, añadiendole /Stocks_ country.csv
  csv_file <- paste0(Dir,"Stocks_", country, ".csv")
  # Genera un archivo csv para el país <country>
  csv      <- read.csv(csv_file)
  colnames <- names(csv)
  
  # Loop que corre por todas las columnas del archivo <csv> menos la primera (ya que es un dia), retirandole las
  # comas que podrían generar problemas para reconocerlo como formato número
  for (colname in colnames[2:length(colnames)]) {
    csv[, colname] <- as.numeric(gsub(",","",csv[, colname]))
  } # Muestra warning() ya que hay una columna que contiene caracteres "M" 
  
  csv$Date <- as.Date(csv$Date, "%m/%d/%Y")
  
  # Generar la lista de los xts, solamente de la columna <Price>, teniendo en cuenta los índices incluidos en <Date>
  xts_list[[country]] <- xts(csv$Price, csv$Date)
}

# Generar una base de datos que junte todos los indices bursatiles en formato xts.
# Las matriz <base_test> tiene las siguientes dimensiones:
#     columnas: numero de <paises> analizados
#     filas   : total de dias en el que hay al menos un dato para cualquier indice
base_test <- do.call(merge,xts_list)
# Cambiar nombres de las columnas por los nombres de los <paises>
colnames(base_test) <- paises

# Generar un vector de fechas en los que solo se tiene valores para n < <min.dias.stock> mercados.

min.dias.stock <- ceiling((length(paises)/2)) #<<<--- minimo numero de <paises> en donde debe haber datos
navalues = c()
for (i in 1:nrow(base_test)) {
  row <- base_test[i, ]
  if (sum(is.na(row))>=length(paises)- min.dias.stock){
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
for(i in 2:length(paises)) 
  base <- merge(base,na.approx(base_test[,i]))
# Asegurarnos que quede con los nombres de columnas correctos
colnames(base) <- colnames(base_test)

# Eliminar las filas en las que hay valores NA. Las dimensiones de <base_precios> son:
#       columnas: las mismas que <base_test>
#       filas   : dependiendo de los valores en <base>, <base_precios> puede tener o el mismo numero de filas que <base> o una fila menos, 
#                 o dos filas menos.
base_precios <- base[complete.cases(base),]

# Genera la base de retornos. Se coloca [2:nrow(base_precios)] porque de no hacerlo toda la primera fila serian valores
# NA, por lo que se perdio un dato. El operador diff se realizo para toda la <base_precios>,pero el <[2:nrow(base_precios)]>
# lo que hace es solamente quitar la primera fila de NA.
# La dimension de <base_retornos> es la siguiente:
#       columnas: las mismas que <base_test> y las demás bases anteriores
#       filas   : una fila menos que <base_precios>
base_retornos <- 100*diff(log(base_precios))[2:nrow(base_precios),]

# Otra variable importante es la media de los promedios moviles, por lo cual se genera el promedio movil de cada
# retorno de orden 22, ya que hay aproximadamente 22 dias para cada mes, usando la funcion <moving_average>.

orden <- 22 #<<<---  Orden del promedio movil del indice global de largo plazo de los indices accionarios 
# mov_average_base <- moving_average(base_retornos,orden)
# La siguiente funcion <apply> genera una base de datos (<mov_average_base>). Las dimensiones de la matriz son:
#       columnas: las mismas que las demas bases anteriores
#       filas   : aquellas de <base_retornos> menos <orden>-1
mov_average_base <- apply(base_retornos, MARGIN=2, FUN=rollmean, k=orden, align="right")

# Media de los anteriores promedios. Genera vector con una longitud igual al numero de filas de <mov_average_base>
mean_mov_average = apply(mov_average_base, MARGIN=1, FUN=mean)
mean_mov_average = xts(mean_mov_average, order.by=index(base_retornos)[-c(1:(orden-1))]) #-> XTS
colnames(mean_mov_average) = c("Mean_Returns_Moving_Averages") # Nombre de la variable

# Lectura base de datos ---------------------------------------------------

emdat_public <- read_excel(paste0(Dir,"EMDAT_PUBLIC.xlsx"),sheet="emdat data")
# Se seleccionan las columnas de interes:
#  <Disaster Subgroup>: uno de los cinco subgrupos de desastres: meteorologico, geofisico, hidrologico, climatologico, extraterrestrial
#  <Disaster Type>: tipo de desastre
#  <Disaster Subtype>: subtipo del desastre
#  <Country>: pais donde sucedio el desastre
#  <Start Year>: año en que inicio el desastre
#  <Start Month>: mes en que inicio el desastre
#  <Start Day>: dia en que inicio el desastre
#  <End Year>: año en que termino el desastre
#  <End Month>: mes en que termino el desastre
#  <End Day>: dia en que termino el desastre
#  <Total Deaths>: total de muertes 
#  <No Injured>: numero de heridos
#  <No Affected>: numero de afectados
#  <No Homeless>: numero de personas cuya casa fue destruida
#  <Total Affected>: total de afectados
#  <Damages>: total de daños totales en miles de dolares ajustados al 2021

emdat_base <- emdat_public %>% 
  dplyr::select('Disaster Subgroup','Disaster Type','Disaster Subtype','Country','Start Year','Start Month','Start Day','End Year','End Month',
                'End Day','Total Deaths','No Injured','No Affected','No Homeless','Total Affected',
                Damages = "Total Damages, Adjusted ('000 US$)")

# Seleccion paises --------------------------------------------------------

countries    <- c('Austria','Belgium','Croatia','Czech Republic (the)','France','Germany','Greece','Ireland','Italy','Netherlands (the)',
                  'Norway','Poland','Portugal','Romania','Russian Federation (the)','Slovenia','Spain') #<<<--- 17 paises en el paper
# segun el nombre en base original
# Se filtra la base de datos principal para que contenga solamente los paises en <countries>
emdat_base_filtered <- emdat_base %>% 
  dplyr::filter(Country %in% countries)
# Se cambian los nombres de algunos paises para mejor manejo de la base de datos
emdat_base_filtered <- emdat_base_filtered %>%
  mutate(Country = case_when(
    Country == "Netherlands (the)" ~ "Netherlands",
    Country == "Czech Republic (the)" ~ "CzechRepublic",
    Country == "Russian Federation (the)" ~ "Russia",
    TRUE ~ Country # TRUE ~ Country permite que los demas valores de <Country> se mantengan
  ))

# Seleccion de desastres --------------------------------------------------

# En el paper solamente tienen 92 desastres despues de comparar con respecto al PIB de cada pais. Por el momento selecciono los desastres
# con mayores daños por cada pais segun el paper. Por ejemplo: en la muestra solo tienen 4 desastres para Austria, por lo cual selecciono
# para Austria los 4 desastres con mayores daños, y asi con todos los paises.
# Esta con <if(1)> ya que es necesario cambiar el codigo para que seleccione los mismos 92 desastres que el paper
if(1){
  countries_corrected <- sort(unique(emdat_base_filtered$Country)) #<<<-- nombres de los paises en la base
  n <- c(4,4,3,1,6,12,3,1,22,3,1,4,1,1,15,3,8) #<<<--- desastres en la muestra por cada pais
  emdat_topn <- NULL
  for(i in seq_along(countries_corrected)){
    base <- emdat_base_filtered %>% 
      filter(Country == countries_corrected[i]) %>% 
      arrange(desc(Damages)) %>% 
      slice(1:n[i])
    if(is.null(emdat_topn)){
      emdat_topn <- base
    }else{
      emdat_topn <- dplyr::bind_rows(emdat_topn,base)
    }
  }
}

# Estadistica descriptiva -------------------------------------------------

# Se genera un conteo de desastres por cada año
emdat_topn_count <- emdat_topn %>%
  group_by(`Start Year`) %>%
  summarise(count = n())

# Se grafican los conteos
ggplot(emdat_topn_count, aes(x = `Start Year`, y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Year") +
  ylab("Count") +
  scale_x_continuous(breaks = emdat_topn_count$`Start Year`) +
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) 

# Generacion columna de fecha ---------------------------------------------

# Primero se ve que algunas de las fechas tienen NA en el dia, por lo cual se asume que es el primer dia del mes, pero colocando una dummy
# donde es igual a 1 si se asumio el dia y 0 en otro caso

if(sum(is.na(emdat_topn$`Start Day`))!=0){
  warning("Hay dias faltantes en la base de datos! Se va a asumir que el dia de inicio del desastre es el primero del mes")
  emdat_topn <- emdat_topn %>%
    mutate(na_start = ifelse(is.na(`Start Day`),1,0))
  emdat_topn <- emdat_topn %>% 
    mutate(`Start Day`=replace_na(`Start Day`,1)) # <replace_na> se utiliza para reemplazar los valores <NA> por <1>
}

# La base contiene año, mes y dia, pero no una fecha, por lo cual generamos la columna de fecha de inicio

emdat_topn <- emdat_topn %>% 
  unite(`Start Date`, c(`Start Year`, `Start Month`, `Start Day`), sep = "-",remove=FALSE) %>% 
  mutate(`Start Date` = as.Date(`Start Date`))

# Dataframe eventos -------------------------------------------------------

# Por simplicidad, se genera un dataframe solamente con la fecha y el pais del evento

eventos <- emdat_topn %>% 
  dplyr::select(Country,`Start Date`)

# Eliminar los eventos en Rumania ya que no se tiene el indice todavia, esta con <if(1)> ya que al momento de encontrar el indice se 
# puede borrar. 
if(1){
  eventos <- eventos %>% 
    dplyr::filter(Country != "Romania")
}

# Regresion estimation window ---------------------------------------------

# Por cada evento se hace una regresion OLS de [-150,-1] dias para obtener alfa, beta 

estimation_start     <- 150 #<<<--- dias antes del evento para comenzar la estimacion
estimation_end       <- 1   #<<<--- dias antes del evento para finalizar la estimacion
max_abnormal_returns <- 15  #<<<--- dias maximos despues del evento para calcular retorno anormal
days_to_be_evaluated <- 5   #<<<--- dias despues del evento a ser evaluados
all_events_list      <- list() # lista que contendra todos los xts + errores estandar

## Falta volverlo una funcion que tome como argumentos un dataframe de eventos y una base de retornos, mas los argumentos de arriba
for(i in 1:nrow(eventos)){
  # Primero se encuentra a que dato le corresponde el dia del evento, y el dia final de la ventana de evento es el dia del evento
  # mas <max_abnormal_returns>
  event_list <- list()
  row  <- eventos[i,]
  pais <- as.character(row[1])
  suppressWarnings({
    for(j in 0:days_to_be_evaluated){
      if((eventos[i,2]+j) %in% index(base_retornos[,pais])){ # Arreglar porque las bases estan hasta 2019, falta descargar a 2021
        event_start_index <- which(index(base_retornos[,pais])==eventos[i,2]+j)
        break
      }
    }
    #event_start_index <- which(index(base_retornos[,pais]) == eventos[i,2]+1)
    event_end_index   <- index(base_retornos[,pais])[event_start_index + max_abnormal_returns]
  })
  
  # Se selecciona la ventana de estimacion tanto para el indice como para el promedio movil
  estimation_xts         <- base_retornos[,pais][(event_start_index-estimation_start):(event_start_index-estimation_end),]
  estimation_start_index <- index(estimation_xts)[1]
  estimation_end_index   <- index(estimation_xts)[length(estimation_xts)]
  mean_mov_xts <- mean_mov_average[index(mean_mov_average)>=estimation_start_index & index(mean_mov_average) <= estimation_end_index]
  data <- cbind(estimation_xts,mean_mov_xts)
  
  # Regresion por OLS
  model          <- lm(data[,pais] ~ data$Mean_Returns_Moving_Averages)
  alpha          <- model$coefficients[["(Intercept)"]] 
  beta           <- model$coefficients[["data$Mean_Returns_Moving_Averages"]]
  standard_error <- sd(residuals(model))
  
  observed       <- base_retornos[,pais][index(base_retornos[,pais])>=estimation_start_index & index(base_retornos[,pais])<=event_end_index]
  predicted      <- alpha + beta*(mean_mov_average[index(mean_mov_average)>=estimation_start_index & index(mean_mov_average)<=event_end_index]) 
  abnormal       <- observed - predicted
  df             <- merge(observed,predicted,abnormal)
  colnames(df)   <- c('Observed','Predicted','Abnormal')
  event_list[["Dataframe"]]       <- df 
  event_list[["Standard_Error"]]  <- standard_error 
  all_events_list[[as.character(i)]] <- event_list
}


# Wilcoxon --------------------------------------------------------------

length_car_window <- 1 #<<<--- cual es la ventana a la cual se quiere calcular el CAR (por ejemplo 5 significa [0,+5], donde 0 es el dia del evento)
# En cada dataframe de <all_events_list>, se tiene una columna <Abnormal>. A partir del elemento <estimation_start>+1 se tienen los retornos
# anormales. El fin de la ventana de evento esta dada por <car_window>
# El siguiente codigo esta con <if(1)> porque falta volverlo una funcion 
if(1){
  car_window_begin <- estimation_start + 1 
  # Se crea un vector para guardar los CAR
  all_car <- c()
  for(element in all_events_list){
    car <- sum(element$Dataframe$Abnormal[car_window_begin:(car_window_begin+length_car_window)])
    all_car <- c(all_car,car)
  }
  # La media del vector es el CAAR
  caar <- sum(all_car)/length(all_car)
  
  # Se genera un dataframe para poder realizar el ordenamiento de los car
  df_car <- data.frame("car"=all_car,"magnitude"=abs(all_car),"sign"=sign(all_car))
  df_car <- df_car %>% 
    mutate(magnitude=ifelse(magnitude==0,NA,magnitude)) ## Se coloca NA si la magnitud es 0
  df_car <- df_car %>% 
    mutate(rank = rank(magnitude))
  
  # Se suman los rangos de car positivos
  rank_sum <- df_car %>% 
    group_by(sign) %>%
    summarize(sum = sum(rank)) %>%
    arrange(sum)
  positive_rank_sum <- rank_sum$sum[rank_sum$sign=="1"]
  
  # Revisar significancia del estadistico
  significance <- ""
  ## Revisa para cada nivel de significancia si el valor en statistics es lo suficientemente extremo para rechazar H_0
  # qsignrank da la funcion cuantil.
  N <- nrow(eventos)
  significance[positive_rank_sum >= stats::qsignrank(1 - 0.1, n = N) | 
                 positive_rank_sum <= N * (N + 1)/2 - stats::qsignrank(1 - 0.1/2, 
                                                                       n = N)] <- "*"
  significance[positive_rank_sum >= stats::qsignrank(1 - 0.05, n = N) | 
                 positive_rank_sum <= N * (N + 1)/2 - stats::qsignrank(1 - 0.05/2, 
                                                                       n = N)] <- "**"
  significance[positive_rank_sum >= stats::qsignrank(1 - 0.01, n = N) | 
                 positive_rank_sum <= N * (N + 1)/2 - stats::qsignrank(1 - 0.01/2, 
                                                                       n = N)] <- "***"
  resultado <- data.frame("Wilcoxon_statistic" = positive_rank_sum,"Significancia" = significance)
}