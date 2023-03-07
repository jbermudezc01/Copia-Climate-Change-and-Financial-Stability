#----------------------------------  read_csv ------------------------------------#
# Se crea una funcion que va a leer los csv para los indices bursatiles, genera una base de datos de los indices 
# bursatiles en formato xts. 
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- dir      : el directorio en donde se encuentran las bases de los países
#-- countries: lista que denota los paises de los cuales se cuenta el índice bursátil
# ----Argumentos de salida  ----#
#-- base: es una base de datos en formato xts que incluye todos los indices bursátiles para los países en countries
#---------------------------------------------------------------------------------------#
read_csv <- function(dir,countries) {
  # Se forma una lista vacía, en la cual se irán añadiendo los elementos xts de los indices por cada país
  xts_list     <- list() 
  for (country in countries) {
    # generar el nombre del archivo csv, siguiendo el directorio especificado, añadiendole /Stocks_ country.csv
    csv_file <- paste0(dir,"Stocks_", country, ".csv")
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
  base <- do.call(merge,xts_list)
  return(base)
}
#------------------------------------------------------------------------------------#

#----------------------------------  moving_average  ------------------------------------#
# Se crea una función que toma como argumento una base de datos, para la cual genera para cada columna
# el promedio movil de orden k.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- x: el argumento denota una base de datos
#-- k: el argumento denota el orden del promedio móvil
# ----Argumentos de salida  ----#
#-- mov_average_base: base de datos con el promedio movil de orden k de cada una de las columnas de la base x
#---------------------------------------------------------------------------------------#

moving_average <- function(x,k){
  mov_average_list <- list()
  for(column in 1:ncol(x)){
    rolling_average <- rollmean(x[, column], k, align = "right")
    mov_average_list[[length(mov_average_list)+1]] <- rolling_average
  }
  mov_average_base <- do.call(merge, mov_average_list)
  return(mov_average_base)
}
#---------------------------------------------------------------------------------------#


#----------------------------------  muestra_paper  ------------------------------------#
# Se crea una función que reduzca la base de datos dependiendo del indice. Esta funcion toma un día en 
# específico y tomará desde ese día en adelante para generar la base recortada.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- obj: el argumento denota una base de datos
#-- t: el argumento denota un día en específico que hace parte del índice de la base de datos obj
# ----Argumentos de salida  ----#
#-- obj: la misma base de datos de entrada pero las filas irán desde aquella que corresponda al dia t 
#---------------------------------------------------------------------------------------#

muestra_paper <- function(obj,t){
  indice           <- index(obj)
  base_start       <- which(indice == t)
  obj              <- obj[base_start:nrow(obj),]
  return(obj)
}

#---------------------------------------------------------------------------------------#


#----------------------------------  chow_lin ------------------------------------#
# Se crea una función que genere la desagregacion temporal de una base de datos siguiendo el método de Chow-lin.
# Tanto la matriz de varianzas covarianzas como el procedimiento para realizar la desagregacion temporal fueron
# extraidas del articulo de Hurtado y Melo (2015).
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- time_Series_list: lista con las series de tiempo a las cuales se les quiere realizar la desagregacion temporal
#-- c               : matriz de agregacion
#-- w               : vector constante de valores 1
#-- alpha           : valor de alpha, que corresponde al valor del coeficiente del AR(1)
#-- var_covar       : matriz de varianzas y covarianzas
# ----Argumentos de salida  ----#
#-- return_base: base de datos con la desagregacion temporal de cada una de los elementos en la lista time_Series_list
#---------------------------------------------------------------------------------------#

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
  return_base <- do.call(merge,return_list)
  return(return_base)
} 

#---------------------------------------------------------------------------------------#

#----------------------------------  series_list_function  ------------------------------------#
# Teniendo en cuenta que la función de chow_lin requiere de primer argumento una lista, se necesita una función
# que genere una lista teniendo en cuenta los objetos xts
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- ts1: el argumento denota una base de datos, puede ser en formato xts
# ----Argumentos de salida  ----#
#-- series_list: una lista que contiene los datos de cada columna de la base de datos
#---------------------------------------------------------------------------------------#

series_list_function <- function(ts1){
  series_list <- list()
  for(country in colnames(ts1)){
    func_series <- as.vector(ts1[,country])
    series_list[[tolower(country)]] <- func_series
  }
  return(series_list)
}

#---------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------#

#----------------------------------  days  ------------------------------------#
# La siguiente función se encarga de generar la matriz de agregación trimestral, añadiendo uno a los dias que 
# corresponden a cada trimestre
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- x: un entero
#-- m: una matriz de agregación que antes de la función solamente contiene ceros
#-- months: vector caracter tipo "yyyy-mm" de los meses que hacen parte de los trimestres a desagregar
# ----Argumentos de salida  ----#
#-- m: la matriz de agregación ya con el valor de 1 en los días que pertenezcan a cierto trimestre
#---------------------------------------------------------------------------------------#

days <- function(x, m, months){
  first_month <- months[3*x+1]
  second_month <- months[3*x+2]
  third_month <- months[3*x+3]
  for(date in dates){
    if(substr(date,1,7)==first_month |substr(date,1,7)==second_month|substr(date,1,7)==third_month){
      pos <- which(dates == date)
      m[x+1,pos] <- 1
    }
  }
  return(m)
}
#---------------------------------------------------------------------------------------#
