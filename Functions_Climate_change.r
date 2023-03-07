#---------------------------------- 1. read_csv ------------------------------------#
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

#---------------------------------- 2. moving_average  ------------------------------------#
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


#---------------------------------- 3. muestra_paper  ------------------------------------#
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


#---------------------------------- 4. chow_lin ------------------------------------#
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

#---------------------------------- 5. series_list_function  ------------------------------------#
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

#---------------------------------- 6. days  ------------------------------------#
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


#---------------------------------- 7. create_dummies  ------------------------------------#
# La siguiente función genera las dummies t_0, t_1, t_2, t_3, t_4 y D para cada tipo de desastre
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- excel_file: un archivo excel que contiene los dias correspondientes a las dummies
# ----Argumentos de salida  ----#
#-- xts_dummies_list: una lista con las dummies de todos los tipos de desastres
#---------------------------------------------------------------------------------------#

create_dummies <- function(excel_file){
  #Lee el nombre de las hojas del archivo
  sheet_names <- excel_sheets(excel_file)
  xts_dummies_list <- list()
  
  #Loop para todas las hojas
  for(sheet_name in sheet_names) {
    #lee la hoja especifica
    current_sheet    <- read.xlsx(excel_file, sheet = sheet_name, detectDates = TRUE)
    #la columna t0 sale del nombre del archivo excel
    dummies_t0       <- current_sheet$t0
    #Primero generamos un vector de 0 de longitud nrow(Retornos), al que se le añadira 1 dependiendo del dia
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
    
    #Por otro lado, para formar las dummies t_1, t_2, t_3 y t_4 se usan rezagos de t_0, ya que en esta funcion
    #se asume que el n-paso adelante del evento es igual al n-ésimo día hábil después de t_0.
    t_1 <- lag(t_0,1)
    t_2 <- lag(t_0,2)
    t_3 <- lag(t_0,3)
    t_4 <- lag(t_0,4)
    
    #Se genera un dataframe de las dummies
    dummies_df       <- cbind(t_0,t_1,t_2,t_3,t_4)
    #Se reemplaza los valores NA al inicio del dataframe por cero, ya que no se tienen en cuenta eventos anteriores
    dummies_df_c     <- ifelse(is.na(dummies_df),0,dummies_df)
    
    #se genera la dummy D, la cual es 0 si en el dia i tanto t_0, t_1, t_2, t_3 y t_4 son 0, y 1 en otro caso
    
    D <- c()
    for(i in 1:nrow(dummies_df_c)){
      if(sum(dummies_df_c[i,]) == 0){
        D <- c(D,0)
      }else if(sum(dummies_df_c[i,]) != 0){
        D <- c(D,1) 
      }
    }
    
    #Generamos el objeto xts
    dummies_complete <- cbind(dummies_df_c, D)  
    dummies_xts <- xts(dummies_complete,order.by=index(Retornos))
    
    #Crear objetos distintos para cada hoja
    xts_name <- paste0(sheet_name, "_dummies_xts}","test")
    xts_dummies_list[[xts_name]] <- dummies_xts
  }
  return(xts_dummies_list)
}

#---------------------------------------------------------------------------------------#


#---------------------------------- 8. interaction_function  ------------------------------------#
# La siguiente función se encarga de generar el vector de interacción entre la dummy D y el promedio movil
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- obj: un objeto xts
#-- La función asume que "Promedio_movil" existe, vector que se tiene arriba en el codigo
# ----Argumentos de salida  ----#
#-- interaction_xts: objeto xts de la interacción entre D y el promedio movil
#---------------------------------------------------------------------------------------#

interaction_function <- function(obj){
  interaction <- c()
  for(i in 1:nrow(obj)){
    interaction <- c(interaction, as.numeric(Promedio_movil[i])*as.numeric((obj[,ncol(obj)])[i]))
  }
  interaction_xts <- xts(interaction, order.by = index(obj))
  return(interaction_xts)
}

#---------------------------------------------------------------------------------------#


#---------------------------------- 9. arma_seleccion_df  ------------------------------------#
# La siguiente función se encarga de tomar una serie de tiempo, un rezago p y q maximos del modelo ARMA(p,q)
# y crear un dataframe de todos los posibles modelos con el criterio de Akaike y bayesiano.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- object: la serie de tiempo a al cual se le quiere encontrar el modelo
#-- AR.m  : rezago máximo de la parte autorregresiva
#-- Ma.m  : rezago máximo de la parte de promedio movil
#-- d     : orden de diferenciación
#-- bool  : booleano que indica si realizar la estimación arima con constante
#-- metodo: método por el cual se hará la estimación ARIMA (existe CS, ML y CSS-ML)
# ----Argumentos de salida  ----#
#-- dataframe de todos los modelos ARMA(p,q) de las posibles combinaciones (0,AR.m)x(0,MA.m) con su respectivo
#-- criterio de Akaike y bayesiano
#---------------------------------------------------------------------------------------#

arma_seleccion_df = function(object, AR.m, MA.m, d, bool, metodo){
  index = 1
  df = data.frame(p = double(), d = double(), q = double(), AIC = double(), BIC = double())
  for (p in 0:AR.m) {
    for (q in 0:MA.m)  {
      fitp <- arima(object, order = c(p, d, q), include.mean = bool, 
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


#---------------------------------------------------------------------------------------#

#---------------------------------- 10. lag_function  ------------------------------------#
# La siguiente función se encarga de encontrar los rezagos optimos para cada pais utilizando el
# criterio de Akaike, utilizando la función 9. Además, la función tambien va a generar los rezagos y 
# agregarlos a un dataframe
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- country: caracteres indicando un pais
# ----Argumentos de salida  ----#
#-- lags_reduced: objeto xts con los p-rezagos para cada país.
#---------------------------------------------------------------------------------------#

lag_function <- function(country){
  
  #Utilizamos la funcion arma_seleccion_df para obtener el rezago para incluir en la ecuacion segun el 
  #criterio de Akaike. Como queremos ver AR(p), MA.m = 0, y como todos los retornos son estacionarios, 
  #entonces d =0.
  mod <- arma_seleccion_df(object=base_retornos[,country], AR.m=20, MA.m=0, d=0, bool=TRUE, metodo="CSS")
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


#---------------------------------------------------------------------------------------#


#---------------------------------- 11. model_equation  ------------------------------------#
# La siguiente función va a generar una ecuación para el país country y teniendo en cuenta las variables 
# exógenas exo. Lo anterior dado que vamos a estimar por el metodo SUR, el cual necesitara las 27 
# ecuaciones siguiendo el paper de Pagnottoni.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- country: caracteres indicando un pais
#-- exo    : conjunto de variables exogenas, especificamente las dummies. En el primer ejemplo son dummies 
#            por tipo de desastre
# ----Argumentos de salida  ----#
#-- eq: ecuación de variable dependiente ~ regresoras.
#---------------------------------------------------------------------------------------#

model_equation <- function(country,exo){
  
  #Busca el dataframe con los rezagos
  lags_name <- paste0("lags_reduced_", country)
  lags_df <- get(lags_name)
  
  #Busca las variables para el gdp y el fdi
  gdp_variable <- paste("gdp",country,sep="_")
  fdi_variable <- paste("gfdi",country,sep="_")
  
  #Genera la ecuacion n.4 por el país country
  eq  <- base_datos[,country]  ~ base_datos[,"Mean_Returns_Moving_Averages"] +exo + base_datos[,gdp_variable] +
    base_datos[,fdi_variable] + lags_df
  return(eq)
}


#---------------------------------------------------------------------------------------#


#---------------------------------- 11. dens  ------------------------------------#
# La siguiente función va a generar una ecuación para el país country y teniendo en cuenta las variables 
# exógenas exo. Lo anterior dado que vamos a estimar por el metodo SUR, el cual necesitara las 27 
# ecuaciones siguiendo el paper de Pagnottoni.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- fit : modelo estimado al que se le puede sacar coeficientes
#-- step: pasos adelante despues del evento
# ----Argumentos de salida  ----#
#-- densidad: densidad kernel de los coeficientes del modelo estimado
#---------------------------------------------------------------------------------------#

dens <- function(fit, step){
  coefs <- coef(get(fit))
  interest_indices <- grep(step,names(coefs))
  interest_coefficients <- coefs[interest_indices]
  densidad <- density(as.numeric(interest_coefficients))
  return(densidad)
}

#---------------------------------------------------------------------------------------#


#---------------------------------- 12. densidad_CAR ------------------------------------#
# La siguiente función va a generar la densidad de los retornos acumulados. 
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- x: toma un vector de coeficentes
#-- countries: vector de paises
# ----Argumentos de salida  ----#
#-- densidad_c: densidad de los retornos acumulados 
#---------------------------------------------------------------------------------------#

densidad_CAR <- function(x,countries){
  CAR <- c()
  for(country in countries){
    #Mira dentro el vector de coeficientes cuales inician con country, es decir el pais y luego los suma
    start_with <- paste0("^",country)
    sum_of_coefficients <- sum(as.numeric(x[grep(start_with, names(x))]))
    #Agrega la suma (el CAR) en un vector, para luego hallar la densidad
    CAR <- c(CAR,sum_of_coefficients)
  }
  densidad_C <- density(CAR)
  return(densidad_C)
}

#---------------------------------------------------------------------------------------#



#---------------------------------- 12. grafico  ------------------------------------#
# La siguiente función va a generar la densidad de los retornos acumulados. 
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- vector: un vector especifico que incluye el nombre del gráfico más los objetos a graficar
#-- labels: leyenda del grafico
#-- colors: colores de las lineas del gráfico
# ----Argumentos de salida  ----#
#-- NA. No retorna argumentos, más bien un gráfico que incluye las 5 densidades (biologico, climatológico
#-- hidrologico, geologico, meteorologico).
#---------------------------------------------------------------------------------------#

grafico <- function(vector,labels, colors){
  maximos <- c()
  for(i in vector[2:length(vector)]){
    maximos <- c(maximos, max(get(i)$y))
  }
  limite_y          <-  max(maximos)
  
  x11()
  plot(get(vector[2]), main = vector[1], col = colors[1],lwd=2,ylim=c(0,limite_y))
  for(i in 3:length(vector)){
    lines(get(vector[i]),col=colors[i-1],lwd=2)
  }
  legend("topright",legend = labels,col = colors, lwd = 2)
}
#---------------------------------------------------------------------------------------#


