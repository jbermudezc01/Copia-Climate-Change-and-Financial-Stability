#--- Funcion anterior que leia los archivos csv, pero los argumentos no estaban explicitos, por lo que se prescinde de la funcion ---#
if(0){
  #---------------------------------- 1. read_csv ------------------------------------#
  # Lee los archivos csv para los indices bursatiles, genera una base de datos de los indices 
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
}

#--- Funcion anterior que generaba el promedio movil, no es necesaria ya que con apply se hace lo mismo --#
if(0){
  #---------------------------------- 2. moving_average  ------------------------------------#
  # Toma como argumento una base de datos, para la cual genera para cada columna
  # el promedio movil de orden k.
  #---------------------------------------------------------------------------------------#
  # ----Argumentos de entrada ----#
  #-- x: Nombre de la base de datos
  #-- k: Orden del promedio móvil
  # ----Argumentos de salida  ----#
  #-- mov_average_base: base de datos con el promedio movil de orden <k> de cada una de las columnas de la base <x>
  #---------------------------------------------------------------------------------------#
  moving_average <- function(x,k){
    mov_average_list <- list()
    for(column in 1:ncol(x)){
      rolling_average <- rollmean(x=x[, column], k=k, align="right")
      mov_average_list[[length(mov_average_list)+1]] <- rolling_average
    }
    mov_average_base <- do.call(merge, mov_average_list)
    return(mov_average_base)
  }
  #---------------------------------------------------------------------------------------#
}

#--- Funcion anterior de reducir el indice de una serie, fue mejorada por una propiedad de xts, donde solo tocaba especificar la fecha de inicio ---#
if(0){
  #---------------------------------- 3. muestra_paper  ------------------------------------#
  # Reduce la base de datos dependiendo del indice. Esta funcion toma un día en 
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
}

#---------------------------------- 4. chow_lin ------------------------------------#
# Genera la desagregacion temporal de una base de datos siguiendo el método de Chow-lin.
# Tanto la matriz de varianzas covarianzas como el procedimiento para realizar la desagregacion temporal fueron
# extraidas del articulo de Hurtado y Melo (2015).
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- time_Series_list: lista con las series de tiempo a las cuales se les quiere realizar la desagregacion temporal
#-- c               : matriz de agregacion
#-- w               : vector constante de valores 1
#-- var_covar       : matriz de varianzas y covarianzas
#-- base_indice     : base de datos en xts que dara el indice para la base de datos desagregada
# ----Argumentos de salida  ----#
#-- return_base: base de datos con la desagregacion temporal de cada una de los elementos en la lista time_Series_list
#---------------------------------------------------------------------------------------#
chow_lin <- function(time_Series_list, c, w, var_covar,base_indice){
  return_list <- list()
  for(series in time_Series_list){
    CW        <- c%*%w
    #model_gls <- gls(series ~ CW - 1, correlation = corAR1(value = alpha, fixed = TRUE)) 
    #sigma_sq  <- (model_gls$sigma)^2
    #V         <- (sigma_sq/(1-alpha^2))*var_covar # no se requiere <sigma_sq>, ya que no se necesita, se cancela  
    CVC       <- solve(c%*%(var_covar%*%t(c)))
    beta      <- (solve(t(CW)%*%(CVC%*%CW))%*%t(CW))%*%(CVC%*%series)
    y         <- w%*%beta + ((var_covar%*%t(c))%*%CVC)%*%(series-CW%*%beta)
    y          <- xts(y,order.by= index(base_indice)) 
    #gdp_growth <- diff(log(y))[2:nrow(y),]
    #return_list[[length(return_list)+1]] <- gdp_growth
    return_list[[length(return_list)+1]] <- y
  }
  return_base <- do.call(merge,return_list)
  return(return_base)
} 
#---------------------------------------------------------------------------------------#

#--- Funcion anterior que generaba una lista de series de tiempo, arreglada con as.list()---#
if(0){
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
}


#---------------------------------- 6. days  ------------------------------------#
# Genera la matriz de agregación trimestral, anhadiendo uno a los dias que corresponden a cada trimestre
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- x           : un entero que corresponde al trimestre (menos uno) tenido en cuenta
#-- m           : matriz de agregación (inicializada con ceros)
#-- months      : vector caracter tipo "yyyy-mm" de los meses que hacen parte de los trimestres a desagregar 
#-- dates       : vector caracter que contiene todos los dias que están en cierta base de datos (high-freq)
#-- startstring : numero que indica el inicio de la string a comparar (default = 1) 
#-- endstring   : numero que indica el final de la string a comparar (default = 7, ya que de ese modo la fecha de cada día estara en formato "yyyy-mm")
# ----Argumentos de salida  ----#
#-- m: la matriz de agregación ya con el valor de 1 en los días que pertenezcan a cierto trimestre
#---------------------------------------------------------------------------------------#
days <- function(x, m, months, dates, startstring=1, endstring=7){
  first_month  <- months[3*x+1] #Primer mes del trimeste
  second_month <- months[3*x+2]
  third_month  <- months[3*x+3]
  for(date in dates){
    if(substr(date,startstring,endstring)==first_month |substr(date,startstring,endstring)==second_month|substr(date,startstring,endstring)==third_month){
      pos <- which(dates == date)
      m[x+1,pos] <- 1
    }
  }
  return(m)
}
#---------------------------------------------------------------------------------------#


#---------------------------------- 7. create_dummies  ------------------------------------#
# Genera las dummies t_0, t_1, t_2, t_3, t_4 y D para cada tipo de desastre (o país)
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- excel_file: un archivo excel que contiene los dias correspondientes a las dummies
#-- Retornos   : serie de tiempo, se usara su indice para generar las dummies
#-- no.rezagos : Numero de rezagos de la primera dummy (t_0) se interpretan como n dias despues del desastre
#-- first.calendar.days.tobe.evaluated: numero de dias despues del evento a ser evaluados
# ----Argumentos de salida  ----#
#-- xts_dummies: Array con las dummies de todos los tipos de desastres (o paises) de tres dominsiones, donde la primera es el tipo 
#                de desastre (o el pais), la segunda el indice de fechas y la tercera los pasos adelante del desastre (0,...,no.rezagos) 
#---------------------------------------------------------------------------------------#
create_dummies <- function(excel_file, Retornos, no.rezagos=4, first.calendar.days.tobe.evaluated = 10 ){
  #Lee el nombre de las hojas del archivo, cada hoja corresponde a un tipo de desastre
  sheet_names      <- excel_sheets(excel_file)
  xts_dummies      <- array(NA,dim=c(length(sheet_names), nrow(Retornos), no.rezagos+2), 
                            dimnames = list(sheet_names,as.character(index(Retornos)),c(paste0('t',0:no.rezagos),'D')))# +2: Por rezago 0 y por D
  #Loop para todas las hojas
  for(sheet_name in sheet_names) {
    #lee la hoja especifica
    current_sheet <- openxlsx::read.xlsx(excel_file, sheet = sheet_name, detectDates = TRUE)
    #Selecciona la columna t0 del archivo excel
    dummies_t0    <- current_sheet$t0
    #Se inicializa en ceros 
    t_0 <- c(rep(0,nrow(Retornos)))
    
    ##Para generar las dummies, se utiliza un loop que evalua si el dia del desastre esta dentro de indice de
    # retornos. Si el dia esta, establece 1 en la posicion de ese dia.
    # Si no se encuentra, evalua si el dia calendario siguiente está en la base de Retornos, si se encuentra establece 1
    # en ese dia. Así sucesivamente hasta encontrar el dia transable mas cercano al dia del desastre. 
    # El maximo no. de dias evaluados es <first.calendar.days.tobe.evaluated>
    for(i in 1:length(dummies_t0)){
      for(j in 0:first.calendar.days.tobe.evaluated){
        if((as.Date(dummies_t0[i])+j) %in% index(Retornos)){
          index_f <- which(index(Retornos) == (as.Date(dummies_t0[i])+j)) 
          t_0[index_f] <- 1
          break
        }
      }
    }
    
    #Por otro lado, para formar las dummies t_1, t_2, t_3 y t_4 se usan rezagos de t_0, ya que en esta funcion
    #se asume que el n-paso adelante del evento es igual al n-ésimo día hábil después de t_0.
    #t_1 <- dplyr::lag(t_0,1)
    #t_2 <- dplyr::lag(t_0,2)
    #t_3 <- dplyr::lag(t_0,3)
    #t_4 <- dplyr::lag(t_0,4)
    
    #Se genera una matriz de la forma cbind(t_0, t_1, t_2, t_3, ...), los valores de <NA> se reemplazan por <0>  
    dummies_lags = matrix(0, length(t_0) , no.rezagos+1, dimnames=list(as.character(index(Retornos)),paste0('t_',0:no.rezagos)) )
    for(Lags in 0:no.rezagos)
      dummies_lags[(1+Lags):length(t_0),Lags+1] = dplyr::lag(t_0, Lags)[(1+Lags):length(t_0)]
    
    #Se genera un dataframe de las dummies
    #dummies_df       <- cbind(t_0, t_1, t_2, t_3, t_4)
    #Se reemplaza los valores NA al inicio del dataframe por cero, ya que no se tienen en cuenta eventos anteriores
    #dummies_df_c     <- ifelse(is.na(dummies_df),0,dummies_df)
    
    #se genera la dummy D, la cual es 0 si en el dia i tanto t_0, t_1, t_2, t_3 y t_4 son 0, y 1 en otro caso
    #D <- c()
    #for(i in 1:nrow(dummies_df_c)){
    #  if(sum(dummies_df_c[i,]) == 0)        D <- c(D, 0)
    #  else if(sum(dummies_df_c[i,]) != 0)   D <- c(D, 1) 
    #}
    D = (rowSums(dummies_lags)!=0) + 0
    
    #Generamos el objeto xts
    dummies_complete <- cbind(dummies_lags, D)  
    dummies_xts      <- xts(dummies_complete,order.by=index(Retornos))
    
    #Crear objetos distintos para cada hoja
    #xts_name        <- paste0(sheet_name, "_dummies_xts")
    #xts_dummies_list[[xts_name]] <- dummies_xts
    xts_dummies[sheet_name,,] <- dummies_xts
  }
  return(xts_dummies)
}
#---------------------------------------------------------------------------------------#

#--- Funcion anterior que generaba las interacciones entre la dummy D y el promedio movil, arreglada usando matrices y *---#
if(0){
  #---------------------------------- 8. interaction_function  ------------------------------------#
  # Genera el vector de interacción entre la dummy D y el promedio movil
  #---------------------------------------------------------------------------------------#
  # ----Argumentos de entrada ----#
  #-- obj: un objeto xts
  #-- average: una columna con los promedios moviles
  #-- La función asume que "Promedio_movil" existe, vector que se tiene arriba en el codigo
  # ----Argumentos de salida  ----#
  #-- interaction_xts: objeto xts de la interacción entre D y el promedio movil
  #---------------------------------------------------------------------------------------#
  interaction_function <- function(obj,average){
    interaction <- c()
    for(i in 1:nrow(obj)){
      interaction <- c(interaction, as.numeric(average[i])*as.numeric((obj[,ncol(obj)])[i]))
    }
    interaction_xts <- xts(interaction, order.by = index(obj))
    return(interaction_xts)
  }
  #---------------------------------------------------------------------------------------#
}

#---------------------------9. arma_seleccion_df --------------------------------------------#
#---- Toma una serie de tiempo, un rezago p y q maximos del modelo ARMA(p,q)
# y crea un dataframe de todos los posibles modelos con el criterio de Akaike y bayesiano.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- object: la serie de tiempo a al cual se le quiere encontrar el modelo
#-- AR.m  : rezago máximo de la parte autorregresiva
#-- Ma.m  : rezago máximo de la parte de promedio movil
#-- d     : orden de diferenciación
#-- bool  : booleano que indica si realizar la estimación arima con constante
#-- metodo: método por el cual se hará la estimación ARIMA (existe CS, ML y CSS-ML)
# ----Argumentos de salida  ----#
#-- df : dataframe donde cada fila representa los modelos ARIMA estimados. Las variables 
#--      del dataframe son: orden del polinomino AR (p), orden de integracion (d),
#--      orden del polinomio MA (q), criterio de Akaike (AIC) y Bayesiano (BIC)
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
# Encuentra los rezagos optimos para cada pais utilizando el criterio de Akaike, utilizando la función arma_seleccion_df.
# Además, la función tambien va a generar los rezagos y agregarlos a un dataframe. 
# Como tiene una función adentro, lag_function hereda los parámetros de arma_seleccion_df.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- base_niveles: base a la cual se le sacaran los rezagos
#-- ind         : caracteres indicando un indice bursatil
#-- AR.m        : rezago máximo de la parte autorregresiva
#-- Ma.m        : rezago máximo de la parte de promedio movil
#-- d           : orden de diferenciación
#-- bool        : booleano que indica si realizar la estimación arima con constante (el default es TRUE)
#-- metodo      : metodo por el cual se hará la estimación ARIMA (existe CS, ML y CSS-ML) (el default es CSS)
#-- dia.inicial : sirve para seleccionar el dia desde el cual debe empezar la base de rezagos 
#                 (el default es el mismo de las otras bases, dia.inicial)
# ----Argumentos de salida  ----#
#-- lags_reduced: objeto xts con los p-rezagos para cada país. La columna n de este objeto es el n-rezago de la serie original
#---------------------------------------------------------------------------------------#
lag_function <- function(base_niveles,ind,AR.m,MA.m,d,bool=TRUE,metodo="CSS",dia.inicial = dia.inicial)
{
  
  #Utilizamos la funcion arma_seleccion_df para obtener el rezago para incluir en la ecuacion segun el 
  #criterio de Akaike. Como queremos ver AR(p), MA.m = 0, y como todos los retornos son estacionarios, 
  #entonces d =0.
  mod <- arma_seleccion_df(object=base_niveles[,ind], AR.m, MA.m, d, bool, metodo)
  p   <- mod[which.min(mod$AIC),'p']
  
  #generamos una base de datos que genere columnas de rezagos, el numero de columnas sera el mismo que el orden 
  #obtenido en el procedimiento anterior
  if(p>0) lags_df <- timeSeries::lag((base_niveles[,ind]), c(1:p) )
  
  #Lo colocamos desde el 8 de febrero para cuadrar con el indice de la base de datos principal
  lags_reduced <- lags_df[paste0(dia.inicial,"/"),]
  
  #Nombres del dataframe de rezagos
  colnames(lags_reduced) <- paste0(ind,'.l',1:p) 
  
  return(lags_reduced)
}
#---------------------------------------------------------------------------------------#

#--- Funcion que generaba el sistema de ecuaciones para estimar, fue mejorada por model_equation.LF ---#
if(0){
  #---------------------------------- 11. model_equation  ------------------------------------#
  # Genera una ecuación para el país country y teniendo en cuenta las variables 
  # exógenas exo. Lo anterior dado que vamos a estimar por el metodo SUR, el cual necesitara las 27 
  # ecuaciones siguiendo el paper de Pagnottoni.
  #---------------------------------------------------------------------------------------#
  # ----Argumentos de entrada ----#
  #-- database : base de datos donde se encuentran gran parte de las variables
  #-- country  : caracteres indicando un pais
  #-- exo      : conjunto de variables exogenas, especificamente las dummies. En el primer ejemplo son dummies 
  #            por tipo de desastre
  # ----Argumentos de salida  ----#
  #-- eq: ecuación de variable dependiente ~ regresoras.
  #---------------------------------------------------------------------------------------#
  model_equation <- function(database,country,exo){  
    
    #Busca el dataframe con los rezagos
    lags_name <- paste0("lags_reduced_", country)
    if(exists(lags_name)==TRUE) lags_df <- get(lags_name) ## Primero toca ver si lags_name existe, ya que si existe algun país que no
    #  tenga matriz de rezagos, lags_name no existe. En nuestro caso todos los países
    #  tienen rezagos.
    
    #Busca las variables para el gdp y el fdi
    gdp_variable <- paste("gdp",country,sep="_")
    fdi_variable <- paste("fdi",country,sep="_")
    
    #Genera la ecuacion n.4 por el país country
    eq  <- database[,country]  ~ database[,"Mean_Returns_Moving_Averages"] +exo + database[,gdp_variable] +
      database[,fdi_variable] + lags_df  ## Se asume el nombre "Mean_Returns_Moving_Averages, que fue nombrada en la linea 99 del otro codigo
    return(eq)
  }
  #---------------------------------------------------------------------------------------#
}

#-------------------------------- 12. model_equation.LF----------------------------------------#
# Genera una ecuación para el país country y teniendo en cuenta las variables 
# Lo anterior dado que vamos a estimar por el metodo SUR, el cual necesitara las 27 ecuaciones siguiendo el paper de Pagnottoni.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- database     : base de datos donde se encuentran gran parte de las variables
#-- country      : caracteres indicando un pais
#-- ind          : caracteres indicandon un indice
#-- var.exo      : conjunto de variables exogenas que no dependen del pais 
#-- var.exo.pais : conjunto de variables exogenas que dependen del país
#-- Lags         : string que indica el nombre por el cual empiezan las matrices de rezagos en el codigo principal
# ----Argumentos de salida  ----#
#-- eq: ecuación de variable dependiente ~ regresoras para el país country. 
#---------------------------------------------------------------------------------------#
model_equation.LF <- function(database, country, ind, var.exo, var.exo.pais, Lags){  
  
  #Busca el dataframe con los rezagos
  #lags_name <- paste0("lags_reduced_", country)
  #if(exists(lags_name)==TRUE) lags_df <- get(lags_name) ## Primero toca ver si lags_name existe, ya que si existe algun país que no
  #  tenga matriz de rezagos, lags_name no existe. En nuestro caso todos los países
  #  tienen rezagos.
  
  #Busca las variables para el gdp y el fdi
  var.exo.pais.total = c()
  for (i in 1:length(var.exo.pais))
    var.exo.pais.total = c(var.exo.pais.total, paste(var.exo.pais[i], country, sep="_"))
  
  lag.matrix           =  get(paste0(Lags,'_',ind))
  
  #Genera la ecuacion n.4 por el indice ind
  eq  <- database[,ind]  ~ database[,c(var.exo, var.exo.pais.total)] + lag.matrix  
  return(eq)
}
#---------------------------------------------------------------------------------------#


#---------------------------------- 13. dens  ------------------------------------#
# Genera la densidad kernel para los coeficientes de las dummies dependiendo de <step> y del modelo <fit>
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- fit : modelo estimado al que se le puede sacar coeficientes
#-- step: pasos adelante despues del evento
# ----Argumentos de salida  ----#
#-- densidad: densidad kernel de los coeficientes del modelo estimado
#---------------------------------------------------------------------------------------#
dens <- function(fit, step){
  interest_indices <- grep(step,names(fit))
  interest_coefficients <- fit[interest_indices]
  densidad <- density((interest_coefficients))
  return(densidad)
}
#---------------------------------------------------------------------------------------#


#---------------------------------- 14. densidad_CAR ------------------------------------#
# Genera la densidad de los retornos acumulados. 
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- x: toma un vector de coeficentes
#-- countries: vector de paises
# ----Argumentos de salida  ----#
#-- densidad_c: densidad de los retornos acumulados 
#---------------------------------------------------------------------------------------#

densidad_CAR <- function(x,indices){
  CAR <- c()
  for(ind in indices){
    #Mira dentro el vector de coeficientes cuales inician con ind, es decir el pais y luego los suma
    start_with <- paste0("^",ind)
    sum_of_coefficients <- sum(as.numeric(x[grep(start_with, names(x))]))
    #Agrega la suma (el CAR) en un vector, para luego hallar la densidad
    CAR <- c(CAR,sum_of_coefficients)
  }
  densidad_C <- density(CAR)
  return(densidad_C)
}
#---------------------------------------------------------------------------------------#



#---------------------------------- 15. grafico_densidad  ------------------------------------#
# Genera la densidad de los retornos acumulados. 
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- vector   : un vector que indica los objetos a graficar
#-- labels   : leyenda del grafico
#-- colors   : colores de las lineas del gráfico
#-- main     : título del grafico
#-- width    : ancho de las lineas (default = 2)
#-- position : posicion de la leyenda (default = "topright")
# ----Argumentos de salida  ----#
#-- NA. No retorna argumentos, más bien un gráfico que incluye las 5 densidades (biologico, climatológico
#-- hidrologico, geologico, meteorologico).
#---------------------------------------------------------------------------------------#
grafico_densidad <- function(vector,main,labels,colors,width=2, position = "topright"){
  maximo_y <- c()
  minimo_x <- c()
  maximo_x <- c()
  for(i in vector[1:length(vector)]){
    maximo_y <- c(maximo_y, max(get(i)$y))
    minimo_x <- c(minimo_x, min(get(i)$x))
    maximo_x <- c(maximo_x, max(get(i)$x))
  }
  limite_y          <-  max(maximo_y)
  limite_min_x      <-  min(minimo_x)
  limite_max_x      <-  max(maximo_x)
  
  x11()
  plot(get(vector[1]), main = main, col = colors[1],lwd=width,ylim=c(0,limite_y),xlim=c(limite_min_x,limite_max_x))
  for(i in 2:length(vector)){
    lines(get(vector[i]),col=colors[i],lwd=width)
  }
  legend(position,legend = labels,col = colors, lwd = width)
}
#---------------------------------------------------------------------------------------#


#---------------------------------- 16. grafico_retornos  ------------------------------------#
# Genera la densidad de los retornos acumulados. 
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- list     : lista que contiene las densidades a graficar
#-- vector   : un vector especifico que incluye el nombre del gráfico más los objetos a graficar
#-- main     : titulo del grafico
#-- legends  : leyenda del grafico
#-- colors   : colores de las lineas del gráfico
#-- width    : tamaño de las lineas (default = 3)
#-- position : posicion de la leyenda (default = "topleft")
# ----Argumentos de salida  ----#
#-- NA. No retorna argumentos, más bien un gráfico que incluye las densidades de los retornos de los indices relacionados 
#-- a los paises incluidos en vector.
#---------------------------------------------------------------------------------------#
grafico_retornos <- function(list,vector,main,legends,colors, width=3, position="topleft"){
  maximo_y <- c()
  minimo_x <- c()
  maximo_x <- c()
  
  for(country in vector[1:length(vector)]){
    
    maximo_y <- c(maximo_y, max(list[[country]]$y))
    minimo_x <- c(minimo_x, min(list[[country]]$x))
    maximo_x <- c(maximo_x, max(list[[country]]$x))
  }
  
  limite_y          <-  max(maximo_y)
  limite_min_x      <-  min(minimo_x)
  limite_max_x      <-  max(maximo_x)
  
  x11()
  plot(list[[vector[1]]], main = main, col = colors[1],lwd=width,ylim=c(0,limite_y),xlim=c(limite_min_x,limite_max_x))
  for(i in 2:length(vector)){
    lines(list[[vector[i]]],col=colors[i], lwd=width)
  }
  legend(position,legend = legends,col = colors, lwd=width)
}
#---------------------------------------------------------------------------------------#

#--- Funcion que generaba el las dummies sin tener en cuenta los dias habiles, fue mejorada por create_dummies ---#
if(0){
  #---------------------------------- 17. create_dummies_xts  ------------------------------------#
  # Genera las dummies sin tener  en cuenta los días hábiles . Es decir las dummies originales
  #---------------------------------------------------------------------------------------#
  # ----Argumentos de entrada ----#
  #-- excel_file: archivo de excel
  # ----Argumentos de salida  ----#
  #-- xts_dummies_list: lasta de objetos xts de las dummies
  #---------------------------------------------------------------------------------------#
  create_dummies_xts <- function(excel_file) {
    # Get the names of all the sheets in the Excel file
    sheet_names <- excel_sheets(excel_file)
    
    # Create an empty list to store the xts objects
    xts_dummies_list <- list()
    
    # Loop through all the sheet names
    for (sheet_name in sheet_names[1:length(sheet_names)]) {
      # Read the current sheet into a data frame
      current_sheet <- read.xlsx(excel_file, sheet = sheet_name, detectDates = TRUE)
      
      # Perform the necessary operations on the current sheet to create the xts object
      dates <- index(Retornos)
      dummies <- data.frame(t_0 = double(),t_1 = double(), t_2 = double(),t_3 = double(),t_4 = double())
      for(i in 1:ncol(current_sheet)){
        for(j in 1:nrow(Retornos)){
          if(dates[j] %in% current_sheet[[i]]){
            dummies[j,i] <- 1
          }else if(!(dates[j] %in% current_sheet[[i]])){
            dummies[j,i] <- 0
          }
        }
      }
      D <- c()
      for(i in 1:nrow(dummies)){
        if(sum(dummies[i,]) == 0){
          D <- c(D,sum(dummies[i,]))
        }else if(sum(dummies[i,]) != 0){
          D <- c(D,sum(dummies[i,])/sum(dummies[i,])) 
        }
      }
      xts_object <- as.xts(cbind(dummies,D), order.by = index(Retornos))
      
      # Use the sheet name to create a variable name for the xts object and store it in the list
      xts_name <- paste0(sheet_name, "_dummies_xts}")
      xts_dummies_list[[xts_name]] <- xts_object
    }
    # Return the list of xts objects
    return(xts_dummies_list)
  }
  #---------------------------------------------------------------------------------------#
}

#---------------------------------- 18. grafico_estimates  ------------------------------------#
# La siguiente función es para evitar la repeticion en los graficos AR_estimate y sus t-tests. 
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- object : objeto a graficar (el dataframe incluye group, values, subgroup)
#-- yaxis  : titulo del eje y
#-- title  : titulo del grafico
#-- colors : colores
# ----Argumentos de salida  ----#
#-- NA. No genera argumentos, pero si un grafico de los retornos anormales o estadísticos t.
#---------------------------------------------------------------------------------------#
grafico_estimates <- function(object,yaxis,title,colors){
  ggplot(object, aes(x=group,y=values,fill=subgroup))+
    geom_bar(stat="identity", position="dodge", width=0.7) +
    scale_fill_manual(values= colors) +
    labs(y=yaxis,title=title) +
    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_blank(),
          axis.title.x = element_blank())
}
#---------------------------------------------------------------------------------------#

#----------------------------------- 19. order_coef --------------------------------------------------#
# Ordena un vector de nombres de coeficientes basado en otro vector
# ------------------------------------------------------------------------------------------------
# ----Argumentos de entrada ----#
#-- vec_desordenado : vector que se esta buscando ordenar
#-- vec_orden       : vector que dara el orden
# ----Argumentos de salida  ----#
#-- vec_ordenado    : vector numerico con el orden 
#---------------------------------------------------------------------------------------#

order_coef <- function(vec_desordenado, vec_orden){
  indexes <- c()
  for (i in 1:length(vec_orden)){
    indexes <- c(indexes, grep(vec_orden[i],vec_desordenado))
  }
  vec_ordenado = vec_desordenado[indexes]
  return(vec_ordenado)
}

# Codigo para graficar los CAR por pais, fue mejorado por car_countries2, ya que usa una lista de coeficientes y no una lista
# de modelos.
if(0){
  #----------------------------------- 20. car_countries --------------------------------------------------#
  # Genera una grafica para cada indice de los retornos anormales acumulados (CAR) promedios dependiendo del continente y un
  # nivel de significancia.
  # ------------------------------------------------------------------------------------------------
  # ----Argumentos de entrada ----#
  # continent_model    : Un modelo estimado, el cual tiene ciertas propiedades, se le puede hacer summary y sacar los coeficientes
  #                      con $ coeficients, los cuales incluyen el coeficiente estimado, error estandar, t_value y p_value.
  # significance.level : El nivel de significancia que deben tener los coeficientes estimados para graficarlos
  # pattern.step       : Al estimar los modelos por continente, los coeficientes tienen un nombre característico, pattern.step
  #                      se utiliza para extraer la parte que corresponde a en que dia despues del evento se refiere el coeficiente (t0,t1,t2...)
  # pattern.indexes    : string que es la parte del nombre del coeficiente que se refiere al pais del indice bursatil
  # pattern.countries  : string que es la parte del nombre del coeficiente que se refiere al pais donde ocurre el desastre
  # order.graph        : vector de nombres de paises que indicara el orden en que se graficara
  # labels             : leyendas del eje de los indices bursatiles
  # color              : color para la grafica
  # title.graph        : titulo para la grafica
  # ----Argumentos de salida  ----#
  #-- plot_continent   : objeto tipo ggplot para poder graficar los retornos anormales acumulados (CAR) promedio para cada continente
  #                      dependiendo de un nivel de significancia
  #---------------------------------------------------------------------------------------#
  
  car_countries <- function(continent_model, significance.level, pattern.step, pattern.indexes, pattern.countries, order.graph, labels, color, title.graph){
    
    # Generamos un dataframe que va a guardar los parametros estimados, el error estandar, el valor del t test y el p_value.
    dataframe_modelo <- data.frame(Estimate=double(), SD_error=double(), t_value=double(), p_value=double()) 
    
    # Filtrar por aquellos que acaben en t0, t1, t2, t3 o t4.
    for (element in continent_model){
      ## Genera un dataframe con el estimado, error estandar, t_value, p_value, que salen de la estimacion element
      dataframe_coef <- as.data.frame(summary(element)$coefficients) 
      ## Le cambiamos nombres al dataframe para mejor manejo, pero reflejan lo mismo
      colnames(dataframe_coef) <- c("Estimate","SD_error","t_value","p_value")
      ## Extraemos las filas que nos interesan, es decir aquellas de las dummies, que acaban en t0, t1, t2, t3 o t4, es decir steps
      dataframe_coef_filtrado  <- dataframe_coef %>% 
        dplyr::filter(str_ends(row.names(.),pattern = pattern.step)) %>% 
        # Filtramos tambien para aquellos coeficientes que tengan un nivel de significancia menor que niv.significancia
        dplyr::filter(p_value < significance.level)
      ## Dejamos todos los coeficientes que cumplen con las condiciones en un solo dataframe
      dataframe_modelo   <- rbind(dataframe_modelo, dataframe_coef_filtrado)
    }
    
    ## Ahora para poder promediar los retornos anormales es necesario poder extraer el pais del indice y el step, usando la funcion
    ## str_extract, que extrae el primer valor identico entre dos strings. Esto permite extraer el indice, que en cada fila aparece 
    ## de primer lugar
    string_start <- stringr::str_extract(row.names(dataframe_modelo), pattern.indexes)  
    # cualquier valor de countries
    string_end   <- stringr::str_extract(row.names(dataframe_modelo), pattern.step)  #En caso de querer revisar coeficientes por step
    
    ## Aparte, necesitamos hacer el CAR por cada pais del desastre por lo que el siguiente codigo obtiene todos los valores identicos entre 
    #  dos strings. Sin embargo, para algunas filas va a encontrar dos coincidencias, la primera refiriendose al indice bursatil y la segunda 
    #  al pais del desastre, por lo que necesitaremos por cada sublista guardar solamente el ultimo elemento.
    string_pais     <- stringr::str_extract_all(row.names(dataframe_modelo), pattern.countries) 
    string_pais_vec <- sapply(string_pais, function(x) x[[length(x)]])
    
    dataframe_modelo <- cbind(dataframe_modelo,string_start,string_pais_vec,string_end)
    
    ## Ellos mencionan que realizaran un promedio del CAR, por lo cual realizaremos el CAR de cada pais que tenemos datos
    #  para cada indice y realizaremos un promedio
    
    promedio_car <- dataframe_modelo %>% 
      group_by(string_start, string_pais_vec) %>% 
      summarise(CAR = sum(Estimate)) %>% # El nombre Estimate sale del nombre que le colocamos anteriormente
      group_by(string_start) %>% 
      summarise(mean_CAR = mean(CAR)) %>% 
      arrange(match(string_start,order.graph))
    
    ## Por ultimo, se agregan a las graficas los indices que no contienen valores, de modo que al graficar no saldra dato (es 0)
    #  pero sirve para comparar con la grafica del paper
    
    dataframe_pagnorden <- data.frame(string_start = order.graph)
    #Juntar las dos dataframes y rellenar con 0 los datos faltantes
    promedio_car_all <- left_join(dataframe_pagnorden, promedio_car, by = "string_start") # string_start sale de un nombre que le colocamos en la funcion
    promedio_car_all$mean_CAR <- ifelse(is.na(promedio_car_all$mean_CAR), 0, promedio_car_all$mean_CAR)
    
    promedio_car_all$string_start <- factor(promedio_car_all$string_start, levels = promedio_car_all$string_start)
    
    plot_continent <- ggplot(data = promedio_car_all, aes(y = mean_CAR, x = string_start, fill = mean_CAR < 0)) +
      geom_col() +
      scale_fill_manual(values = c(color, color)) +
      coord_flip()+
      scale_x_discrete(labels = labels)+
      theme_light() + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      guides(fill = "none") +
      labs(y = paste0("Average CAR | p-value < ",percent(niv.significancia)),x="Index",title = title.graph)
    return(plot_continent)
  }
}

#----------------------------------- 20. car_countries2 --------------------------------------------------#
# Genera una grafica para cada indice de los retornos anormales acumulados (CAR) promedios dependiendo del continente y un
# nivel de significancia.
# ------------------------------------------------------------------------------------------------
# ----Argumentos de entrada ----#
# continent_coefficients    : Una lista de coeficientes, junto a su error estandar, t_value y p_value. 
# significance.level        : El nivel de significancia que deben tener los coeficientes estimados para graficarlos
# pattern.step              : Al estimar los modelos por continente, los coeficientes tienen un nombre característico, pattern.step
#                             se utiliza para extraer la parte que corresponde a en que dia despues del evento se refiere el coeficiente (t0,t1,t2...)
# pattern.indexes           : string que es la parte del nombre del coeficiente que se refiere al pais del indice bursatil
# pattern.countries         : string que es la parte del nombre del coeficiente que se refiere al pais donde ocurre el desastre
# order.graph               : vector de nombres de paises que indicara el orden en que se graficara
# labels                    : leyendas del eje de los indices bursatiles
# color                     : color para la grafica
# title.graph               : titulo para la grafica
# ----Argumentos de salida  ----#
#-- plot_continent   : objeto tipo ggplot para poder graficar los retornos anormales acumulados (CAR) promedio para cada continente
#                      dependiendo de un nivel de significancia
#---------------------------------------------------------------------------------------#


car_countries2 <- function(continent_coefficients, significance.level, pattern.step, pattern.indexes, pattern.countries, order.graph, labels, color, title.graph){
  
  # Generamos un dataframe que va a guardar los parametros estimados, el error estandar, el valor del t test y el p_value.
  dataframe_modelo <- data.frame(Estimate=double(), SD_error=double(), t_value=double(), p_value=double()) 
  
  # Filtrar por aquellos que acaben en t0, t1, t2, t3 o t4.
  for (element in continent_coefficients){
    ## Genera un dataframe con el estimado, error estandar, t_value, p_value, que salen de la estimacion element
    dataframe_coef <- as.data.frame(element) 
    ## Le cambiamos nombres al dataframe para mejor manejo, pero reflejan lo mismo
    colnames(dataframe_coef) <- c("Estimate","SD_error","t_value","p_value")
    ## Extraemos las filas que nos interesan, es decir aquellas de las dummies, que acaban en t0, t1, t2, t3 o t4, es decir steps
    dataframe_coef_filtrado  <- dataframe_coef %>% 
      dplyr::filter(str_ends(row.names(.),pattern = pattern.step)) %>% 
      # Filtramos tambien para aquellos coeficientes que tengan un nivel de significancia menor que niv.significancia
      dplyr::filter(p_value < significance.level)
    ## Dejamos todos los coeficientes que cumplen con las condiciones en un solo dataframe
    dataframe_modelo   <- rbind(dataframe_modelo, dataframe_coef_filtrado)
  }
  
  ## Ahora para poder promediar los retornos anormales es necesario poder extraer el pais del indice y el step, usando la funcion
  ## str_extract, que extrae el primer valor identico entre dos strings. Esto permite extraer el indice, que en cada fila aparece 
  ## de primer lugar
  string_start <- stringr::str_extract(row.names(dataframe_modelo), pattern.indexes) 
  # cualquier valor de countries
  string_end   <- stringr::str_extract(row.names(dataframe_modelo), pattern.step)  #En caso de querer revisar coeficientes por step
  
  ## Usamos la misma funcion para extraer el nombre del pais donde sucedio el desastre
  string_pais <- stringr::str_extract(row.names(dataframe_modelo), pattern.countries)
  
  dataframe_modelo <- cbind(dataframe_modelo,string_start,string_pais,string_end)
  
  ## Ellos mencionan que realizaran un promedio del CAR, por lo cual realizaremos el CAR de cada pais que tenemos datos
  #  para cada indice y realizaremos un promedio
  
  promedio_car <- dataframe_modelo %>% 
    group_by(string_start, string_pais) %>% 
    summarise(CAR = sum(Estimate)) %>% # El nombre Estimate sale del nombre que le colocamos anteriormente
    group_by(string_start) %>% 
    summarise(mean_CAR = mean(CAR)) %>% 
    arrange(match(string_start,order.graph))
  
  ## Por ultimo, se agregan a las graficas los indices que no contienen valores, de modo que al graficar no saldra dato (es 0)
  #  pero sirve para comparar con la grafica del paper
  
  dataframe_pagnorden <- data.frame(string_start = order.graph)
  #Juntar las dos dataframes y rellenar con 0 los datos faltantes
  promedio_car_all <- left_join(dataframe_pagnorden, promedio_car, by = "string_start") # string_start sale de un nombre que le colocamos en la funcion
  promedio_car_all$mean_CAR <- ifelse(is.na(promedio_car_all$mean_CAR), 0, promedio_car_all$mean_CAR)
  promedio_car_all$string_start <- factor(promedio_car_all$string_start, levels = promedio_car_all$string_start)
  
  plot_continent <- ggplot(data = promedio_car_all, aes(y = mean_CAR, x = string_start, fill = mean_CAR < 0)) +
    geom_col() +
    scale_fill_manual(values = c(color, color)) +
    coord_flip()+ 
    scale_x_discrete(labels = labels)+
    theme_light() + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    guides(fill = "none") +
    labs(y = paste0("Average CAR | p-value < ",percent(significance.level)),x="Index",title = title.graph)
  return(plot_continent)
}


#----------------------------------- 20. average_countries2 --------------------------------------------------#
# Genera una grafica para cada indice de los retornos anormales promedio dependiendo del continente y un
# nivel de significancia. Es parecida a car_countries2, solamente que en vez de realizar el promedio de los retornos anormales
# acumulados, realiza un promedio de los promedios de los retornos anormales.
# ------------------------------------------------------------------------------------------------
# ----Argumentos de entrada ----#
# continent_coefficients    : Una lista de coeficientes, junto a su error estandar, t_value y p_value. 
# significance.level        : El nivel de significancia que deben tener los coeficientes estimados para graficarlos
# pattern.step              : Al estimar los modelos por continente, los coeficientes tienen un nombre característico, pattern.step
#                             se utiliza para extraer la parte que corresponde a en que dia despues del evento se refiere el coeficiente (t0,t1,t2...)
# pattern.indexes           : string que es la parte del nombre del coeficiente que se refiere al pais del indice bursatil
# pattern.countries         : string que es la parte del nombre del coeficiente que se refiere al pais donde ocurre el desastre
# order.graph               : vector de nombres de paises que indicara el orden en que se graficara
# labels                    : leyendas del eje de los indices bursatiles
# color                     : color para la grafica
# title.graph               : titulo para la grafica
# ----Argumentos de salida  ----#
#-- plot_continent   : objeto tipo ggplot para poder graficar los retornos anormales acumulados (CAR) promedio para cada continente
#                      dependiendo de un nivel de significancia
#---------------------------------------------------------------------------------------#


average_countries2 <- function(continent_coefficients, significance.level, pattern.step, pattern.indexes, pattern.countries, order.graph, labels, color, title.graph){
  
  # Generamos un dataframe que va a guardar los parametros estimados, el error estandar, el valor del t test y el p_value.
  dataframe_modelo <- data.frame(Estimate=double(), SD_error=double(), t_value=double(), p_value=double()) 
  
  # Filtrar por aquellos que acaben en t0, t1, t2, t3 o t4.
  for (element in continent_coefficients){
    ## Genera un dataframe con el estimado, error estandar, t_value, p_value, que salen de la estimacion element
    dataframe_coef <- as.data.frame(element) 
    ## Le cambiamos nombres al dataframe para mejor manejo, pero reflejan lo mismo
    colnames(dataframe_coef) <- c("Estimate","SD_error","t_value","p_value")
    ## Extraemos las filas que nos interesan, es decir aquellas de las dummies, que acaban en t0, t1, t2, t3 o t4, es decir steps
    dataframe_coef_filtrado  <- dataframe_coef %>% 
      dplyr::filter(str_ends(row.names(.),pattern = pattern.step)) %>% 
      # Filtramos tambien para aquellos coeficientes que tengan un nivel de significancia menor que niv.significancia
      dplyr::filter(p_value < significance.level)
    ## Dejamos todos los coeficientes que cumplen con las condiciones en un solo dataframe
    dataframe_modelo   <- rbind(dataframe_modelo, dataframe_coef_filtrado)
  }
  
  ## Ahora para poder promediar los retornos anormales es necesario poder extraer el pais del indice y el step, usando la funcion
  ## str_extract, que extrae el primer valor identico entre dos strings. Esto permite extraer el indice, que en cada fila aparece 
  ## de primer lugar
  string_start <- stringr::str_extract(row.names(dataframe_modelo), pattern.indexes) 
  # cualquier valor de countries
  string_end   <- stringr::str_extract(row.names(dataframe_modelo), pattern.step)  #En caso de querer revisar coeficientes por step
  
  ## Usamos la misma funcion para extraer el nombre del pais donde sucedio el desastre
  string_pais <- stringr::str_extract(row.names(dataframe_modelo), pattern.countries)
  
  dataframe_modelo <- cbind(dataframe_modelo,string_start,string_pais,string_end)
  
  ## Ellos mencionan que realizaran un promedio del CAR, por lo cual realizaremos el CAR de cada pais que tenemos datos
  #  para cada indice y realizaremos un promedio
  
  promedio_car <- dataframe_modelo %>% 
    group_by(string_start, string_pais) %>% 
    summarise(AVERAGE = mean(Estimate)) %>% # El nombre Estimate sale del nombre que le colocamos anteriormente
    group_by(string_start) %>% 
    summarise(mean_AVERAGE = mean(AVERAGE)) %>% 
    arrange(match(string_start,order.graph))
  
  ## Por ultimo, se agregan a las graficas los indices que no contienen valores, de modo que al graficar no saldra dato (es 0)
  #  pero sirve para comparar con la grafica del paper
  
  dataframe_pagnorden <- data.frame(string_start = order.graph)
  #Juntar las dos dataframes y rellenar con 0 los datos faltantes
  promedio_car_all <- left_join(dataframe_pagnorden, promedio_car, by = "string_start") # string_start sale de un nombre que le colocamos en la funcion
  promedio_car_all$mean_AVERAGE <- ifelse(is.na(promedio_car_all$mean_AVERAGE), 0, promedio_car_all$mean_AVERAGE)
  
  promedio_car_all$string_start <- factor(promedio_car_all$string_start, levels = promedio_car_all$string_start)
  
  plot_continent <- ggplot(data = promedio_car_all, aes(y = mean_AVERAGE, x = string_start, fill = mean_AVERAGE < 0)) +
    geom_col() +
    scale_fill_manual(values = c(color, color)) +
    coord_flip()+
    scale_x_discrete(labels = labels)+
    theme_light() + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    guides(fill = "none") +
    labs(y = paste0("Average ARs | p-value < ",percent(significance.level)),x="Index",title = title.graph)
  return(plot_continent)
}

#---------------------------------- 18. matching  ------------------------------------#
# Hacer matching entre el pais y el nombre del indice. 
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- pais: el nombre de un pais
# ----Argumentos de salida  ----#
#-- index: el nombre del indice que le corresponde
#---------------------------------------------------------------------------------------#
matching <- function(pais){
  if(pais=="Australia"){
    index ="S.PASX200"
  }else if(pais=="Belgium"){
    index ="BEL20"
  }else if(pais=="Brazil"){
    index ="Bovespa"
  }else if(pais=="Canada"){
    index ="S.PTSXComposite"
  }else if(pais=="Chile"){
    index ="S.PCLXIPSA"
  }else if(pais=="Denmark"){
    index ="OMXCopenhagen20"
  }else if(pais=="Finland"){
    index ="OMXHelsinki25"
  }else if(pais=="France"){
    index ="CAC40"
  }else if(pais=="Germany"){
    index ="DAX"
  }else if(pais=="HongKong"){
    index ="HangSeng"
  }else if(pais=="India"){
    index ="Nifty50"
  }else if(pais=="Indonesia"){
    index ="JakartaStockExchange"
  }else if(pais=="Mexico"){
    index ="S.PBMVIPC"
  }else if(pais=="Netherlands"){
    index ="AEX"
  }else if(pais=="Norway"){
    index ="OSEBenchmark"
  }else if(pais=="Poland"){
    index ="WIG20"
  }else if(pais=="Russia"){
    index ="MOEXRussia"
  }else if(pais=="SouthAfrica"){
    index ="SouthAfricaTop40"
  }else if(pais=="SouthKorea"){
    index ="KOSPI"
  }else if(pais=="Spain"){
    index ="IBEX35"
  }else if(pais=="Sweden"){
    index ="OMXStockholm30"
  }else if(pais=="Switzerland"){
    index ="SMI"
  }else if(pais=="Thailand"){
    index ="SETIndex"
  }else if(pais=="Turkey"){
    index ="BIST100"
  }else if(pais=="UnitedKingdom"){
    index ="FTSE100"
  }else if(pais=="USA"){
    index =c("NASDAQComposite","Nasdaq100")
  }else{
    index = NULL
  }
  return(index)
}
#---------------------------------------------------------------------------------------#

#---------------------------------- 19. drop.events  ------------------------------------#
# Filtrar una base de eventos para tener una ventana minima de estimacion y una ventana 
# minima de evento al momento de estimar por OLS. 
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- data.events        : dataframe de eventos, que debe incluir alguna columna en formato fecha para funcionar
#-- market.returns     : indice de mercado
#-- estimation.start   : numero de dias previos al evento para el inicio de la ventana de estimacion
#-- max.ar             : numero de dias maximos despues del evento para calcular retornos anormales
#-- date.col.name      : nombre de la columna de fechas de eventos
#-- var.col.name      : nombre de la columna que tiene informacion del evento como su locacion
# ----Argumentos de salida  ----#
#-- data.droped.events : dataframe de eventos filtrados. En este df ya no estan los eventos que no cuentan con una 
#                        ventana minima de estimacion ni con una ventana minima de evento
#---------------------------------------------------------------------------------------#

drop.events <- function(data.events,market.returns,estimation.start,max.ar,date.col.name, var.col.name){
  
  # Se renombran las columnas para un uso correcto de la funcion
  colnames(data.events)[colnames(data.events) == date.col.name] <- "Start.Date"
  colnames(data.events)[colnames(data.events) == var.col.name] <- "Country"
  # Se detiene la funcion en caso de que la columna <data.events$Start.Date> no sea de clase date
  if(!inherits(eventos$Start.Date,"Date")) {
    stop(paste0(paste0("La columna ",date.col.name)," no es formato fecha."))
  }

  # Fecha minima para que la estimacion pueda empezar desde <estimation.start> dias previos al evento
  Fecha_minima_estimacion <- index(market.returns)[estimation.start+1]
  # Fecha minima para que se pueda realizar el calculo de retornos anormales para <max.ar>+1 dias
  Fecha_minima_evento     <- index(market.returns)[length(index(market.returns))-(max.ar)]
  # Filtracion <data.events>. Solamente contiene eventos entre <Fecha_minima_estimacion> y <Fecha_minima_evento>
  data.events <- data.events %>% 
    dplyr::filter(dplyr::between(Start.Date, Fecha_minima_estimacion, Fecha_minima_evento))
  return(data.events)
}
#---------------------------------------------------------------------------------------#

#------------------------------   19. estimation.event.study  --------------------------#
# Realizar una estimacion por OLS siguiendo el modelo de mercado, obteniendo retornos anormales 
# y error estandar de la estimacion.
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- data.events        : dataframe de eventos, que debe incluir alguna columna en formato fecha para funcionar
#-- days.evaluated     : maximo numero de dias a evaluar en caso de que la fecha de un evento no este en el indice de las 
#                        series a estimar
#-- asset.returns      : base de datos con variables independientes en la estimacion de modelo de mercado (R_it)
#-- market.returns     : serie que corresponde al indice de mercado (R_mt)
#-- max.ar             : numero de dias maximos despues del evento para calcular retornos anormales
#-- es.start           : numero de dias previos al evento para comenzar la estimacion
#-- es.end             : numero de dias previos al evento para terminar la estimacion
# ----Argumentos de salida  ----#
#-- all.events.list    : lista que incluye para cada par evento-indice la siguiente informacion:
#--   <Dataframe>      : base de datos con retornos observados, estimados y anormales para la ventana 
#--                      de estimacion y para la ventana de evaluacion del evento
#--   <Standard_Error> : error estandar de los residuales de la estimacion por OLS
#---------------------------------------------------------------------------------------#

estimation.event.study <- function(data.events, days.evaluated, asset.returns, market.returns, max.ar, es.start, es.end){
  all_events_list      <- list() # lista que contendra todas las series de retornos + errores estandar
  # Loop: Por cada evento se hace una regresion OLS con la muestra [-<es.start>,-<es.end>] dias antes del evento para estimar alfa, beta 
  for(i in 1:nrow(data.events)){
    # Primero se encuentra a que dato le corresponde el dia del evento, y el dia final de la ventana de evento es el dia del evento
    # mas <max.ar>
    event_list  <- list() # lista donde se guarda por cada evento un dataframe de retornos observados, estimados y anormales;
    # junto a error estandar del error en la estimacion
    pais        <- as.character(data.events[i,'Country']) # Establece el pais donde sucedio el evento
    index_names <- matching(pais) # Nombre de la variable del <pais> con la que se calculan retornos anormales (ej: stock-index del pais)
    # Detener la funcion si no se tiene indice para el pais especificado
    if(is.null(index_names)) stop(paste0("No hay indice para el pais: ", pais))
    suppressWarnings({
      # Loop que genera la posicion de desastre respecto al indice de <asset.returns>. Si la fecha del evento no esta en el indice de 
      # <asset.returns>,se revisara hasta <days.evaluated> dias despues del desastre para ser considerado como el inicio del evento
      for(j in 0:days.evaluated){
        if((data.events[i,'Start.Date']+j) %in% index(asset.returns[,index_names])){ 
          # Generacion del dia del desastre (o j dias despues del desastre, si el dia del desastre no esta en el indice de 
          # <asset.returns>)
          event_start_date  <- data.events[i,'Start.Date']+j
          # Generacion de la posicion del dia de desastre en el indice de fechas de <asset.returns>
          # (o j dias despues del desastre, si el dia del desastre no esta en el indice de <asset.returns>)
          event_start_index <- which(index(asset.returns[,index_names])==event_start_date)
          break
        }
      }
    })
    
    # Regresion por OLS del modelo de mercado
    # Loop para los casos en que haya mas de un indice por pais, se realiza regresion OLS para estimar alpha y beta
    # Nota: En general solo hay un indice por pais, pero en USA hay dos.
    for(name in index_names){
      # Creacion  de la base de datos de la ventana de estimacion en <asset.returns> para <name> 
      # y para el indice de mercado, <market.returns>, que es una var.exogena del modelo
      # <est.dependent.var> se refiere a la variable dependiente en la estimacion.
      # <est.independent.var> se refiere a la variable independiente en la estimacion
      # La posicion del primer dia de la ventana de estimacion respecto al indice de <asset.returns> o <market.returns>
      # es (<event_start_index> - <es.start>) mientras que la posicion de ultimo dia de la ventana de estimacion es 
      # (<event_start_index> - <es.end>)
      est.dependent.var   <- asset.returns[,name][(event_start_index-es.start):(event_start_index-es.end),]
      est.independent.var <- market.returns[(event_start_index-es.start):(event_start_index-es.end)]
      
      # Detener la funcion si los indices de fechas de <est.dependent.var> y de <est.independent.var> no son los mismos
      if(!identical(index(est.dependent.var),index(est.independent.var))) stop("Las series tienen indices de fechas diferentes")
      
      # Usar <as.numeric> para evitar problemas con la estimacion <lm()>
      # Estimar el modelo por OLS
      model          <- lm(as.numeric(est.dependent.var) ~ as.numeric(est.independent.var))
      # Calcular los parametros de interes
      alpha          <- model$coefficients[["(Intercept)"]] 
      beta           <- model$coefficients[["as.numeric(est.independent.var)"]]
      standard_error <- sd(residuals(model))
      
      # Creacion series <observed>, <predicted> y <abnormal> solamente para la ventana de estimacion y la ventana de evento
      # Se usa el indice de <est.dependent.var> para obtener las fechas pertenecientes a la ventana de estimacion.
      # <window.event.dates> son las fechas que pertenecen a la ventana de evento
      window_event_dates <- index(asset.returns[,name][(event_start_index):(event_start_index+max.ar)])
      # Se selecciona de <asset.returns> solamente las observaciones que esten en la ventana de estimacion o la de evento
      observed           <- asset.returns[,name][c(index(est.dependent.var),window_event_dates)]
      # Se selecciona de <market.returns> solamente las observaciones que esten en la ventana de estimacion o la de evento
      predicted          <- alpha + beta*(market.returns[index(observed)]) 
      # Se restan los retornos estimados de los observados
      abnormal           <- observed - predicted
      # Se juntan las tres series en un solo dataframe
      df             <- merge(observed,predicted,abnormal)
      # Cambio de nombre de columnas
      colnames(df)   <- c('Observed','Predicted','Abnormal')
      # Agregar el dataframe a la lista <event_list>
      event_list[["Dataframe"]]       <- df 
      # Agregar el error estandar a la lista <event_list>
      event_list[["Standard_Error"]]  <- standard_error
      # Agregar la lista <event_list> a la lista <all_events_list>, por lo que seria una lista de listas
      all_events_list[[paste(name,i,sep="_")]] <- event_list
    }
  } 
  return(all_events_list)
}

#---------------------------------------------------------------------------------------#

#------------------------------   20. wilcoxon.jp.test  ----- --------------------------#
# Realizar una prueba de rank-signed Wilcoxon teniendo en cuenta una lista generada por la funcion
# <estimation.event.study>
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- data.list        : lista generada por la funcion <estimation.event.study>, que incluye para 
#--                    cada par evento-indice un dataframe con retornos anormales
#-- es.window.length : tamaño ventana de estimacion
#-- ev.window.length : tamaño ventana de evento
# ----Argumentos de salida  ----#
#-- result           : dataframe con el estadistico de Wilcoxon y su significancia (* para 10%, ** para 5% y *** para 1%)
#---------------------------------------------------------------------------------------#

wilcoxon.jp.test <- function(data.list,es.window.length,ev.window.length){
  # Para el calculo del CAR se toma la serie <Abnormal> a partir de la obs <es.window.length> + 1 hasta 
  # <es.window.length> + <ev.window.length>
  # Se crea el vector <all_car> para guardar los CAR
  # Nota: La longitud de este vector es igual al numero de eventos si solo hay un indice por pais.
  #       Si hay mas de un indice por pais, la longitud de <all_car> aumenta consecuentemente
  all_car <- c()
  for(element in data.list){
    car     <- sum(element$Dataframe$Abnormal[(es.window.length+1):(es.window.length+ev.window.length)])
    all_car <- c(all_car,car)
  }
  
  # Se genera un dataframe para poder realizar el ordenamiento de los car
  df_car <- data.frame("car"=all_car,"magnitude"=abs(all_car),"sign"=sign(all_car))
  df_car <- df_car %>% 
    mutate(magnitude=ifelse(magnitude==0,NA,magnitude)) ## Se coloca NA si la magnitud es 0, ya que no se deben considerar
  # Funcion <rank> para ordenar las magnitudes de los car
  df_car <- df_car %>% 
    mutate(rank = rank(magnitude))
  
  # Suma de los rangos de car positivos: <positive_rank_sum>
  rank_sum <- df_car %>% 
    dplyr::group_by(sign) %>%
    dplyr::summarize(sum = sum(rank))
  positive_rank_sum <- rank_sum$sum[rank_sum$sign=="1"]
  
  # Calculo de la significancia del estadistico de Wilcoxon
  # La funcion para hallar los cuantiles de la distribucion del estadistico de Wilcoxon es <stats::qsignrank()>. 
  # A partir de 1000 observaciones, la funcion no se comporta adecuadamente, pero debido a que es muestra grande, la distribucion
  # converge a una normal con media N(N+1)/4 y varianza N(N+1)(2N+1)/24 por lo que usamos <stats::qnorm()>>
  significance <- ""
  ## Calculo para cada nivel de significancia si el valor de <positive_rank_sum> es lo suficientemente extremo para rechazar H_0
  #  La prueba se hace a dos colas, por lo que la primera condicion para cada prueba de significancia compara <positive_rank_sum> con el 
  #  valor critico de la cola derecha.
  #  La segunda condicion es la comparacion de <positive_rank_sum> con el valor critico de la cola izquierda.
  N <- length(data.list)
  
  # Uso de <qsignrank> o <qnorm> dependiendo del tamaño de la muestra
  if(N<=1000){
    # Si se evalua la significancia al 10%, con un test a dos colas, debemos buscar los percentiles 5 y 95, y comparar con estadistico
    significance[positive_rank_sum >= stats::qsignrank(1 - 0.1/2,n=N)|
                   positive_rank_sum <= stats::qsignrank(0.1/2, n=N)] <- "*"
    # Para evaluar al 5%:
    significance[positive_rank_sum >= stats::qsignrank(1 - 0.05/2, n=N)|
                   positive_rank_sum <= stats::qsignrank(0.05/2, n=N)] <- "**"
    # Al 1%:
    significance[positive_rank_sum >= stats::qsignrank(1 - 0.01/2, n=N)|
                   positive_rank_sum <= stats::qsignrank(0.01/2, n=N)] <- "***"
    resultado <- data.frame("Wilcoxon_statistic" = positive_rank_sum,"Significancia" = significance)
  }else{
    mu = N*(N+1)/4
    sigma = sqrt(N*(N+1)*(2*N+1)/24)
    # Si se evalua la significancia al 10%, con un test a dos colas, debemos buscar los percentiles 5 y 95, y comparar con estadistico
    significance[positive_rank_sum >= stats::qnorm(1 - 0.1/2,mean = mu,sd = sigma)|
                   positive_rank_sum <= stats::qnorm(0.1/2, mean = mu,sd = sigma)] <- "*"
    # Para evaluar al 5%:
    significance[positive_rank_sum >= stats::qnorm(1 - 0.05/2, mean = mu,sd = sigma)|
                   positive_rank_sum <= stats::qnorm(0.05/2, mean = mu,sd = sigma)] <- "**"
    # Al 1%:
    significance[positive_rank_sum >= stats::qnorm(1 - 0.01/2, mean = mu,sd = sigma)|
                   positive_rank_sum <= stats::qnorm(0.01/2, mean = mu,sd = sigma)] <- "***"
    resultado <- data.frame("Wilcoxon_statistic" = positive_rank_sum,"Significancia" = significance)
  }
  # Por ultimo, usando la funcion <wilcox.test> obtenemos el pvalor
  p_value <- wilcox.test(all_car)$p.value
  resultado <- cbind(resultado, "p_value" = p_value)
  return(resultado)
}

#---------------------------------------------------------------------------------------#

#------------------------------   21. bootstrap_CT  ----- --------------------------#
# Realizar una prueba usando bootstrap siguiendo el procedimiento de Corrado & Truong (2008)
# El estadistico usado es el de Patell
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- data.list        : lista generada por la funcion <estimation.event.study>, que incluye para 
#--                    cada par evento-indice un dataframe con retornos anormales
#-- market.returns   : serie que corresponde al indice de mercado (Rmt)
#-- es.window.length : tamaño ventana de estimacion
#-- ev.window.length : tamaño ventana de evento
#-- no.simul         : numero de simulaciones para el bootstrap
# ----Argumentos de salida  ----#
#-- result           : dataframe con el estadistico de Patell y su significancia (* para 10%, ** para 5% y *** para 1%)
#---------------------------------------------------------------------------------------#

bootstrap_CT <- function(data.list,market.returns,es.window.length,ev.window.length,no.simul){ 
 standardized_cars <- c()
  for(element in data.list){
    # Calculo de las fechas de ventana de estimacion y de evento para el evento relacionado con <element>
    # Como el objeto <data.list> viene de la funcion <estimation.event.study>, tiene el objeto $Dataframe$Abnormal
    # Aparte, en <element$Dataframe$Abnormal> tenemos observaciones solamente para la ventana de estimacion y la ventana de evento
    # por lo cual, los indices de la ventana de estimacion son (1:<es.window.length>) y los de ventana evento son
    # (<es.window.length>+1):(<es.window.length>+<ev.window.length>)
    estimation_dates <- index(element$Dataframe$Abnormal)[1:es.window.length]
    event_dates      <- index(element$Dataframe$Abnormal)[(es.window.length+1):(es.window.length+ev.window.length)]
    
    # Calculo promedio retornos anormales a lo largo de la ventana de evento
    averaged_car <- (1/ev.window.length)*sum(element$Dataframe$Abnormal[event_dates])
    # El error estandar para C&T(2008) incluye el promedio del retorno de mercado durante la ventana de estimacion y la ventana de evento
    # De la serie de retorno de mercado <market.returns> escogemos aquellas obs. que estan dentro de la ventana de estimacion y ventana de evento
    market_estimation <- market.returns[estimation_dates]
    market_event      <- market.returns[event_dates]
    # Error estandar siguiendo C&T (2008)
    prediction_error  <- (element$Standard_Error/sqrt(ev.window.length))*(sqrt((1) + (ev.window.length/es.window.length) + 
                         (((ev.window.length*((mean(market_event)-mean(market_estimation))^2))/(sum((market_estimation-mean(market_estimation))^2))))))
    
    # Generar el car promedio estandarizado al dividir entre el error estandar estimado
    standardized_car  <- averaged_car/prediction_error
    standardized_cars <- c(standardized_cars, standardized_car)
  }
  
  # Estadistico de Patell que esta en C&T(2008), <Tp>
  N  <- length(data.list)
  Tp <- (sqrt(ev.window.length)/sqrt(N))*sum(standardized_cars)
  
  # Muestreo aleatorio con reemplazo sobre <standardized_cars> y calculo de estadistico
  
  boot_n <- no.simul  #<<<--- Numero de iteraciones para calcular el estadistico 
  boot_statistics <- c() # Vector para guardar los estadisticos calculados por bootstrap
  for(i in 1:boot_n){
    # Tomar muestra de <N> valores con reemplazo de los car estandarizados
    sample_standardized <- sample(standardized_cars,N, replace= TRUE)
    # <boot_Tp> hace referencia al estadistico de la muestra <sample_standardized>
    boot_Tp             <- (sqrt(ev.window.length)/sqrt(N))*sum(sample_standardized)
    boot_statistics     <- c(boot_statistics, boot_Tp)
  }
  
  # Percentil de <Tp> en la poblacion de estadisticos <boot_Tp>
  
  # Hallar los valores criticos de la distribucion usando <quantile> para significancia 10%
  significancia <- ""
  # Comprobar significancia al 10% con dos colas. La primera condicion es verificar si <Tp> es menor que el valor critico de
  # la distribucion hallada por bootstrap. La segunda condicion es <Tp> mayor que el valor critico de la distribucion bootstrap.
  significancia[Tp <= quantile(boot_statistics, 0.1/2) | Tp >= quantile(boot_statistics, 1- 0.1/2)] <- "*"
  # Significancia al 5%
  significancia[Tp <= quantile(boot_statistics, 0.05/2) | Tp >= quantile(boot_statistics, 1- 0.05/2)] <- "**"
  # Significancia al 1%
  significancia[Tp <= quantile(boot_statistics, 0.01/2) | Tp >= quantile(boot_statistics, 1- 0.01/2)] <- "**"
  # Dataframe que reune el estadistico junto a la significancia
  resultado_boot <- data.frame("Estadistico Patell" = Tp, "Significancia"= significancia)
  return(resultado_boot)
}

#---------------------------------------------------------------------------------------#

#------------------------------   21. Corrado_Zivney   ----- --------------------------#
# Realizar el test no parametrico de Corrado y Zivney (1992) siguiendo la formulacion de
# Pynnonen (2022) para CAAR
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- data.list        : lista generada por la funcion <estimation.event.study>, que incluye para 
#--                    cada par evento-indice un dataframe con retornos anormales
#-- es.window.length : tamaño ventana de estimacion
#-- ev.window.length : tamaño ventana de evento
#-- no.simul         : numero de simulaciones para el bootstrap
# ----Argumentos de salida  ----#
#-- result           : dataframe con el estadistico de Patell y su significancia (* para 10%, ** para 5% y *** para 1%)
#---------------------------------------------------------------------------------------#

corrado_zivney <- function(data.list,es.window.length,ev.window.length){
  # Establecer variables para guardar los rangos de retornos anormales
  full_rank  <- NULL
  event_rank <- NULL
  for(element in data.list){
    # Generar el ranking de los retornos anormales para toda los retornos anormales de la ventana de estimacion y evento
    # Acceder a los datos del objeto zoo con la funcion <coredata>. Ranking sin importar el signo
    element_full_rank  <- rank(zoo::coredata(element$Dataframe$Abnormal[1:(es.window.length+ev.window.length)]), 
                               na.last="keep", ties.method = "average")
    
    # Guardar el ranking exclusivamente para los retornos anormales de la ventana de evento 
    # (<es.window.length> + 1 a <es.window.length> + <ev.window.length>)
    element_event_rank <- element_full_rank[(es.window.length+1):(es.window.length+ev.window.length)] 
    
    # Generar matrices de rankings de los retornos anormales para todos los objetos en <data.list>
    if(is.null(full_rank)){
      full_rank <- element_full_rank
    }else{
      full_rank <- cbind(full_rank, element_full_rank)
    }
    if(is.null(event_rank)){
      event_rank <- element_event_rank
    }else{
      event_rank <- cbind(event_rank,element_event_rank)
    }
  }
  
  # Para cada columna de full_rank tenemos que restarle el valor esperado de K_{it}, siguiendo la ecuacion 44 de Pynnonen (2022), teniendo en cuenta que 
  # E[K_{it}] = (T_i'+1)/2 de acuerdo a Campbell y Wasley (1993), donde T_1' es el numero de retornos para la firma i en toda la muestra 
  full_rank_reduced <- apply(full_rank, 2, function(x) x - ((nrow(full_rank) + 1) / 2))
  
  # Generar k promedio, siguiendo la ecuacion 44 de Pynnonen (2022)
  neventos <- ncol(full_rank_reduced)
  average_rank <- apply(full_rank_reduced, 1, function(x) sum(x, na.rm = TRUE)/neventos)
  
  # Rangos promedio para la ventana de evento para usar la ecuacion 43 de Pynnonen (2022)
  average_event_rank <- average_rank[(es.window.length+1):(es.window.length+ev.window.length)]
  
  # El numerador del estadistico z_{cw} es entonces
  num_stat <- sum(average_event_rank)
  
  # Desviacion estandar de la ecuacion 45 de Pynnonen (2022). 
  sk <- sqrt((1/(es.window.length+ev.window.length))*sum(average_rank^2))
  
  # Denominador del estadistico z_{cw}
  den_stat <- sk*sqrt(ev.window.length)
  
  # El estadistico es
  stat <- num_stat/den_stat
  
  # Comparar con los valores criticos al 10%, 5% y 1% de normal estandar
  
  significancia <- ""
  significancia[stat <= qnorm(0.1/2) | stat >= qnorm(1- 0.1/2)] <- "*"
  # Significancia al 5%
  significancia[stat <= qnorm(0.05/2) | stat >= qnorm(1- 0.05/2)] <- "**"
  # Significancia al 1%
  significancia[stat <= qnorm(0.01/2) | stat >= qnorm(1- 0.01/2)] <- "**"
  
  result <- cbind(round(stat,4),significancia)
  colnames(result) <- c("Statistic","Significance")
  return(result)
}

#---------------------------------------------------------------------------------------#

#------------------------------   21. wilcoxon_Pagnottoni   ----- --------------------------#
# Realizar el test no parametrico de Wilcoxon para los CAR de cada SUR estimado
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- coefficients.list: lista que contiene los coeficientes estimados para los SUR
#-- name.variable    : nombre de una columna de <dataframe_wilcoxon>, que es el resultado de la funcion
#-- pattern.step     : vector de caracteres que indican los coeficientes relacionados con retornos anormales
#-- pattern.indexes  : vector caracter que indica el nombre de los indices de interes
#-- pattern.variable : vector caracter que indica el nivel de agregacion de los CAR, puede ser por tipo de 
#--                    desastre o por pais donde ocurrio el desastre
# ----Argumentos de salida  ----#
#-- dataframe_wilcoxon : dataframe que va a incluir el pais o tipo de desastre, su retorno anormal acumulado
#                        promedio (CAAR), su estadistico de Wilcoxon, su p_valor y su significancia
#---------------------------------------------------------------------------------------#

wilcoxon_Pagnottoni <- function(coefficients.list,name.variable,pattern.step,pattern.indexes,pattern.variable){
  # Dataframe que va a guardar el tipo de desastre/pais, el CAAR, el estadistico de wilcoxon, el p value y la significancia
  dataframe_wilcoxon <- data.frame() 
  
  # Filtrar por aquellos que acaben en cualquier valor de <pattern.step>.
  for (element in coefficients.list){
    ## Genera un dataframe con el estimado, error estandar, t_value, p_value, que salen de la estimacion element
    dataframe_coef <- as.data.frame(element) 
    ## Le cambiamos nombres al dataframe para mejor manejo, pero reflejan lo mismo
    colnames(dataframe_coef) <- c("Estimate","SD_error","t_value","p_value")
    ## Extraemos las filas que nos interesan, es decir aquellas de las dummies, que acaban en t0, t1, t2, t3 o t4, es decir <pattern.step>
    dataframe_coef_filtrado  <- dataframe_coef %>% 
      dplyr::filter(str_ends(row.names(.),pattern = paste(pattern.step,collapse="|")))
    
    ## Ahora para poder agregar los retornos anormales es necesario poder extraer el nombre del indice, usando la funcion
    ## <str_extract>, que extrae el primer valor identico entre dos strings. Esto permite extraer el indice, que en cada fila aparece 
    ## de primer lugar
    string_start <- stringr::str_extract(row.names(dataframe_coef_filtrado), pattern = paste(pattern.indexes,collapse="|")) 
    
    # Agrega la columna con el nombre de los indices al dataframe <dataframe_coef_filtrado>
    dataframe_coef_filtrado <- cbind(dataframe_coef_filtrado,string_start)
    
    # Agrega los retornos anormales por el indice al cual pertenecen
    CAR_df <- dataframe_coef_filtrado %>% 
      group_by(string_start) %>% 
      summarise(CAR = sum(Estimate)) # <Estimate> es el valor del retorno anormal estimado
    
    # Calcular el <CAAR>
    CAAR <- mean(CAR_df$CAR)
    
    # Usar la funcion <stringr::str_extract_all> para determinar cual es el tipo de desastre 
    matches       <- stringr::str_extract_all(row.names(dataframe_coef_filtrado),paste(pattern.variable,collapse = "|"))
    type_disaster <- unique(unlist(matches))
    # Detener la funcion si hay mas de un tipo de desastre o mas de un pais
    if(length(type_disaster)!=1) stop("La agregacion de retornos anormales solo se puede hacer para un pais/un tipo de desastre")
    
    # Se procede a realizar el test de Wilcoxon
    # Se genera un dataframe para poder realizar el ordenamiento de los car
    df_car <- data.frame("car"=CAR_df$CAR,"magnitude"=abs(CAR_df$CAR),"sign"=sign(CAR_df$CAR))
    df_car <- df_car %>% 
      mutate(magnitude=ifelse(magnitude==0,NA,magnitude)) ## Se coloca NA si la magnitud es 0, ya que no se deben considerar
    # Funcion <rank> para ordenar las magnitudes de los car
    df_car <- df_car %>% 
      mutate(rank = rank(magnitude))
    
    # Suma de los rangos de car positivos: <positive_rank_sum>
    rank_sum <- df_car %>% 
      dplyr::group_by(sign) %>%
      dplyr::summarize(sum = sum(rank))
    positive_rank_sum <- rank_sum$sum[rank_sum$sign=="1"]
    
    # Colocarle la sumatoria de rango 0 si no hubo ningun CAR positivo
    if(length(positive_rank_sum)==0) positive_rank_sum <- 0
    
    # Calculo de la significancia del estadistico de Wilcoxon
    # La funcion para hallar los cuantiles de la distribucion del estadistico de Wilcoxon es <stats::qsignrank()>. 
    # A partir de 1000 observaciones, la funcion no se comporta adecuadamente, pero debido a que es muestra grande, la distribucion
    # converge a una normal con media N(N+1)/4 y varianza N(N+1)(2N+1)/24 por lo que usamos <stats::qnorm()>>
    significance <- ""
    ## Calculo para cada nivel de significancia si el valor de <positive_rank_sum> es lo suficientemente extremo para rechazar H_0
    #  La prueba se hace a dos colas, por lo que la primera condicion para cada prueba de significancia compara <positive_rank_sum> con el 
    #  valor critico de la cola derecha.
    #  La segunda condicion es la comparacion de <positive_rank_sum> con el valor critico de la cola izquierda.
    N <- nrow(df_car)
    
    # Uso de <qsignrank> o <qnorm> dependiendo del tamaño de la muestra
    if(N<=1000){
      # Si se evalua la significancia al 10%, con un test a dos colas, debemos buscar los percentiles 5 y 95, y comparar con estadistico
      significance[positive_rank_sum >= stats::qsignrank(1 - 0.1/2,n=N)|
                     positive_rank_sum <= stats::qsignrank(0.1/2, n=N)] <- "*"
      # Para evaluar al 5%:
      significance[positive_rank_sum >= stats::qsignrank(1 - 0.05/2, n=N)|
                     positive_rank_sum <= stats::qsignrank(0.05/2, n=N)] <- "**"
      # Al 1%:
      significance[positive_rank_sum >= stats::qsignrank(1 - 0.01/2, n=N)|
                     positive_rank_sum <= stats::qsignrank(0.01/2, n=N)] <- "***"
    }else{
      mu = N*(N+1)/4
      sigma = sqrt(N*(N+1)*(2*N+1)/24)
      # Si se evalua la significancia al 10%, con un test a dos colas, debemos buscar los percentiles 5 y 95, y comparar con estadistico
      significance[positive_rank_sum >= stats::qnorm(1 - 0.1/2,mean = mu,sd = sigma)|
                     positive_rank_sum <= stats::qnorm(0.1/2, mean = mu,sd = sigma)] <- "*"
      # Para evaluar al 5%:
      significance[positive_rank_sum >= stats::qnorm(1 - 0.05/2, mean = mu,sd = sigma)|
                     positive_rank_sum <= stats::qnorm(0.05/2, mean = mu,sd = sigma)] <- "**"
      # Al 1%:
      significance[positive_rank_sum >= stats::qnorm(1 - 0.01/2, mean = mu,sd = sigma)|
                     positive_rank_sum <= stats::qnorm(0.01/2, mean = mu,sd = sigma)] <- "***"
    }
    # Por ultimo, usando la funcion <wilcox.test> obtenemos el pvalor
    p_value <- wilcox.test(CAR_df$CAR)$p.value
    # Agregamos las variables a <dataframe_wilcoxon>
    dataframe_wilcoxon <- rbind(dataframe_wilcoxon,c(type_disaster,round(CAAR,4),positive_rank_sum,round(p_value,4),significance))
  }
  colnames(dataframe_wilcoxon) <- c(name.variable, "CAAR","Wilcoxon_statistic","p_value","Significance")
  return(dataframe_wilcoxon)
}

#---------------------------------------------------------------------------------------#
