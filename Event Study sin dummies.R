# Para el análisis de Pagnottoni (2022) se utilizaba una base con la muestra reducida a <dia.inicial>, lo 
# cual no se hace para Tommaso (2023), ya que necesitamos la mayor cantidad posible de datos antes de 
# <dia.inicial>. 
# En <base_Tommaso> estan las series que van a servir como variables dependientes, un indice de mercado,
# y las variables exogenas del modelo.
base_Tommaso <- merge(base_retornos,mean_mov_average,gdp_growth_base,fdi_growth_base)
# Por construccion, <base_Tommaso> tiene dos columnas para Estados Unidos para gdp y fdi: <gdp_USA1>/<gdp_USA2> y <fdi_USA1>/<fdi_USA2>
# Dejar solamente una columna para gdp y una para fdi que se llamen <gdp_USA> y <fdi_USA>
base_Tommaso <- base_Tommaso[,!colnames(base_Tommaso) %in% c('gdp_USA2','fdi_USA2')]
colnames(base_Tommaso) <- gsub("USA1$", "USA", colnames(base_Tommaso))

# Parametros event study --------------------------------------------------------------

estimation_start         <- 150  #<<<--- No. de dias antes del evento para comenzar la estimacion
estimation_end           <- 1   #<<<--- No. dias antes del evento para finalizar la estimacion
max_abnormal_returns     <- 15   #<<<--- No. dias maximos despues del evento para calcular retorno anormal
days_to_be_evaluated     <- 5    #<<<--- No. dias despues del evento a ser evaluados
length_car_window        <- 10   #<<<--- Ventana para calcular el CAR (por ejemplo 5 significa [0,+5], donde 0 es el dia del evento)
length_estimation_window <- estimation_start - estimation_end + 1 # Tamaño de la ventana de estimacion
length_event_window      <- length_car_window + 1 # Longitud ventana de evento es 1 mas <length_car_window>
# <length_car_window> no puede ser mayor a <max_abnormal_returns>, ya que implica una ventana de evento mayor al numero de retornos
# anormales estimados.
# Asegurar que la ventana de evento no sea mayor que los retornos anormales estimados. 
if(length_car_window > max_abnormal_returns) length_car_window <- max_abnormal_returns

# Otros parametros --------------------------------------------------------

paises.pagnottoni <- c("Australia","Belgium", "Brazil", "Canada", "Chile", "Denmark", "Finland",
                       "France", "Germany", "HongKong", "India", "Indonesia","Mexico","Netherlands","Norway","Poland","Russia",
                       "SouthAfrica","SouthKorea", "Spain", "Sweden","Switzerland","Thailand","Turkey", 
                       "UnitedKingdom","USA") #<<<--- Paises usados en Pagnottoni
# Si <unico_pais> es NULL, se obtiene una base de datos con los eventos de todos los paises de <paises.pagnottoni>
# En caso de querer analizar solamente un pais, se escribe en <unico_pais>
unico_pais <- NULL

# Base de eventos ---------------------------------------------------------

# Lectura de y filtros de la base de eventos <emdat_completa>. 
if(1){
  # Lectura de la base de datos <EMDAT>,  en excel, se dejaron los desastres entre el 8-feb-2001 y 31-dic-2019 (fechas usadas en el paper).
  emdat_completa     <- openxlsx::read.xlsx(paste0(Dir,"EMDAT_Mapamundi.xlsx"),sheet = "Mapamundi") #<<<--- base de datos 
  # Correccion del nombre de algunos paises en <emdat_base>
  emdat_completa <- emdat_completa %>% 
    mutate(Country = case_when(
      Country == "United Kingdom of Great Britain and Northern Ireland (the)" ~ "UnitedKingdom",
      Country == "United States of America (the)" ~ "USA",
      Country == "Hong Kong" ~ "HongKong",
      Country == "Netherlands (the)" ~ "Netherlands",
      Country == "Russian Federation (the)" ~ "Russia",
      Country == "South Africa" ~ "SouthAfrica",
      Country == "Korea (the Republic of)" ~ "SouthKorea",
      TRUE ~ Country
    ))
  # Se seleccionan las columnas de interes
  if(0){
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
  }
  emdat_base <- emdat_completa %>% 
    dplyr::select('Disaster.Subgroup','Disaster.Type','Disaster.Subtype','Country','Start.Year','Start.Month','Start.Day','End.Year','End.Month',
                  'End.Day','Total.Deaths','No.Injured','No.Affected','No.Homeless','Total.Affected',
                  Damages = "Total.Damages,.Adjusted.('000.US$)")
  
  # Generacion de <Start.Date> para cada evento  ---------------------------------------------
  
  # Algunas de las fechas tienen NA en el dia, por lo cual se asume que es el primer dia del mes.
  # En estos casos, la variable dummy <na_start> es igual a 1, o.w. es 0.
  if(sum(is.na(emdat_base$Start.Day))!=0){
    warning("Hay dias faltantes en la base de datos! Se va a asumir que el dia de inicio del desastre es el primero del mes")
    # Creacion de la variable <na_start>
    emdat_base <- emdat_base %>%
      mutate(na_start = ifelse(is.na(Start.Day),1,0))
    # Cambiar valores <NA> por el primer dia del mes
    emdat_base <- emdat_base %>% 
      mutate(Start.Day=replace_na(Start.Day,1)) # <replace_na> se utiliza para reemplazar los valores <NA> por <1>
  }
  # Generacion de la fecha completa del inicio de evento, <Start.Date>, 
  # a partir de <Start.Year>, <Start.Month> y <Start.Day>
  emdat_base <- emdat_base %>% 
    unite(Start.Date, c(Start.Year, Start.Month, Start.Day), sep = "-",remove=FALSE) %>% 
    mutate(Start.Date = as.Date(Start.Date))
  
  # Vector con los nombres de paises usados para la generacion de la base de eventos
  if(is.null(unico_pais)){
    paises.usados <- paises.pagnottoni 
  }else{
    paises.usados <- unico_pais
  }
  
  # Filtrar la base solo por los paises de <paises.usados>
  emdat_base <- emdat_base %>% 
    dplyr::filter(Country %in% paises.usados)
  
  # Dataframe eventos -------------------------------------------------------
  
  # Generacion de un dataframe solo con dos variables: <Country> y <Start.Date>
  eventos <- emdat_base %>% 
    dplyr::select(Country,Start.Date)
}

# El codigo anterior genera una base de datos con variables:
#    <Country>    : pais donde sucede el evento
#    <Start.Date> : fecha de inicio del evento


# Agregar rezagos a base de datos -----------------------------------------

# Se usa la funcion <create.lags>, que toma una base de datos junto a unas variables de interes. Retorna la base de datos inicial junto con 
# rezagos de las variables de interes. Si se desea un numero de rezagos en especifico para todas las variables de interes, asignar el numero 
# a <number_lags>. Si se desea un numero de rezagos para cada variable de interes, asignar una lista a <number_lags> con los numeros de 
# rezagos. Nota: Si se coloca la lista tiene que tener el mismo numero de datos que numero de variables de interes.
# Si se desea que se elijan los rezagos siguiendo el criterio de informacion de Akaike, dejar <number_lags> como NULL
number_lags <- NULL
base_lagged <- create.lags(base = base_Tommaso,interest.vars = indexes,no.lags = number_lags,AR.m = 20)

# Se eliminan los eventos que no cuentan con la ventana minima de estimacion ni con la ventana minima de evento usando la funcion <drop.events>
date_col_name <- "Start.Date" #<<<--- Parametro que indica el nombre de la columna clase <Date>, la cual contiene la fecha de eventos
geo_col_name  <- "Country"    #<<<--- Parametro que indica el nombre de la columna que contiene los paises, o puede ser cualquier otra variable 
                              #       que se quiera estudiar, como regiones, ciudades, etc
eventos_filtrado <- drop.events(data.events = eventos,base = base_lagged,estimation.start = estimation_start,max.ar=max_abnormal_returns, 
                       date_col_name, geo_col_name)

# -------------------------- Regresion estimation window ---------------------------------------------

# <estimation.event.study> realiza la estimacion por OLS para cada evento en <data.events>. Retorna una lista para cada evento que incluye:
#     Dataframe      : retornos observados, estimados y anormales para la ventana de estimacion y ventana de evento. 
#     Standard_error : error estandar de los errores de la estimacion por OLS
# El objeto de salida de esta funcion sera la base para las pruebas de Wilcoxon y bootstrap

#Ya no es necesario. <create_lags> genera automaticamente la base de datos con los rezagos deseados
if(0){
  # Para incluir rezagos de la dependiente, escribir el nombre de las bases de rezagos, por ej: "lags_", sin incluir el pais
  # Si no se desea incluir rezagos, dejar como NULL
  lags.base <- "lags_"
}

# Otras variables exogenas de una base de datos que se quieren incluir. 
var_exo <- c("gdp_","fdi_")

all_events_list <- estimation.event.study(base = base_lagged,data.events = eventos_filtrado[3:nrow(eventos_filtrado),],days.evaluated = 5,market.returns = "Mean_Returns_Moving_Averages",
                                           max.ar = 15,es.start = estimation_start,es.end = estimation_end,add.exo = TRUE,vars.exo = var_exo)

# Wilcoxon --------------------------------------------------------------

# Usamos la funcion <wilcoxon.jp.test> para realizar la prueba de Wilcoxon asociada a los <CAR> 
# de los eventos. Obtenemos el estadistico junto a su significancia
# <Significancia> = "*" indica que el estadistico es significativo al 10%, "**" al 5% y "***" al 1%.
# <Significancia> = " "  indica que el estadistico no es significativo a ningun nivel convencional
# Aparte, se usa la funcion <stats::wilcox.test> para obtener el p-valor
# La prueba que realiza wilcoxon.jp.test es a dos colas
wilcoxon.resultado <- wilcoxon.jp.test(data.list = all_events_list,es.window.length = length_estimation_window,
                                       ev.window.length = length_event_window);wilcoxon.resultado

# Bootstrap CAAR ----------------------------------------------------------

# Para hacer el procedimiento por Bootstrap se sigue el procedimiento usado por Corrado & Truong (2008)
boot_n_simul <- 1000 #<<<--- parametro que indica el numero de repeticiones para bootstrapping

bootstrap.resultado <- bootstrap_CT(data.list = all_events_list,market.returns=Media_Promedio_movil,
                                    es.window.length = length_estimation_window, ev.window.length = length_event_window,
                                    no.simul = boot_n_simul);bootstrap.resultado

# Corrado and Zivney rank test --------------------------------------------

corrado.resultado <- corrado_zivney(data.list = all_events_list,es.window.length = length_estimation_window,
                                    ev.window.length = length_event_window); corrado.resultado


# Volatility event study --------------------------------------------------

# El siguiente programa sigue la metodologia del paper The impact of natural disasters on stock returns and volatilities
# of local firms (Bourdeau-Brien)

estimation_vol_start <- 500 #<<<-- ventana para la estimacion de la volatilidad previa al evento. 
vol_ev_window <- 15  #<<<--- Tamaño de la ventana de evento

# Se eliminan los eventos que no cuentan con la ventana minima de estimacion ni con la ventana minima de evento usando la funcion <drop.events>
eventos_filtrado <- drop.events(data.events = eventos,base = base_lagged,estimation.start = estimation_vol_start,max.ar=vol_ev_window, 
                                date_col_name, geo_col_name)

# Agregar columna a <eventos_filtrado>, ya que no deberian haber desastres con 500 dias de diferencia
# Se agrega la columna que indica el numero que le corresponde al dia cuando sucedio
index_vector <- c()
for(i in 1:nrow(eventos_filtrado)){
  for(j in 0:days_to_be_evaluated){
    if((eventos_filtrado[i,'Start.Date']+j) %in% index(base_Tommaso)){ 
      indice <- which(index(base_Tommaso)==eventos_filtrado[i,'Start.Date']+j)
      index_vector <- c(index_vector,indice)
      break
    }
  }
}

eventos_filtrado <- cbind(eventos_filtrado, index_vector)

overlap_parameter <- 100 #<<<--- Tamaño de los intervalos para asegurar que las ventanas de estimacion no se sobrepongan

# Crear las categorias. Para cada una de ellas solamente va a quedar un evento por cada país (o 0 si en el evento no ocurrieron
# desastres durante la categoría)
h      <- 0:ceiling((max(eventos_filtrado$index_vector)-estimation_vol_start)/overlap_parameter)
breaks <- estimation_vol_start + overlap_parameter*h

# Dividir los eventos en las categorias
eventos_filtrado <- eventos_filtrado %>%
  mutate(intervalo = cut(index_vector, breaks = breaks, labels = FALSE, include.lowest = TRUE))

# Por cada pais-categoria solamente puede quedar un evento, con fin de que las ventanas de estimacion no tengan un traslape
# Todavia no se ha determinado cual de ellos debe mantenerse para el analisis, por ahora es el primero que sucede en 
# la categoria
eventos_filtrado <- eventos_filtrado %>%
  group_by(Country) %>%
  distinct(Country, intervalo, .keep_all = TRUE)

#<if(0)> porque todo lo que sigue fue integrado en la funcion <volatility_event_study>
if(0){
  # -------------------------------------------------------------------------
  
  test <- apply(eventos_filtrado, MARGIN = 1,FUN = )
  
  evento_prueba <- eventos_filtrado[5,]
  dia_evento    <- evento_prueba[2]
  pais_evento   <- evento_prueba[1]
  
  indice_del_evento   <- which(index(base_Tommaso)==dia_evento)
  t_menos_1           <- indice_del_evento -1
  t_menos_es_window   <- indice_del_evento - estimation_vol_start
  
  # Estimacion APARCH con modelo de media ARMA -------------------------------
  
  # Generamos un dataframe que incluya los ordenes de rezagos para cada variable de interes, stock (Pagnottoni) o CDS
  # Tambien se tendra un parametro que le permita al usuario decidir cuantos rezagos desea en especifico para todas las variables de interes.
  # Si se desa el mismo numero de rezagos para todas las variables de interes, asignar el numero a <num_lags>. Si se desea un numero de 
  # rezagos para cada variable de interes, asignar una lista a <num_lags> con los numeros de rezagos. 
  # Nota: Si se coloca la lista, tiene que tener el mismo numero de datos que numero de variables de interes.
  
  # Si se desea que se elijan los rezagos siguiendo el criterio de informacion de Akaike, dejar <num_lags> como NULL
  num_lags <- NULL #<<<--- parametro para decidir cuantos rezagos agregar al modelo de la media en el modelo ARMA-GARCH
  lags_database <- arma_lags_database(base=base_Tommaso,interest.vars=indexes,num_lags, AR.m = 20, MA.m = 0,d = 0,
                                      bool = TRUE,metodo = "CSS")
  
  # Se obtiene primero el indice del pais donde sucedio el desastre
  indice_s <- matching(pais_evento)[1] # El 1 solamente se coloca porque con la base de Pagnottoni, USA tiene dos stocks
                                       # Ya no es necesario escribirlo cuando se utilicen los CDS, cada pais solo tiene un CDS
  
  # Se obtienen los ordenes para el modelo ARMA(p,q) del indice
  mod_base <- arma_seleccion_df(object = base_Tommaso[,indice_s], AR.m = 20,MA.m = 0,d = 0,bool = TRUE,metodo = "CSS")
  p   <- mod_base[which.min(mod_base$AIC),'p']
  q   <- mod_base[which.min(mod_base$AIC),'q']
  
  spec <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 1)),
                     mean.model = list(armaOrder = c(p, q),
                                       external.regressors = as.matrix(base_Tommaso[(t_menos_es_window:t_menos_1),
                                                                                    c("Mean_Returns_Moving_Averages",
                                                                                      paste0("gdp_", pais_evento),
                                                                                      paste0("fdi_", pais_evento))])),
                     distribution.model = "std")#, fixed.pars = list(delta=1))
  fit <- ugarchfit(spec, data = base_Tommaso[(t_menos_es_window:t_menos_1),indice_s],solver="hybrid")
  
  # Residuales
  
  resdi <- as.numeric(residuals(fit,standardize=TRUE))
  
  # Coeficiente de asimetria
  asymmetry <- coef(fit)["gamma1"]
  
  # Funcion de distribucion t
  df <- fit@fit$coef["shape"]
  t_gof <- gnfit(resdi,"t",df) #No rechazo H0, se sigue distribucion t
  t_gof_pval <- c(t_gof$Apval,t_gof$Wpval) # Guardar el p_valor de lass pruebas Anderson-Darling y Cramer Von Misses
  names(t_gof_pval) <- c("Anderson-Darling","Cramer-Von Misses")
  
  # Forecast volatilidad condicional
  # Usando la ecuacion de Bialkowski (2008) para realizar el forecast de sigma^2
  # h_t sale de fit@fit$var, \varepsilon_t sale de fit@fit$residuals y los errores estandarizados 
  # salen de fit@fit!z
  omega         <- fit@fit$coef["omega"]
  alpha         <- fit@fit$coef["alpha1"]
  beta          <- fit@fit$coef["beta1"]
  fcast_var     <- c()
  for(k in 1:vol_ev_window){
    j <- 0:(k-1)
    fcast_var_ti  <- omega*sum((beta+alpha)^j) + (beta+alpha)^(k-1)*beta*(tail(fit@fit$var,1))+
                     (beta+alpha)^(k-1)*alpha*(tail((fit@fit$residuals)^2,1))
    fcast_var <- c(fcast_var, fcast_var_ti)
  }
  
  # Crear la serie de los residuales para la ventana de evento
  residual_evento <- base_Tommaso[(indice_del_evento:(indice_del_evento+vol_ev_window-1)),indice_s] - forecast@forecast$seriesFor
  # Duda de como generar estos residuales
  
  # Convertir en xts <fcast_var>
  fcast_var <- as.xts(fcast_var, order.by = index(residual_evento))
  
  setClass("ESVolatility",slots=list(coefficients = "numeric",goodness_of_fit = "numeric",variance_forecast="xts",residuales_evento="xts"))
  
  object <- new("ESVolatility",coefficients=coef(fit),goodness_of_fit=t_gof_pval,variance_forecast=fcast_var,residuales_evento=residual_evento)
}

#Ejemplo
start <- Sys.time()
lista_test <-volatility_event_study(base.evento = eventos_filtrado[1:10,],date.col.name = "Start.Date",geo.col.name = "Country",
                                    base.vol = base_Tommaso,interest.vars = indexes,num_lags = NULL,es.start=estimation_vol_start,
                                    len.ev.window = vol_ev_window,var.exo="Mean_Returns_Moving_Averages",var.exo.pais = c("gdp","fdi"))
end <- Sys.time()

