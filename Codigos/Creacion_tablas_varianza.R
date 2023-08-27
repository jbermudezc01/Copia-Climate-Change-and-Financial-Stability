# Prueba de filtro  -------------------------------------------------------
directorio.saved        <- paste0(getwd(),'/Resultados_regresion/')
directorio.guardar      <- paste0(directorio.saved,'Tablas/')

tipo.serie              <- 'CDS'   #<<<--- Puede ser 'CDS' o 'Indices'  
ventana.estimacion      <- '500'   #<<<--- Puede ser 200, 300 o 500   (Importante que sea string)
ventana.traslape        <- '250'   #<<<--- Puede ser 50, 100 o 200   (Importante que sea string)
tipo.estudio            <- 'varianza' #<<<--- Puede ser de 'media' o 'varianza'
regresor.mercado        <- 'PM'    #<<<--- Retornos de mercado 'PM' es promedio movil y 'MSCI' es el retorno MSCI Emerging Markets

tipos.desastre.eliminar <- c('Biological','Climatological') #<<<--- NULL si no se desea eliminar ningun tipo de desastre 
paises.resultados       <- countries # Seleccionar los paises sobre los cuales se quiere hacer el analisis de resultados. <countries> si se desea
# de todos los paises de los que se tiene informacion
eventos.fecha.exac      <- T  #<<<--- booleano para indicar si se quieren usar solamente los eventos que tengan una fecha exacta
# <T> en caso de querer solo los que tienen fecha exacta.<F>si se quieren usar tambien aquellos eventos de
# los que se asumio el dia  
columna.agrupar         <- 'Disaster.Subgroup'  #<<<--- Columna del evento por la cual se quiere separar la lista de regresiones para las tablas/graficas
# 'Country' la separa por pais donde sucedio el desastre y 'Disaster.Subgroup' por el tipo de desastre
vol_ev_window           <- 15  #<<<--- Tamaño de la ventana de evento

# Volatility event study --------------------------------------------------
# load de los objetos de la varianza
load(paste0(directorio.saved,tipo.serie,'_tra',ventana.traslape,'_est',ventana.estimacion,'_',tipo.estudio,'_',regresor.mercado,'.RData'))
# Eliminar objetos NA
volatility_results <- purrr::discard(volatility_results,is.na)

# Filtracion de los resultados -------------------------------------------
# Tipo de desastre
round(table(unlist(purrr::map(volatility_results, ~.x@info.evento$Disaster.Subgroup)))/length(volatility_results),3)
# Dejar solamente los eventos que tengan fecha exacta, para el caso en que eventos.fecha.exac == T
if(eventos.fecha.exac) volatility_results <- purrr::keep(volatility_results, ~ .x@info.evento$na_start == 0)
# Conservar solamente los eventos geofisicos, meteorologicos e hidrologicos 
if(!is.null(tipos.desastre.eliminar)) volatility_results <- volatility_results[!(purrr::map(volatility_results, ~.x@info.evento$Disaster.Subgroup) %in% tipos.desastre.eliminar)]
# Dejar solo los eventos que hayan sucedido en algun pais de <paises.resultados>
volatility_results <- volatility_results[purrr::map(volatility_results,~.x@info.evento$Country) %in% paises.resultados]
# Ver cuantos desastres hay por cada valor de <columna.agrupar>. Por ejemplo: 1 biological, 3 climatological
# 37 geophysical, 110 hydrological, 31 meteorological. De este modo se observa que hay muy pocos eventos biologicos
# y climatologicos para poder realizar cualquier analisis
table(unlist(purrr::map(volatility_results,~.x@info.evento$Disaster.Subgroup)))

# Separar la lista dependiendo de una columna en especifico introducida por el usuario 
v.lista.separada <- split(volatility_results, sapply(volatility_results, function(x) x@info.evento[[columna.agrupar]]))

# Generar listas distintas para cada valor de la <columna.agrupar>, en caso de querer utilizarlas mas adelante
for (i in seq_along(v.lista.separada)) assign(paste0("v.list.", names(v.lista.separada)[i]), v.lista.separada[[i]])

# Graficas CAV ------------------------------------------------------------
for(i in seq_along(v.lista.separada)){
  element <- v.lista.separada[[i]]
  name    <- names(v.lista.separada)[i]  
  grafico_cav(element,as.numeric(ventana.estimacion),vol_ev_window)
  title(name,line=0.75)
}

# Tabla CAV/significancia ------------------------------------------------

# Dataframe con muchas ventanas
matrix.volatilidad <- matrix(nrow=(vol_ev_window),ncol=(length(v.lista.separada)+1))
iteraciones.bool  <- 5000
for(i in seq_along(v.lista.separada)){
  for(j in (1:(vol_ev_window))){
    prueba <- bootstrap.volatility(v.lista.separada[[i]],as.numeric(ventana.estimacion),j,bootstrap_vol_iterations = iteraciones.bool)
    matrix.volatilidad[j,i] <- paste(prueba$CAV,prueba$Significancia)
  }
}

k <- length(v.lista.separada)+1
for(j in (1:(vol_ev_window))){ 
  prueba.cav <- bootstrap.volatility(volatility_results,as.numeric(ventana.estimacion),j,bootstrap_vol_iterations = iteraciones.bool)
  matrix.volatilidad[j,k] <- paste(prueba.cav$CAV,prueba.cav$Significancia)
}
colnames(matrix.volatilidad) <- c(names(v.lista.separada),'Todos')
Ventana                      <- 1:(vol_ev_window)
matrix.volatilidad           <- cbind(Ventana,matrix.volatilidad)

dataframe.volatilidad        <- data.frame(matrix.volatilidad)

# Guardar las tablas de significancia. No es necesario agregar el tipo de test ya que podemos guardar ambas tablas
if(columna.agrupar=='Disaster.Subgroup') agrupacion <- 'tipodesastre'
if(columna.agrupar=='Country') agrupacion <- 'pais'
save(dataframe.volatilidad,
     file=paste0(directorio.guardar,'Tablas_',tipo.serie,'_tra',ventana.traslape,'_est',ventana.estimacion,'_',tipo.estudio,'_',regresor.mercado,'_',agrupacion,'.RData'))
