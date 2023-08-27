# Los siguientes argumentos van a filtrar los resultados y tablas
serie             <- 'Indices'  #<<<--- puede ser 'Indices' o 'CDS'
tipo.estudio      <- 'media' #<<<--- puede ser 'media' o 'varianza'
regresor.mercado  <- 'PM'       #<<<--- puede ser 'PM' o 'benchmark', para CDS todavia no hay benchmark
umbral.del.evento <- 200         #<<<--- puede ser 50 100 o 200
estimation.window <- 200        #<<<--- Para media puede ser 200, 300 o 500. Para CDS solamente 500
columnas.tabla    <- 'tipodesastre' #<<<--- Las tablas de la media estan guardadas tanto por tipo de desastre como por pais
# <columnas.tabla> toma el valor de 'tipodesastre' o 'pais'

if(tipo.estudio == 'media') 
  load(file=paste0(getwd(),'/Resultados_regresion/',serie,'_tra',umbral.del.evento,'_est',estimation.window,'_',tipo.estudio,'_',regresor.mercado,'.RData'))

if(tipo.estudio == 'varianza') 
  load(file=paste0(getwd(),'/Resultados_regresion/',serie,'_tra',umbral.del.evento,'_est',estimation.window,'_',tipo.estudio,'_',regresor.mercado,'.RData'))

# Posteriormente al <load> se tiene la lista <all_events_list>, la cual tiene objetos de clase 'ESmean', la cual fue creada
# con la  funcion <estimation.event.study>.Cada uno de estos objetos tiene diversos slots a los cuales se puede acceder usando 
# @. Los slots son : 
# - <retornos>                   : data.frame con retornos observados, estimados y anormales para toda la ventana de estimacion y evento
# - <error_estandar>             : error_estandar de la estimacion, exclusivamente cuando no hay GARCH
# - <res_estandar_estimacion>    : residuales estandar de la estimacion, exclusivamente cuando hay GARCH
# - <res_no_estandar_estimacion> : residuales no estandar de la estimacion, exclusivamente cuando hay GARCH
# - <variance_forecast>          : prediccion de la varianza en la ventana de evento
# - <evento>                     : informacion del desastre
# - <fit>                        : lista de los resultados que se extraen de la funcion <ugarchfit>, o <ugarchfilter>

# Por otro lado, si el <load> se hizo para un evento de <tipo.estudio> == 'varianza', vamos a tener la lista
# <volatility_results>, la cual tiene objetos de clase 'ESVolatility', la cual fue creada por la funcion <volatility_event_study>
# Cada uno de estos objetos tiene diversos slots a los cuales se puede acceder usando @. Los slots son:
# - <coefficients>               : coeficientes de la estimacion ARIMA-GARCH
# - <goodness_of_fit>            : pvalues de bondad de ajuste a la distribucion teorica de los residuales
# - <res_estandar_estimacion>    : residuales estandar de la estimacion, exclusivamente cuando hay GARCH
# - <res_no_estandar_estimacion> : residuales no estandar de la estimacion, exclusivamente cuando hay GARCH
# - <variance_forecast>          : prediccion de la varianza en la ventana de evento
# - <residuales_evento>          : retorno anormal durante la ventana de evento
# - <info.evento>                : informacion del evento

# tipo.estudio == 'media' -------------------------------------------------
# Se puede hacer un load de las tablas de significancia <table.wilcoxon> y <table.bmp>
if(tipo.estudio == 'media'){
  load((file=paste0(getwd(),'/Resultados_regresion/Tablas/Tablas_',serie,'_tra',umbral.del.evento,'_est',
                    estimation.window,'_',tipo.estudio,'_',regresor.mercado,'_',columnas.tabla,'.RData')))
  # Todas las tablas guardadas fueron hechas excluyendo los desastres biologicos y climatologicos por falta de 
  # desastres en la muestra
  # Colocar el numero de desastres por cada columna
  if(1){
    tipos.desastre.eliminar <- c('Biological','Climatological') #<<<--- NULL si no se desea eliminar ningun tipo de desastre 
    countries   <- c('Brazil','Chile','China','Colombia','Indonesia','Korea','Malaysia','Mexico','Peru',
                     'SouthAfrica','Turkey') #<<<--- Lista de los paises de cada CDS/indice
    paises.resultados       <- countries # Seleccionar los paises sobre los cuales se quiere hacer el analisis de resultados. <countries> si se desea
    # de todos los paises de los que se tiene informacion
    eventos.fecha.exac      <- T  #<<<--- booleano para indicar si se quieren usar solamente los eventos que tengan una fecha exacta
    # <T> en caso de querer solo los que tienen fecha exacta.<F>si se quieren usar tambien aquellos eventos de
    # los que se asumio el dia
    if(columnas.tabla == 'pais') {
      columna.agrupar  <- 'Country'  #<<<--- Columna del evento por la cual se quiere separar la lista de regresiones para las tablas/graficas
      # 'Country' la separa por pais donde sucedio el desastre y 'Disaster.Subgroup' por el tipo de desastre
    }else{columna.agrupar <- 'Disaster.Subgroup'}
    suppressWarnings(all.events.list <- purrr::discard(all_events_list,is.na))
    
    # Conservar solamente los eventos geofisicos, meteorologicos e hidrologicos 
    if(!is.null(tipos.desastre.eliminar)) all.events.list <- all.events.list[!(purrr::map(all.events.list, ~.x@evento$Disaster.Subgroup) %in% tipos.desastre.eliminar)]
    # Dejar solamente los eventos que tengan fecha exacta, para el caso en que eventos.fecha.exac == T
    if(eventos.fecha.exac) all.events.list <- purrr::keep(all.events.list, ~ .x@evento$na_start == 0)
    # Dejar solo los eventos que hayan sucedido en algun pais de <paises.resultados>
    all.events.list <- all.events.list[purrr::map(all.events.list,~.x@evento$Country) %in% paises.resultados]
    lista.separada <- split(all.events.list, sapply(all.events.list, function(x) x@evento[[columna.agrupar]]))
    
    # Por ultimo, agregamos el numero de eventos por cada columna
    colnames(dataframe.bmp)[2:(ncol(dataframe.bmp)-1)] <- paste(colnames(dataframe.bmp)[2:(ncol(dataframe.bmp)-1)],
                                                                unlist(lapply(lista.separada,FUN = length)))
    colnames(dataframe.bmp)[ncol(dataframe.bmp)] <- paste(colnames(dataframe.bmp)[ncol(dataframe.bmp)],
                                                          length(all.events.list))
    colnames(dataframe.wilcoxon)[2:(ncol(dataframe.wilcoxon)-1)] <- paste(colnames(dataframe.wilcoxon)[2:(ncol(dataframe.wilcoxon)-1)],
                                                                          unlist(lapply(lista.separada,FUN = length)))
    colnames(dataframe.wilcoxon)[ncol(dataframe.wilcoxon)] <- paste(colnames(dataframe.wilcoxon)[ncol(dataframe.wilcoxon)],
                                                                    length(all.events.list))
  }
  print(dataframe.bmp)
  print(dataframe.wilcoxon)
}

# tipo.estudio == 'varianza' ---------------------------------------------
# Se puede hacer un load de las tablas de significancia <table.wilcoxon> y <table.bmp>
if(tipo.estudio == 'varianza'){
  load((file=paste0(getwd(),'/Resultados_regresion/Tablas/Tablas_',serie,'_tra',umbral.del.evento,'_est',
                    estimation.window,'_',tipo.estudio,'_',regresor.mercado,'_',columnas.tabla,'.RData')))
  # Todas las tablas guardadas fueron hechas excluyendo los desastres biologicos y climatologicos por falta de 
  # desastres en la muestra
  if(1){
    tipos.desastre.eliminar <- c('Biological','Climatological') #<<<--- NULL si no se desea eliminar ningun tipo de desastre 
    countries   <- c('Brazil','Chile','China','Colombia','Indonesia','Korea','Malaysia','Mexico','Peru',
                     'SouthAfrica','Turkey') #<<<--- Lista de los paises de cada CDS/indice
    paises.resultados       <- countries # Seleccionar los paises sobre los cuales se quiere hacer el analisis de resultados. <countries> si se desea
    # de todos los paises de los que se tiene informacion
    eventos.fecha.exac      <- T  #<<<--- booleano para indicar si se quieren usar solamente los eventos que tengan una fecha exacta
    # <T> en caso de querer solo los que tienen fecha exacta.<F>si se quieren usar tambien aquellos eventos de
    # los que se asumio el dia
    # Necesitamos el numero por desastre
    # Eliminar objetos NA
    volatility_results <- purrr::discard(volatility_results,is.na)
    # Dejar solamente los eventos que tengan fecha exacta, para el caso en que eventos.fecha.exac == T
    if(eventos.fecha.exac) volatility_results <- purrr::keep(volatility_results, ~ .x@info.evento$na_start == 0)
    # Conservar solamente los eventos geofisicos, meteorologicos e hidrologicos 
    if(!is.null(tipos.desastre.eliminar)) volatility_results <- volatility_results[!(purrr::map(volatility_results, ~.x@info.evento$Disaster.Subgroup) %in% tipos.desastre.eliminar)]
    if(columnas.tabla == 'pais') {
      columna.agrupar  <- 'Country'  #<<<--- Columna del evento por la cual se quiere separar la lista de regresiones para las tablas/graficas
      # 'Country' la separa por pais donde sucedio el desastre y 'Disaster.Subgroup' por el tipo de desastre
    }else{columna.agrupar <- 'Disaster.Subgroup'}
    v.lista.separada   <- split(volatility_results, sapply(volatility_results, function(x) x@info.evento[[columna.agrupar]]))
    
    # Renombrar las columnas
    colnames(dataframe.volatilidad)[2:(length(dataframe.volatilidad)-1)] <- paste(colnames(dataframe.volatilidad)[2:(length(dataframe.volatilidad)-1)],
                                                                                  unlist(lapply(v.lista.separada,FUN = length)))
    colnames(dataframe.volatilidad)[length(dataframe.volatilidad)] <- paste(colnames(dataframe.volatilidad)[length(dataframe.volatilidad)],
                                                                            length(volatility_results))
  }
  dataframe.volatilidad
}


