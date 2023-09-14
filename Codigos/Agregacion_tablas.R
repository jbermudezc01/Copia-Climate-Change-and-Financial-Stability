if(1){
  if(Sys.info()["sysname"]=='Windows') Sys.setlocale("LC_TIME","English")
  
  rm(list = ls())
  if (Sys.info()["sysname"]=='Windows')  setwd('C:/Users/jpber/OneDrive/Documents/Codigo_compartido_Melo/Climate_Change_and_Financial_Stability/Climate-Change-and-Financial-Stability')
  if (Sys.info()["sysname"]!='Windows')  setwd('/Users/lumelo/archivos/Climate-Change-and-Financial-Stability/Github/Climate-Change-and-Financial-Stability')
  
  cat("\014")
  
  # Cargar librerias --------------------------------------------------------
  
  library(tidyverse)
  library(xts)
  library(timeDate)
  library(zoo)
  library(tempdisagg)
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
  library(moments)
  library(stargazer)
  library(Hmisc)
  library(scales)
  library(vars)
  library(smoots)
  library(dynlm)
  library(systemfit)
  library(ks)
  library(knitr)
  library(gridExtra)
  library(stringr)
  library(maps)
  library(mapproj)
  library(ggthemes)
  library(tmap)
  library(sf)
  library(ggsci)
  library(classInt)
  library(gnFit)
  library(rugarch)
  library(knitr)
  library(kableExtra)
  library(janitor) # Para manejo de tablas descriptivas
  library(xtable)  # Para exportar tablas a latex 
  library(RColorBrewer)
  library(tools)
  library(writexl)  # Para crear excel
  library(readxl)
  
  # Cargar funciones --------------------------------------------------------
  source(paste0(getwd(),'/Codigos/Functions_Climate_Change.r')) # Source de las funciones
}

# Los siguientes argumentos van a filtrar los resultados y tablas
serie             <- 'CDS'      #<<<--- puede ser 'Indices' o 'CDS'
tipo.estudio      <- 'varianza'     #<<<--- puede ser 'media' o 'varianza'
regresor.mercado  <- 'PM'    #<<<--- puede ser 'PM' o 'benchmark', para CDS todavia no hay benchmark
umbrales.evento   <- c(50,100,200)  #<<<--- puede ser 50 100 o 200
if(tipo.estudio=='media') es.windows <- c(200,300,500) #<<<--- Para media puede ser 200, 300 o 500. Para varianza solamente 500
if(tipo.estudio=='varianza') es.windows <- c(500,750,1000)
columnas.tabla    <- 'tipodesastre' #<<<--- Las tablas de la media estan guardadas tanto por tipo de desastre como por pais
# <columnas.tabla> toma el valor de 'tipodesastre' o 'pais'

# Organizacion tablas -----------------------------------------------------

lista.wilcoxon  <- list()
lista.bmp       <- list()
lista.bootstrap <- list()
indice.lista   <- 0
for(i in seq_along(umbrales.evento)){
  umbral.del.evento <- umbrales.evento[i]
  for(j in seq_along(es.windows)){
    indice.lista <- indice.lista +1
    estimation.window <- es.windows[j]
    #load((file=paste0(getwd(),'/Resultados_regresion/Tablas/Tablas_',serie,'_tra',umbral.del.evento,'_est',
    #                  estimation.window,'_',tipo.estudio,'_',regresor.mercado,'_',columnas.tabla,'.RData')))
    # Cargamos las tablas con el nuevo bootstrap
    load((file=paste0(getwd(),'/Resultados_regresion/Tablas/Nuevas_Tablas_Varianza/Tablas_',serie,'_tra',umbral.del.evento,'_est',
                      estimation.window,'_',tipo.estudio,'_',regresor.mercado,'_',columnas.tabla,'.RData')))
    if(tipo.estudio=='media'){
      lista.wilcoxon[[indice.lista]] <- dataframe.wilcoxon 
      names(lista.wilcoxon)[[indice.lista]] <- paste('Estimacion',estimation.window,'traslape',umbral.del.evento,sep='_')
      lista.bmp[[indice.lista]] <- dataframe.bmp
      names(lista.bmp)[[indice.lista]] <- paste('Estimacion',estimation.window,'traslape',umbral.del.evento,sep='_')
    }
    if(tipo.estudio=='varianza'){
      lista.bootstrap[[indice.lista]] <- dataframe.volatilidad
      names(lista.bootstrap)[[indice.lista]] <- paste('Estimacion',estimation.window,'traslape',umbral.del.evento,sep='_')
    }
  }
}

# Tablas para la media ----------------------------------------------------

if(tipo.estudio == 'media'){
  tipo.evento   <- 'Meteorological' # Geophysical, Hydrological, Meteorological o Todos 
  lista.interes <- lista.wilcoxon
  dataframe.wil     <- purrr::map_dfc(lista.interes, ~.x[,tipo.evento])
  dataframe.wil200  <- dataframe.wil[,grep('Estimacion_200',colnames(dataframe.wil))] # Escoger los datos que se tienen para estimacion con 200 dias
  dataframe.wil300  <- dataframe.wil[,grep('Estimacion_300',colnames(dataframe.wil))] # Escoger los datos que se tienen para estimacion con 300 dias
  dataframe.wil500 <- dataframe.wil[,grep('Estimacion_500',colnames(dataframe.wil))] # Escoger los datos que se tienen para estimacion con 500 dias
  # Retirar nombres de columnas para hacer rbind 
  colnames(dataframe.wil200) <- NA
  colnames(dataframe.wil300) <- NA
  colnames(dataframe.wil500) <- NA
  # Juntarlos en un gran dataframe
  dataframe.wil.organizado <- rbind(dataframe.wil200,dataframe.wil300, dataframe.wil500)
  # Nombres de columnas
  colnames(dataframe.wil.organizado) <- c('est50','est100','est200')
  # Añadir columna de dias de estimacion
  dataframe.wil.organizado$`Estimacion` <- c(rep(NA,7),200,rep(NA,14),300,rep(NA,14),500,rep(NA,7))
  # Mutar las columnas <50>, <100> y <200> para agregar un '/', para poder colocar la significancia de BMP en la misma tabla
  dataframe.wil.organizado <- dataframe.wil.organizado %>% 
    mutate('50' = paste(est50,'/'), '100' = paste(est100,'/'),'200' = paste(est200,'/'))
  # Seleccionar solamente las columnas de interes
  dataframe.wil.organizado <- dataframe.wil.organizado %>% dplyr::select(Estimacion,`50`,`100`,`200`)
  
  lista.de.interes <- lista.bmp  
  data.bmp         <- purrr::map_dfc(lista.de.interes, ~.x[,tipo.evento])
  data.bmp200      <- data.bmp[,grep('Estimacion_200',colnames(data.bmp))] # Escoger los datos que se tienen para estimacion con 200 dias
  data.bmp300      <- data.bmp[,grep('Estimacion_300',colnames(data.bmp))] # Escoger los datos que se tienen para estimacion con 300 dias
  data.bmp500      <- data.bmp[,grep('Estimacion_500',colnames(data.bmp))] # Escoger los datos que se tienen para estimacion con 500 dias
  # Retirar nombres de columnas para hacer rbind 
  colnames(data.bmp200) <- NA
  colnames(data.bmp300) <- NA
  colnames(data.bmp500) <- NA
  # Juntarlos en un gran dataframe
  data.bmp.organizado <- rbind(data.bmp200,data.bmp300, data.bmp500)
  # Nombres de filas
  colnames(data.bmp.organizado) <- c('est50','est100','est200')
  # Mutar para solamente tener los * de significancia
  # Mutar para solamente tener los * de significancia
  data.bmp.organizado <- data.bmp.organizado %>% 
    mutate('50bmp' = gsub("[^*]","",est50),'100bmp'=gsub("[^*]","",est100),'200bmp' = gsub("[^*]","",est200))
  
  # Lo unico que falta es juntar los dos dataframe: <dataframe.wil.organizado> y <data.bmp.organizado> para tener un solo dataframe con la significancia de 
  # ambos tests
  dataframe.final <- cbind(dataframe.wil.organizado,data.bmp.organizado)
  dataframe.final <- dataframe.final %>% 
    mutate('50' = paste(rep(paste0('[1,',1:15,']'),3),`50`,`50bmp`),'100'=paste(rep(paste0('[1,',1:15,']'),3),`100`,`100bmp`),
           '200'=paste(rep(paste0('[1,',1:15,']'),3),`200`,`200bmp`)) %>% 
    dplyr::select(Estimacion,`50`,`100`,`200`)
  
  # Exportar a latex
  kable(dataframe.final,format='latex') 
}

# Tablas para la varianza -------------------------------------------------

if(tipo.estudio == 'varianza'){
  tipo.evento   <- 'Todos' # Geophysical, Hydrological, Meteorological o Todos 
  lista.interes <- lista.bootstrap
  dataframe.var     <- purrr::map_dfc(lista.interes, ~.x[,tipo.evento])
  dataframe.var500  <- dataframe.var[,grep('Estimacion_500',colnames(dataframe.var))] # Escoger los datos que se tienen para estimacion con 200 dias
  dataframe.var750  <- dataframe.var[,grep('Estimacion_750',colnames(dataframe.var))] # Escoger los datos que se tienen para estimacion con 300 dias
  dataframe.var1000 <- dataframe.var[,grep('Estimacion_1000',colnames(dataframe.var))] # Escoger los datos que se tienen para estimacion con 500 dias
  # Retirar nombres de columnas para hacer rbind 
  colnames(dataframe.var500) <- NA
  colnames(dataframe.var750) <- NA
  colnames(dataframe.var1000) <- NA
  # Juntarlos en un gran dataframe
  dataframe.var.organizado <- rbind(dataframe.var500,dataframe.var750, dataframe.var1000)
  # Nombres de columnas
  colnames(dataframe.var.organizado) <- c('50','100','200')
  # Añadir columna de dias de estimacion
  dataframe.var.organizado$`Estimacion` <- c(rep(NA,7),500,rep(NA,14),750,rep(NA,14),1000,rep(NA,7)) # se elige asi ya que <dataframe.var200> y <dataframe.var300> son NA
  # Reordenar las columnas
  dataframe.var.final <- dataframe.var.organizado %>% 
                           mutate('50'= paste(rep(paste0('[0,',0:14,']'),3),`50`),
                                  '100'= paste(rep(paste0('[0,',0:14,']'),3),`100`),
                                  '200'= paste(rep(paste0('[0,',0:14,']'),3),`200`)) %>% 
                           dplyr::select(Estimacion,`50`,`100`,`200`)
  
  # Exportar a latex
  kable(dataframe.var.final,format='latex') 
}
