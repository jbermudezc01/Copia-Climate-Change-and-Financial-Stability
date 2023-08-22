if(1){
  if(Sys.info()["sysname"]=='Windows') Sys.setlocale("LC_TIME","English")
  
  rm(list = ls())
  if (Sys.info()["sysname"]=='Windows')  setwd('C:/Users/jpber/OneDrive/Documents/Codigo_compartido_Melo/Climate_Change_and_Financial_Stability/Climate-Change-and-Financial-Stability')
  if (Sys.info()["sysname"]!='Windows')  setwd('/Users/lumelo/archivos/Climate-Change-and-Financial-Stability/Github/Climate-Change-and-Financial-Stability')
  
  cat("\014")
  
  # Cargar librerias --------------------------------------------------------
  
  library(tidyverse)
  library(lubridate)
  library(data.table)
  library(xts)
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
  library(moments)
  library(stargazer)
  library(Hmisc)
  library(scales)
  library(vars)
  library(smoots)
  library(dynlm)
  library(systemfit)
  library(ks)
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
  
  # Cargar funciones --------------------------------------------------------
  
  source('Functions_Climate_change.r')
}

# Los siguientes argumentos van a filtrar los resultados y tablas
serie             <- 'Indices'      #<<<--- puede ser 'Indices' o 'CDS'
tipo.estudio      <- 'media'        #<<<--- puede ser 'media' o 'varianza'
regresor.mercado  <- 'PM'           #<<<--- puede ser 'PM' o 'benchmark', para CDS todavia no hay benchmark
umbrales.evento   <- c(50,100,200)  #<<<--- puede ser 50 100 o 200
if(tipo.estudio=='media') es.windows <- c(200,300,500) #<<<--- Para media puede ser 200, 300 o 500. Para varianza solamente 500
if(tipo.estudio=='varianza') es.windows <- 500
columnas.tabla    <- 'tipodesastre' #<<<--- Las tablas de la media estan guardadas tanto por tipo de desastre como por pais
# <columnas.tabla> toma el valor de 'tipodesastre' o 'pais'

lista.wilcoxon  <- list()
lista.bmp       <- list()
list.bootstrap <- list()
indice.lista   <- 0
for(i in seq_along(umbrales.evento)){
  umbral.del.evento <- umbrales.evento[i]
  for(j in seq_along(es.windows)){
    indice.lista <- indice.lista +1
    estimation.window <- es.windows[j]
    load((file=paste0(getwd(),'/Resultados_regresion/Tablas/Tablas_',serie,'_tra',umbral.del.evento,'_est',
                      estimation.window,'_',tipo.estudio,'_',regresor.mercado,'_',columnas.tabla,'.RData')))
    if(tipo.estudio=='media'){
      lista.wilcoxon[[indice.lista]] <- dataframe.wilcoxon 
      names(lista.wilcoxon)[[indice.lista]] <- paste('Estimacion',estimation.window,'traslape',umbral.del.evento,sep='_')
      lista.bmp[[indice.lista]] <- dataframe.bmp
      names(lista.bmp)[[indice.lista]] <- paste('Estimacion',estimation.window,'traslape',umbral.del.evento,sep='_')
    }
    if(tipo.estudio=='varianza'){
      list.bootstrap[[indice.lista]] <- dataframe.volatilidad
    }
  }
}


# Create the main data table
main_table <- data.table(
  ID = c(1, 2, 3),
  SubTable = list(
    data.table(ID = c(10, 20, 30), Value = c(100, 200, 300)),
    data.table(ID = c(40, 50, 60), Value = c(400, 500, 600))
  )
)


