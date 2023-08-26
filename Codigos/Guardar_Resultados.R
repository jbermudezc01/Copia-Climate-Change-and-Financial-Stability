tipo.estudio     <- 'varianza'
if(bool_cds){serie <- 'CDS'}else{serie <- 'Indices'}
if(promedio.movil){regresor.mercado <- 'PM'}else{regresor.mercado <- 'benchmark'}
if(tipo.estudio == 'media') save(all_events_list, 
     file=paste0(getwd(),'/Resultados_regresion/',serie,'_tra',umbral.evento,'_est',estimation_start,'_',tipo.estudio,'_',regresor.mercado,'.RData'))

if(tipo.estudio == 'varianza') save(volatility_results, 
     file=paste0(getwd(),'/Resultados_regresion/',serie,'_tra',umbral.evento.vol,'_est',estimation_vol_start,'_',tipo.estudio,'_',regresor.mercado,'.RData'))


# load(paste0(getwd(),'/Resultados_regresion/CDS_tra',traslape,'_est',estimacion,'.RData'))

# Posteriormente al <load> se tiene la lista <all_events_list>, la cual tiene objetos de clase 'ESmean', la cual fue creada
# con la  funcion <estimation.event.study>.Cada uno de estos objetos tiene diversos slots a los cuales se puede acceder usando 
# @. Los slots son : 