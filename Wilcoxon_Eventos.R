# La base de retornos, <base_retornos>, se carga al correr del codigo 
# <Replicacion_climate_change.R> de la linea 1 a 165

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
   emdat_base <- emdat_base %>%
     mutate(na_start = ifelse(is.na(Start.Day),1,0))
   emdat_base <- emdat_base %>% 
     mutate(Start.Day=replace_na(Start.Day,1)) # <replace_na> se utiliza para reemplazar los valores <NA> por <1>
 }
 # Generacion de la fecha completa del inicio de evento, <Start.Date>, 
 # a partir de <Start.Year>, <Start.Month> y <Start.Day>
 emdat_base <- emdat_base %>% 
   unite(Start.Date, c(Start.Year, Start.Month, Start.Day), sep = "-",remove=FALSE) %>% 
   mutate(Start.Date = as.Date(Start.Date))

 # Vector con los nombres de paises usados en la prueba Wilcoxon
 unico_pais <- NULL
 if(is.null(unico_pais)){
  paises.usados <- c("Australia","Belgium", "Brazil", "Canada", "Chile", "Denmark", "Finland",
                    "France", "Germany", "HongKong", "India", "Indonesia","Mexico","Netherlands","Norway","Poland","Russia",
                    "SouthAfrica","SouthKorea", "Spain", "Sweden","Switzerland","Thailand","Turkey", 
                    "UnitedKingdom","USA") #<<<--- Paises usados en Wilcoxon
 }else{
   paises.usados <- unico_pais
 }

 # Se filtra la base solo por los paises de <paises.usados>
 emdat_base <- emdat_base %>% 
   dplyr::filter(Country %in% paises.usados)

 # Dataframe eventos -------------------------------------------------------

 # Generacion de un dataframe solo con dos variables: <Country> y <Start.Date>
 eventos <- emdat_base %>% 
   dplyr::select(Country,Start.Date)

 # Se eliminan los eventos que no cuentan con la ventana minima de estimacion
 estimation_start <- 150 #<<<--- No. de dias antes del evento para comenzar la estimacion
 # Fecha minima para que se pueda realizar la estimacion con <estimation_start> dias
 Fecha_minima_estimacion <- index(mean_mov_average)[estimation_start+1] 
 # Filtracion <eventos>. Solamente contener eventos despues de <Fecha_minima_estimacion>
 eventos <- eventos %>% 
   dplyr::filter(Start.Date>=Fecha_minima_estimacion)
 # Se eliminan los eventos que no cuentan con la ventana minima de evento
 max_abnormal_returns <- 15  #<<<--- No. dias maximos despues del evento para calcular retorno anormal
 # Fecha minima para que se pueda realizar el calculo de retornos anormales para
 # <max_abnormal_returns> dias
 Fecha_minima_evento <- index(mean_mov_average)[length(index(mean_mov_average))-(max_abnormal_returns)]
 # Filtracion <eventos>. Solammente contener eventos anteriores a <Fecha_minima_evento>
 eventos <- eventos %>% 
   dplyr::filter(Start.Date<=Fecha_minima_evento)
}

# -------------------------- Regresion estimation window ---------------------------------------------

# Loop 110: Por cada evento se hace una regresion OLS con la muestra [-<estimation_start>,-<estimation_end>] dias antes del evento para estimar alfa, beta 

estimation_end       <- 1   #<<<--- No. dias antes del evento para finalizar la estimacion
# Si la fecha del evento no esta en <base_retornos>, se revisara hasta <days_to_be_evaluated> dias
# despues del desastre para ser considerado como el inicio del evento
days_to_be_evaluated <- 5   #<<<--- No. dias despues del evento a ser evaluados

## Falta volverlo una funcion que tome como argumentos un dataframe de eventos y una base de retornos, mas los argumentos de arriba
all_events_list      <- list() # lista que contendra todos los xts + errores estandar
for(i in 1:nrow(eventos)){
  # Primero se encuentra a que dato le corresponde el dia del evento, y el dia final de la ventana de evento es el dia del evento
  # mas <max_abnormal_returns>
  event_list <- list() # lista donde se guarda por cada evento un dataframe de retornos observados, predichos (predicted) y anormales;
                       # junto a error estandar del error en la estimacion
  pais <- as.character(eventos[i,'Country'])
  index_names <- matching(pais) # Nombre de la variable del <pais> con la que se calculan retornos anormales (ej: stock-index del pais)
  suppressWarnings({
    # Loop que genera la posicion de desastre respecto al indice de <base_retornos>. Si la fecha del evento no esta en  el indice de <base_retornos>, 
    # se revisara hasta <days_to_be_evaluated> dias despues del desastre para ser considerado como el inicio del evento
    for(j in 0:days_to_be_evaluated){
      if((eventos[i,'Start.Date']+j) %in% index(base_retornos[,index_names])){ 
        # Generacion de la posicion del dia de desastre en el indice de fechas de <base_retornos>
        # (o j dias despues del desastre, si el dia del desastre no esta en el indice de retornos)
        event_start_index <- which(index(base_retornos[,index_names])==eventos[i,'Start.Date']+j)
        break
      }
    }
    # Generacion de la fecha del ultimo dia de la ventana de evento
    event_end_date   <- index(base_retornos[,index_names])[event_start_index + max_abnormal_returns]
  })
  
  # Creacion  de la base de datos de la ventana de estimacion en <base_retornos> para el <index_names> 
  # y para el promedio movil, <mean_mov_xts>, que es una var.exogena del modelo
  estimation_series     <- base_retornos[,index_names][(event_start_index-estimation_start):(event_start_index-estimation_end),] #Falta!!!
  estimation_start_date <- index(estimation_series)[1] #Falta!!!
  estimation_end_date   <- index(estimation_series)[length(index(estimation_series))] #Falta!!!
  mean_mov_xts          <- mean_mov_average[index(mean_mov_average)>=estimation_start_date & index(mean_mov_average) <= estimation_end_date]
  estimation_data       <- cbind(estimation_series, mean_mov_xts)
  
  # Regresion OLS
  # Loop para los casos en que haya mas de un indice por pais, se realiza regresion OLS para estimar alpha y beta
  # Nota: En general solo hay un indice por pais, pero en USA hay dos.
  for(name in index_names){
    model          <- lm(estimation_data[,name] ~ estimation_data$Mean_Returns_Moving_Averages)
    alpha          <- model$coefficients[["(Intercept)"]] 
    beta           <- model$coefficients[["estimation_data$Mean_Returns_Moving_Averages"]]
    standard_error <- sd(residuals(model))
    
    observed       <- base_retornos[,name][index(base_retornos[,name])>=estimation_start_date & index(base_retornos[,name])<=event_end_date]
    predicted      <- alpha + beta*(mean_mov_average[index(mean_mov_average)>=estimation_start_date & index(mean_mov_average)<=event_end_date]) 
    abnormal       <- observed - predicted
    df             <- merge(observed,predicted,abnormal)
    colnames(df)   <- c('Observed','Predicted','Abnormal')
    event_list[["Dataframe"]]       <- df 
    event_list[["Standard_Error"]]  <- standard_error 
    all_events_list[[paste(i,name,sep="_")]] <- event_list
  }
}


# Wilcoxon --------------------------------------------------------------

length_car_window <- 10 #<<<--- Ventana para calcular el CAR (por ejemplo 5 significa [0,+5], donde 0 es el dia del evento)
# Para el calculo del CAR se toma la serie <Abnormal> a partir de la obs <estimation_start> + 1 hasta <estimation_start>+1+<lenght_car_window>
# El siguiente codigo esta con <if(1)> porque falta volverlo una funcion 
if(1){
  # Se crea el vector <all_car> para guardar los CAR
  # Nota: La longitud de este vector es igual al numero de eventos si solo hay un indice porpais.
  #       Si hay mas de un indice por pais, la longitud de <all_car> aumenta consecuentemente
  all_car <- c()
  for(element in all_events_list){
    car     <- sum(element$Dataframe$Abnormal[(estimation_start+1) : (estimation_start+1+length_car_window)])
    all_car <- c(all_car,car)
  }
  # La media del vector es el CAAR
  caar <- sum(all_car)/length(all_car)
  
  # Se genera un dataframe para poder realizar el ordenamiento de los car
  df_car <- data.frame("car"=all_car,"magnitude"=abs(all_car),"sign"=sign(all_car))
  df_car <- df_car %>% 
    mutate(magnitude=ifelse(magnitude==0,NA,magnitude)) ## Se coloca NA si la magnitud es 0, ya que no se deben considerar
  df_car <- df_car %>% 
    mutate(rank = rank(magnitude))
  
  # Suma de los rangos de car positivos: <positive_rank_sum>
  rank_sum <- df_car %>% 
    dplyr::group_by(sign) %>%
    dplyr::summarize(sum = sum(rank)) %>%
    dplyr::arrange(sum)
  positive_rank_sum <- rank_sum$sum[rank_sum$sign=="1"]
  
  # Calculo de la significancia del estadistico de Wilcoxon
  significance <- ""
  ## Calculo para cada nivel de significancia si el valor en statistics es lo suficientemente extremo para rechazar H_0
  # qsignrank da la funcion cuantil.  ARREGLAR !!!!
  N <- length(all_events_list)
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
  # Comparacion con t_test
  t.test(all_car)
}