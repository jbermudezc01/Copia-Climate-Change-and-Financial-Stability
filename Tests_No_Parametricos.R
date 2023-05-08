# La base de retornos, <base_retornos>, se carga al correr del codigo <Replicacion_climate_change.R> de la linea 1 a 165

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

# Previo a la estimacion, se asegura que los indices de los retornos de mercado y de securities sea el mismo
base_retornos    <- base_retornos[index(mean_mov_average)]
mean_mov_average <- mean_mov_average[index(base_retornos)]

# Se eliminan los eventos que no cuentan con la ventana minima de estimacion ni con la ventana minima de evento usando la funcion <drop.events>
eventos <- drop.events(data.events = eventos,market.returns = mean_mov_average,estimation.start = estimation_start,max.ar=max_abnormal_returns)

# -------------------------- Regresion estimation window ---------------------------------------------

# <estimation.event.study> realiza la estimacion por OLS para cada evento en <data.events>. Retorna una lista para cada evento que incluye:
#     Dataframe      : retornos observados, estimados y anormales para la ventana de estimacion y ventana de evento. 
#     Standard_error : error estandar de los errores de la estimacion por OLS
# <all_events_list> sera la base para las pruebas de Wilcoxon y bootstrap

all_events_list <- estimation.event.study(data.events = eventos,days.evaluated = days_to_be_evaluated,securities.returns = base_retornos,
                                          market.returns = mean_mov_average,max.ar = max_abnormal_returns,es.start = estimation_start,
                                          es.end=estimation_end)

# Wilcoxon --------------------------------------------------------------

# Usamos la funcion <wilcoxon.jp.test> para realizar la prueba de Wilcoxon. Obtenemos el estadistico junto a su significancia
# <Significancia> = * indica que el estadistico es significativo al 10%, ** al 5% y *** al 1%.
# <Significancia> =   indica que el estadistico no es significativo a ningun nivel convencional
# Aparte, se usa la funcion <stats::wilcox.test> para obtener el p-valor
# La prueba que realiza wilcoxon.jp.test es a dos colas
wilcoxon.resultado <- wilcoxon.jp.test(data.list = all_events_list,es.window.length = length_estimation_window,
                                       ev.window.length = length_event_window);wilcoxon.resultado

# Bootstrap CAAR ----------------------------------------------------------

# Para hacer el procedimiento por Bootstrap se sigue el procedimiento usado por Corrado & Truong (2008)
boot_n_simul <- 1000 #<<<--- parametro que indica el numero de repeticiones para bootstrapping

bootstrap.resultado <- bootstrap_CT(data.list = all_events_list,market.returns=mean_mov_average,
                                    es.window.length = length_estimation_window, ev.window.length = length_event_window,
                                    no.simul = boot_n_simul);bootstrap.resultado

# Corrado and Zivney rank test --------------------------------------------

corrado.resultado <- corrado_zivney(data.list = all_events_list,es.window.length = length_estimation_window,
                                    ev.window.length = length_event_window); corrado.resultado
