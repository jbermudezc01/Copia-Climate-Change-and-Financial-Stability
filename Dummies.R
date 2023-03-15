library(dplyr)
library(openxlsx)
library(lubridate)
library(tidyr)
library(xlsx)
library(writexl)

#Primero creo la base de datos original, y la transformo en un objeto tbl ya que es más facil de manejar usando la libreria dplyr
emdat     <- openxlsx::read.xlsx("C:/Users/jpber/OneDrive/Documents/BASE_EMDAT.xlsx",sheet = "Table1")
emdat_tbl <- tbl_df(emdat)

#Selecciono solamente las variables que me interesan para poder manejar mejor la base
emdat_interest_variables <- emdat_tbl %>% 
  dplyr::select(Disaster.Subgroup,Country,Continent,Start.Year,Start.Month,Start.Day,End.Year,End.Month,End.Day)

#Filtro la base,eliminando las filas que contengan NA en el mes de inicio, ya que no se puede suponer el mes en que empezo el desastre.
#Siendo la base final que vamos a manejar la llamaré emdat_final.
emdat_final <- emdat_interest_variables %>% 
  dplyr::filter(!is.na(Start.Month))

#Tambien hay fechas donde no se especifica el dia en la base original, por lo que se asumira que el dia de inicio fue el primer dia 
#del mes correspondiente. A su vez, se asumira que el dia del fin del evento fue el ultimo de cada mes si no hay dato en la base 
#original. Es un supuesto fuerte, por lo que es necesario saber a cuales observaciones se les hizo el supuesto.
#Creo dos dummies. La primera toma 1 si se hizo el supuesto en el dia de inicio y 0 en otro caso. La segunda toma 1 si se hizo el 
#supuesto en el dia final y 0 en otro caso.

emdat_final <- emdat_final %>%
  mutate(na_start = ifelse(is.na(Start.Day),1,0))

emdat_final <- emdat_final %>% 
  mutate(na_end = ifelse(is.na(End.Day),1,0))

## Y ahora ya teniendo las dummies, procedo a llenar los datos NA como se propuso anteriormente, creando nuevas columnas, por si 
## es necesario comparar las columnas originales

emdat_final <- emdat_final %>% 
  mutate(Start.Day2 = ifelse(is.na(Start.Day),1,Start.Day) )

 # El siguiente codigo crea una nueva columna End.Day2 que llena los NA de End.Day, a excepcion de aquellos donde End.Month es NA
 # ya que no los elimine (no me parecio sensato). Llena los NA seleccionando el último día del mes, usando la función celing_date.
emdat_final <- emdat_final %>% 
  mutate(End.Day2 = ifelse(is.na(End.Day)&!is.na(End.Month),
                           day(ceiling_date(ymd(paste(End.Year, End.Month, "01", sep = "-")), "month")-1),
                           End.Day))
  
# Creo dos columnas, cada una con formato fecha, indicando el dia de inicio y el dia final del desastre
emdat_final <- emdat_final %>% 
  mutate(Start.Date = as.Date(paste(Start.Year,Start.Month,Start.Day2,sep="-")))

emdat_final <- emdat_final %>% 
  mutate(End.Date = as.Date(paste(End.Year,End.Month,End.Day2,sep="-")))

#write_xlsx(emdat_final, "C:/Users/jpber/OneDrive/Documents/EMDAT.xlsx")

emdat_selected <- emdat_final %>% 
  dplyr::select(Country, na_start, Start.Date) %>% 
  distinct()

countries_base <- unique(emdat_selected$Country)
countries_base <- sort(countries_base)

wb <- xlsx::createWorkbook()

for(country in countries_base){
  country_data <- emdat_selected %>% 
    dplyr::filter(Country == country)
  colnames(country_data) <- c("Country","na_start","t0")
  sheet <- xlsx::createSheet(wb, sheetName = country)
  addDataFrame(country_data,sheet,startRow = 1,startColumn = 1)
}

saveWorkbook(wb, "C:/Users/jpber/OneDrive/Documents/data_by_country.xlsx")



