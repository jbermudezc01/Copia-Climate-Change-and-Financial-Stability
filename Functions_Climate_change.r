#----------------------------------  read_cvs_files ------------------------------------#
# Se crea una funcion que va a leer los csv para los indices bursatiles, genera una lista de los indices bursatiles
#  en formato xts. 
#---------------------------------------------------------------------------------------#
# ----Argumentos de entrada ----#
#-- dir      : XXXXX xxx 
#-- countries: XXXXX
# ----Argumentos de salida  ----#
#-- xtgs_list: List que contiene XXX
#---------------------------------------------------------------------------------------#
read_csv_files <- function(dir,countries) {
  xts_list     <- list()
  for (country in countries) {
    csv_file <- paste0(dir,"Stocks_", country, ".csv")
    csv      <- read.csv(csv_file, header = TRUE, sep = ";", quote = "\"", col.names = c("Date","Price", "Open", 
                                                                                         "High","Low","Vol.","Change%"))
    colnames <- names(csv)
    for (colname in colnames[2:length(colnames)]) {
      csv[, colname] <- as.numeric(gsub(",","",csv[, colname]))
    } ## Muestra warning() ya quehay una columna que contiene caracteres "M" 
    csv$Date <- as.Date(csv$Date, "%m/%d/%Y")
    xts_list[[country]] <- xts(csv$Price, csv$Date)
  }
  return(xts_list)
}
#------------------------------------------------------------------------------------#

