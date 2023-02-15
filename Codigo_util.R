#Códigos para contrastar

#To verify contrasting to a list of weekdays

start_date <- as.Date("2001-01-01")
end_date <- as.Date("2019-12-31")
date_seq <- seq(from = start_date, to = end_date, by = "day")
date_seq <- date_seq[!(weekdays(as.Date(date_seq)) %in% c("sábado", "domingo"))]

dates_list <- as.list(index(base2))  
dates_sequence <- as.list(date_seq)

x1 <- dates_list[!(dates_list %in% dates_sequence)] #Problem
x2 <- dates_sequence[!(dates_sequence %in% dates_list)] #Check if all are international holidays


#To verify the days that could be removed. For example if == 22, there are 2 01st January that should be removed.
for (i in 1:nrow(base_test)) {
  row <- base_test[i, ]
  if (sum(is.na(row))==22){
    print(row)}
}

#### Codigo que habia usado para convertir en numerico

if(0){
  xts_list <- list(australia_xts, belgium_xts,brazil_xts,canada_xts,chile_xts,denmark_xts,finland_xts,france_xts,
                   germany_xts,hongkong_xts,india_xts,indonesia_xts,mexico_xts,netherlands_xts,poland_xts,russia_xts,
                   southafrica_xts,southkorea_xts,spain_xts,sweden_xts,switzerland_xts,thailand_xts,turkey_xts,
                   unitedkingdom_xts,usa1_xts,usa2_xts)  
  
  for (i in 1:length(xts_list)) {
    xts_obj <- xts_list[[i]]
    colnames(xts_obj)[1] <- "Price"
    df <- as.data.frame(xts_obj)
    df$Price <- as.numeric(df$Price)
    xts_obj <- as.xts(df)
    xts_list[[i]] <- xts_obj
  }
}

if(0){
  
  australia_xts <- xts_list[[1]]
  belgium_xts <- xts_list[[2]]
  brazil_xts <- xts_list[[3]]
  canada_xts <- xts_list[[4]]
  chile_xts <- xts_list[[5]]
  denmark_xts <- xts_list[[6]]
  finland_xts <- xts_list[[7]]
  france_xts <- xts_list[[8]]
  germany_xts <- xts_list[[9]]
  hongkong_xts <- xts_list[[10]]
  india_xts <- xts_list[[11]]
  indonesia_xts <- xts_list[[12]]
  mexico_xts <- xts_list[[13]]
  netherlands_xts <- xts_list[[14]]
  poland_xts <- xts_list[[15]]
  russia_xts <- xts_list[[16]]
  southafrica_xts <- xts_list[[17]]
  southkorea_xts <- xts_list[[18]]
  spain_xts <- xts_list[[19]]
  sweden_xts <- xts_list[[20]]
  switzerland_xts <- xts_list[[21]]
  thailand_xts <- xts_list[[22]]
  turkey_xts <- xts_list[[23]]
  unitedkingdom_xts <- xts_list[[24]]
  usa1_xts <- xts_list[[25]]
  usa2_xts <- xts_list[[26]]
}

if(0){
  colnames(base_test) <- c("australia_xts","belgium_xts","brazil_xts","canada_xts","chile_xts","denmark_xts","finland_xts",
                           "france_xts", "germany_xts","hongkong_xts","india_xts","indonesia_xts","mexico_xts",
                           "netherlands_xts", "poland_xts","russia_xts","southafrica_xts2", "southkorea_xts","spain_xts",
                           "sweden_xts","switzerland_xts", "thailand_xts","turkey_xts", "unitedkingdom_xts", "usa1_xts",
                           "usa2_xts")
}


###==================================================00


difference <- daily - daily_denton
plot(difference)


diff_sq <- (diff(daily_denton)^2)
sum_diff_sq <- sum(diff_sq[2:1826,])
diff_sq2 <- (diff(daily)^2)
sum_diff_sq2 <- sum(diff_sq2[2:1826,])