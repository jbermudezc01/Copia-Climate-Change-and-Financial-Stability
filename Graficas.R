
### CODIGO PARA GRAFICAS

# De acuerdo con la notacion de los modelos estimados, los coeficientes el dia del evento terminan en <t0>, 
# el dia siguiente en <t1>, dos dias despues <t2>, y asi hasta llegar a <t4>. 

steps <- c("t0","t1","t2","t3","t4")  #<<<--- vector con los días adelante del evento, hace referencia a como termina el nombre de las dummies

## El siguiente ciclo genera la densidad Kernel de los coeficientes para cada tipo de desastre 
## y para todos los <t0>, <t1> ...
for(step in steps){
  for(model_name in names(coefficients_disasters_list)){
    dens_name <- paste("dens",model_name,step,sep="_")
    assign(dens_name,dens(coefficients_disasters_list[[model_name]][,"Estimate"],step))
  }
}

# Por otro lado, necesitamos hacer la grafica de los CAR, que es la suma de los retornos anormales.
# Con el loop for estamos haciendo el mismo proceso para cada uno de los 5 modelos estimados.
# Al final tendremos un vector para cada modelo que incluye los coeficientes relacionados para las 5 
# dummies temporales para todos los paises. Lo anterior para posteriormente ser sumadas por cada pais para 
# generar el retorno anormal acumulado t_0+t_1+t_2+t_3+t_4

for(model_name in names(coefficients_disasters_list)){
  #Vamos a generar una lista para cada modelo
  var_name <- paste0("coef_vec_",model_name)
  coef_vec <- c()
  for(step in steps){
    #reunimos los coeficientes en <coefs>
    coefs <- coefficients_disasters_list[[model_name]][,"Estimate"]
    
    #seleccionamos solamente los coeficientes que acaben con <step> y lo añadimos a <coef_vec>
    interest_indices <- grep(step,names(coefs))
    interest_coefficients <- coefs[interest_indices]
    coef_vec <- c(coef_vec, interest_coefficients)
  }
  # al final asignamos <coef_vec> al nombre especifico por modelo.
  assign(var_name, coef_vec)
}

# Generamos la densidad de los retornos anormales acumulados para cada tipo de desastre
densidad_CAR_bio <- densidad_CAR(coef_vec_fitsur_Bio,indexes)
densidad_CAR_cli <- densidad_CAR(coef_vec_fitsur_Cli,indexes)
densidad_CAR_geo <- densidad_CAR(coef_vec_fitsur_Geo,indexes)
densidad_CAR_hyd <- densidad_CAR(coef_vec_fitsur_Hyd,indexes)
densidad_CAR_met <- densidad_CAR(coef_vec_fitsur_Met,indexes)

### =============================== Graficas de retornos anormales ==================================

#Ya con las densidades de los retornos acumulados y de las dummies t_0, t_1, ..., t_4 podemos graficarlas


labels <- c("Biological","Climatological","Geophysical","Hydrological","Meteorological")  #<<<--- leyendas del grafico
colors <- c("blue", "tomato", "orange", "darkorchid4", "green")   #<<<--- colores que usara la grafica

# Para los CAR el vector seria
main_car   <- "Kernel denisty of CAR"  #<<<--- titulo para la grafica
vector_car <- c("densidad_CAR_bio","densidad_CAR_cli","densidad_CAR_geo","densidad_CAR_hyd", 
                "densidad_CAR_met") #<<<---vector de elementos a graficar
grafico_densidad(vector_car,main_car,labels,colors)


#Para los AR_t_0 seria
main_t_0   <- "Kernel density of AR t_0"  #<<<--- titulo para la grafica
vector_t_0 <- c("dens_fitsur_Bio_t0","dens_fitsur_Cli_t0","dens_fitsur_Geo_t0",
                "dens_fitsur_Hyd_t0","dens_fitsur_Met_t0") #<<<---vector de elementos a graficar
grafico_densidad(vector_t_0,main_t_0,labels,colors)


#Para los AR_t_1 seria
main_t_1   <- "Kernel density of AR t_1"  #<<<--- titulo para la grafica
vector_t_1 <- c("dens_fitsur_Bio_t1","dens_fitsur_Cli_t1","dens_fitsur_Geo_t1",
                "dens_fitsur_Hyd_t1","dens_fitsur_Met_t1") #<<<---vector de elementos a graficar
grafico_densidad(vector_t_1,main_t_1,labels,colors)


#Para los AR_t_2 seria
main_t_2   <- "Kernel density of AR t_2"  #<<<--- titulo para la grafica
vector_t_2 <- c("dens_fitsur_Bio_t2","dens_fitsur_Cli_t2","dens_fitsur_Geo_t2",
                "dens_fitsur_Hyd_t2","dens_fitsur_Met_t2") #<<<---vector de elementos a graficar
grafico_densidad(vector_t_2,main_t_2,labels,colors)

#Para los AR_t_3 seria
main_t_3   <- "Kernel density of AR t_3"  #<<<--- titulo para la grafica
vector_t_3 <- c("dens_fitsur_Bio_t3","dens_fitsur_Cli_t3","dens_fitsur_Geo_t3",
                "dens_fitsur_Hyd_t3","dens_fitsur_Met_t3") #<<<---vector de elementos a graficar
grafico_densidad(vector_t_3,main_t_3,labels,colors)

#Para los AR_t_4 seria
main_t_4   <- "Kernel density of AR t_4"  #<<<--- titulo para la grafica
vector_t_4 <- c("dens_fitsur_Bio_t4","dens_fitsur_Cli_t4","dens_fitsur_Geo_t4",
                "dens_fitsur_Hyd_t4","dens_fitsur_Met_t4") #<<<---vector de elementos a graficar
grafico_densidad(vector_t_4,main_t_4,labels,colors)

## ============================== Graficos A.3 de densidad de los retornos ========================================

colores <- c("blue","tomato","orange","darkorchid4","green","cyan","firebrick") #<<<--- colores para grafica de retornos

densidad_retornos <- apply(Retornos, MARGIN = 2, FUN = density)

# Para utilizar la función <grafico_retornos> necesitamos un objeto a graficar, mas un vector con los paises de cada continente, mas un
# titulo para el grafico y un vector de leyendas, el cual sera el nombre del indice de cada país. Ademas necesitamos los colores.

# Para America seria:
main_America      <- "Densidad retornos America" #<<<--- titulo para la grafica
indexes_America <- c("Bovespa","S.PCLXIPSA","NASDAQComposite","Nasdaq100","S.PTSXComposite","S.PBMVIPC")  #<<<--- vector de indices que pertencen al continente
labels_America    <- c("Bovespa","S&P CLX IPSA","NASDAQ Composite","Nasdaq 100","S&P TSX Composite",
                       "S&P BMV IPC") #<<<--- leyendas, indices de cada pais del continente
grafico_retornos(densidad_retornos,indexes_America,main_America,labels_America,colores)

# Para Europa del Este:
main_Europa_Este      <- "Densidad retornos Europa del Este" #<<<--- titulo para la grafica
indexes_Europa_Este <- c("MOEXRussia","OMXCopenhagen20","BIST100","OSEBenchmark","WIG20","OMXHelsinki25","OMXStockholm30") #<<<--- vector de indices que pertencen al continente
labels_Europa_Este    <- c("Moex Russia","OMX Copenhagen 20","BIST 100","OSE Benchmark","WIG20",
                           "OMX Helsinki 25","OMX Stockholm 30") #<<<--- leyendas, indices de cada pais del continente
grafico_retornos(densidad_retornos,indexes_Europa_Este,main_Europa_Este,labels_Europa_Este,colores)

# Para Europa del Oeste
main_Europa_Oeste      <- "Densidad retornos Europa del Oeste"  #<<<--- titulo para la grafica
indexes_Europa_Oeste <- c("FTSE100","SMI","DAX","IBEX35","AEX","BEL20","CAC40") #<<<--- vector de indices que pertencen al continente
labels_Europa_Oeste    <- c("FTSE 100","SMI","DAX","IBEX 35","AEX","BEL 20","CAC 40") #<<<--- leyendas, indices de cada pais del continente
grafico_retornos(densidad_retornos,indexes_Europa_Oeste,main_Europa_Oeste,labels_Europa_Oeste,colores)

#Para Asia
main_Asia      <- "Densidad retornos Asia" #<<<--- titulo para la grafica
indexes_Asia <- c("SETIndex","KOSPI","Nifty50","JakartaStockExchange","HangSeng") #<<<--- vector de paises que pertencen al continente
labels_Asia    <- c("SET Index","KOSPI","Nifty 50","Jakarta Stock Exchange","Hang Seng") #<<<--- leyendas, indices de cada pais del continente
grafico_retornos(densidad_retornos,indexes_Asia,main_Asia,labels_Asia,colores)

#Para Africa y Oceania
main_Africa_Oceania      <- "Densidad retornos Africa y Oceania" #<<<--- titulo para la grafica
indexes_Africa_Oceania <- c("SouthAfricaTop40","S.PASX200")  #<<<--- vector de paises que pertencen al continente
labels_Africa_Oceania    <- c("South Africa Top 40","S&P ASX 200") #<<<--- leyendas, indices de cada pais del continente
grafico_retornos(densidad_retornos,indexes_Africa_Oceania,main_Africa_Oceania,labels_Africa_Oceania,colores)

### ============================ Grafico 3 Pagnottoni. AR estimates ============================================================

# Para el grafico, Pagnottoni tiene un orden especifico, por lo cual toca especificarlo

pagn_orden <- c("SETIndex", "MOEXRussia", "KOSPI", "Nifty50", "JakartaStockExchange", "Bovespa", 
                "S.PCLXIPSA", "HangSeng", "NASDAQComposite", "Nasdaq100", "S.PTSXComposite", "S.PBMVIPC", 
                "SouthAfricaTop40", "OMXCopenhagen20", "BIST100", "OSEBenchmark", "WIG20", "OMXHelsinki25", 
                "FTSE100", "S.PASX200", "OMXStockholm30", "SMI", "DAX", 
                "IBEX35", "AEX", "BEL20", "CAC40") #<<<--- indices en el orden que aparece en la grafica #3

labels_grafico <- c("SET Index", "MOEX Russia", "KOSPI", "Nifty 50", "Jakarta Stock Exchange", "Bovespa", 
                    "S&P CLX IPSA", "Hang Seng", "NASDAQ Composite", "Nasdaq 100", "S&P TSX Composite", "S&P BMV IPC", 
                    "South Africa Top 40", "OMX Copenhagen 20", "BIST 100", "OSE Benchmark", "WIG20", "OMX Helsinki 25", 
                    "FTSE 100", "S&P ASX 200", "OMX Stockholm 30", "SMI", "DAX", 
                    "IBEX 35", "AEX", "BEL 20", "CAC 40") #<<<--- indices en el orden que aparece en la grafica #3 (leyenda)

group         <- rep(labels_grafico,each=5) ## Variable que va a agrupar en grupos de a 5 los datos (porque cada 5 es un indice distinto)
colores.ar = c("#1964C4", "#C9675A", "#D5B259","darkorchid4","#709E3D")


## Para biological
ar_data_Bio       <- coef_vec_fitsur_Bio[order_coef(names(coef_vec_fitsur_Bio),pagn_orden)] ##ordenar
ar_data_frame_Bio <- data.frame(values = ar_data_Bio,  group=group, subgroup =steps)
ar_data_frame_Bio$group <- factor(ar_data_frame_Bio$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>
plot_Bio <- grafico_estimates(ar_data_frame_Bio,"Abnormal return","Biological",colors = colores.ar)

## Para climatological
ar_data_Cli       <- coef_vec_fitsur_Cli[order_coef(names(coef_vec_fitsur_Cli),pagn_orden)] ##ordenar
ar_data_frame_Cli <- data.frame(values = ar_data_Cli, group=group, subgroup =steps)
ar_data_frame_Cli$group <- factor(ar_data_frame_Cli$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>
plot_Cli <- grafico_estimates(ar_data_frame_Cli,"Abnormal return","Climatological",colors = colores.ar)

## Para geophysical

ar_data_Geo       <- coef_vec_fitsur_Geo[order_coef(names(coef_vec_fitsur_Geo),pagn_orden)] ##ordenar
ar_data_frame_Geo <- data.frame(values = ar_data_Geo, group=group, subgroup =steps)
ar_data_frame_Geo$group <- factor(ar_data_frame_Geo$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>
plot_Geo <- grafico_estimates(ar_data_frame_Geo,"Abnormal return","Geophysical",colors = colores.ar)

## Para hydrological

ar_data_Hyd       <- coef_vec_fitsur_Hyd[order_coef(names(coef_vec_fitsur_Hyd),pagn_orden)] ##ordenar
ar_data_frame_Hyd <- data.frame(values = ar_data_Hyd, group=group, subgroup =steps)
ar_data_frame_Hyd$group <- factor(ar_data_frame_Hyd$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>
plot_Hyd <- grafico_estimates(ar_data_frame_Hyd,"Abnormal return","Hydrological",colors = colores.ar)

## Para meteorological
ar_data_Met       <- coef_vec_fitsur_Met[order_coef(names(coef_vec_fitsur_Met),pagn_orden)] ##ordenar
ar_data_frame_Met <- data.frame(values = ar_data_Met,  group=group, subgroup =steps)
ar_data_frame_Met$group <- factor(ar_data_frame_Met$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>
plot_Met <- ggplot(ar_data_frame_Met, aes(x=group,y=values,fill=subgroup))+
  geom_bar(stat="identity", position="dodge", width=0.7) +
  scale_fill_manual(values=c("#1964C4", "#C9675A","#D5B259","#7C63CF","#709E3D")) +
  labs(x="index",y="Abnormal return",title="Meteorological") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10))


#graficas juntas

complete_plot <- grid.arrange(plot_Bio,plot_Cli,plot_Geo,plot_Hyd,plot_Met,nrow=5,ncol=1,heights = c(1,1,1,1,1.7))
#ggsave("abnormal_returns.pdf",plot=complete_plot,device="pdf", width = 8.27, height = 11.69) # Ya esta guardado en el github por lo que no es necesario volverlo a cargar

### ===========================  Grafico A.4 Pagnottoni, t-tests =======================================================

#Primero necesitamos el valor de los estadísticos t

for(model_name in names(coefficients_disasters_list)){
  #Vamos a generar una lista para cada modelo
  tests <- coefficients_disasters_list[[model_name]][,"t value"]
  var_name <- paste0("t_test_",model_name)
  t_test <- c()
  for(step in steps){
    #reunimos los coeficientes en <coefs>
    
    #seleccionamos solamente los coeficientes que acaben con <step> y lo añadimos a <t_test>
    interest_indices <- grep(step,names(tests))
    interest_tests <- tests[interest_indices]
    t_test <- c(t_test, interest_tests)
  }
  # al final asignamos <t_test> al nombre especifico por modelo.
  assign(var_name, t_test)
}

#Para biological

t_data_Bio       <- t_test_fitsur_Bio[order_coef(names(t_test_fitsur_Bio),pagn_orden)] ##ordenar
t_data_frame_Bio <- data.frame(values = t_data_Bio,  group=group, subgroup =steps)
t_data_frame_Bio$group <- factor(t_data_frame_Bio$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>
plot_t_Bio <- grafico_estimates(t_data_frame_Bio, "t_test", "Biological",colors = colores.ar)


#Para climatological

t_data_Cli       <- t_test_fitsur_Cli[order_coef(names(t_test_fitsur_Cli),pagn_orden)] ##ordenar
t_data_frame_Cli <- data.frame(values = t_data_Cli, group=group,subgroup =steps)
t_data_frame_Cli$group <- factor(t_data_frame_Cli$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>
plot_t_Cli <- grafico_estimates(t_data_frame_Cli, "t_test", "Climatological",colors = colores.ar)

## Para geophysical

t_data_Geo       <- t_test_fitsur_Geo[order_coef(names(t_test_fitsur_Geo),pagn_orden)] ##ordenar
t_data_frame_Geo <- data.frame(values = t_data_Geo, group=group, subgroup =steps)
t_data_frame_Geo$group <- factor(t_data_frame_Geo$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>

plot_t_Geo <- grafico_estimates(t_data_frame_Geo, "t_test", "Geophysical",colors = colores.ar)


## Para hydrological

t_data_Hyd       <- t_test_fitsur_Hyd[order_coef(names(t_test_fitsur_Hyd),pagn_orden)] ##ordenar
t_data_frame_Hyd <- data.frame(values = t_data_Hyd, group=group, subgroup =steps)
t_data_frame_Hyd$group <- factor(t_data_frame_Hyd$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>

plot_t_Hyd <- grafico_estimates(t_data_frame_Hyd, "t_test", "Hydrological",colors = colores.ar)


## Para meteorological

t_data_Met       <- t_test_fitsur_Met[order_coef(names(t_test_fitsur_Met),pagn_orden)] ##ordenar
t_data_frame_Met <- data.frame(values = t_data_Met, group=group, subgroup =steps)
t_data_frame_Met$group <- factor(t_data_frame_Met$group, levels = labels_grafico) ## Para preservar el orden de la variable categorica <grupo>

plot_t_Met <- ggplot(t_data_frame_Met, aes(x=group,y=values,fill=subgroup))+
  geom_bar(stat="identity", position="dodge", width=0.7) +
  scale_fill_manual(values=c("#1964C4", "#C9675A", "#D5B259","#7C63CF","#709E3D")) +
  labs(x="index",y="t-test",title="Meteorological") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

#graficas juntas

complete_t_plot <- grid.arrange(plot_t_Bio,plot_t_Cli,plot_t_Hyd,plot_t_Geo,plot_t_Met,nrow=5,ncol=1,heights = c(1,1,1,1,1.7))
#ggsave("t_tests.pdf",plot=complete_t_plot,device="pdf", width = 8.27, height = 11.69) # Ya esta guardado en el github por lo que no es necesario volverlo a cargar


#####=========================================== FIGURA 4 Y A.8 Pagnottoni  ==============================================###

niv.significancia <- 0.05 #<<<--- nivel de significancia para los estimados de retornos anormales
pattern_step      <- paste(steps, collapse = "|") # patron que indica los pasos. # collapse = "|" indica que puede ser cualquier valor de <steps>
pattern_indexes   <- paste(indexes, collapse = "|") #patron que indica los indices
pattern_countries <- paste(paises, collapse = "|")    #patron que indica los paises del desastre

# Graficas del CAR, pero fue cambiado, ya que ahora se utiliza la funcion <car_countries2>
if(0){
  europe_plot <- car_countries(continent_model=fitted_models2_Europe, significance.level=niv.significancia, pattern.step=pattern_step, 
                               pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                               labels=labels_grafico, color="orange", title.graph="Europe")
  europe_plot
  #ggsave("Graficos_CAR/Europe_1.png",plot=europe_plot,device="png")
  
  #====
  america_plot <- car_countries(continent_model=fitted_models2_Americas, significance.level=niv.significancia, pattern.step=pattern_step, 
                                pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                                labels=labels_grafico, color="blue", title.graph="America")
  america_plot
  #ggsave("Graficos_CAR/America_1.png",plot=america_plot,device="png")
  
  #====
  asia_plot <- car_countries(continent_model=fitted_models2_Asia, significance.level=niv.significancia, pattern.step=pattern_step, 
                             pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                             labels=labels_grafico, color="tomato", title.graph="Asia")
  asia_plot
  #ggsave("Graficos_CAR/Asia_1.png",plot=asia_plot,device="png")
  
  #====
  africa_plot <- car_countries(continent_model=fitted_models2_Africa, significance.level=niv.significancia, pattern.step=pattern_step, 
                               pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                               labels=labels_grafico, color="magenta4", title.graph="Africa")
  africa_plot
  #ggsave("Graficos_CAR/Africa_1.png",plot=africa_plot,device="png")
  
  #====
  oceania_plot <- car_countries(continent_model=fitted_models2_Oceania, significance.level=niv.significancia, pattern.step=pattern_step, 
                                pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                                labels=labels_grafico, color="olivedrab3", title.graph="Oceania")
  oceania_plot
  #ggsave("Graficos_CAR/Oceania_1.png",plot=oceania_plot,device="png")
}


europe_plot2 <- car_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Europe, 
                               significance.level=niv.significancia, pattern.step=pattern_step, 
                               pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                               labels=labels_grafico, color="orange", title.graph="Europe")
europe_plot2
#ggsave(paste0(paste0("Graficos_CAR/Europe_",as.character(niv.significancia*100)),".png"),plot=europe_plot2,device="png")

#====
america_plot2 <- car_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Americas, 
                                significance.level=niv.significancia, pattern.step=pattern_step, 
                                pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                                labels=labels_grafico, color="blue", title.graph="America")
america_plot2
#ggsave(paste0(paste0("Graficos_CAR/America_",as.character(niv.significancia*100)),".png"),plot=america_plot2,device="png")

#====
asia_plot2 <- car_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Asia, 
                             significance.level=niv.significancia, pattern.step=pattern_step, 
                             pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                             labels=labels_grafico, color="tomato", title.graph="Asia")
asia_plot2
#ggsave(paste0(paste0("Graficos_CAR/Asia_",as.character(niv.significancia*100)),".png"),plot=asia_plot2,device="png")

#====
africa_plot2 <- car_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Africa, 
                               significance.level=niv.significancia, pattern.step=pattern_step, 
                               pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                               labels=labels_grafico, color="magenta4", title.graph="Africa")
africa_plot2
#ggsave(paste0(paste0("Graficos_CAR/Africa_",as.character(niv.significancia*100)),".png"),plot=africa_plot2,device="png")

#====
oceania_plot2 <- car_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Oceania, 
                                significance.level=niv.significancia, pattern.step=pattern_step, 
                                pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                                labels=labels_grafico, color="olivedrab3", title.graph="Oceania")
oceania_plot2
#ggsave(paste0(paste0("Graficos_CAR/Oceania_",as.character(niv.significancia*100)),".png"),plot=oceania_plot2,device="png")

### Grafica con promedios en vez de CAR ======

europe_plot_aver <- average_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Europe, 
                                       significance.level=niv.significancia, pattern.step=pattern_step, 
                                       pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                                       labels=labels_grafico, color="orange", title.graph="Europe")
europe_plot_aver

#====
america_plot_aver <- average_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Americas, 
                                        significance.level=niv.significancia, pattern.step=pattern_step, 
                                        pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                                        labels=labels_grafico, color="blue", title.graph="America")
america_plot_aver

#====
asia_plot_aver <- average_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Asia, 
                                     significance.level=niv.significancia, pattern.step=pattern_step, 
                                     pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                                     labels=labels_grafico, color="tomato", title.graph="Asia")
asia_plot_aver

#====
africa_plot_aver <- average_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Africa, 
                                       significance.level=niv.significancia, pattern.step=pattern_step, 
                                       pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                                       labels=labels_grafico, color="magenta4", title.graph="Africa")
africa_plot_aver

#====
oceania_plot_aver <- average_countries2(continent_coefficients=coefficients_continents_list$fitted_coefficients_Oceania, 
                                        significance.level=niv.significancia, pattern.step=pattern_step, 
                                        pattern.indexes=pattern_indexes, pattern.countries=pattern_countries, order.graph=pagn_orden, 
                                        labels=labels_grafico, color="olivedrab3", title.graph="Oceania")
oceania_plot_aver


#### Graficas 1, A.1, A.6 y A.7 (mapamundi) ============================

my_colors <- c("#24203B", "#0028C1", "#C4E5F2", "#4891A8", "#63CB92",
               "#F7D73B", "#D0706C", "#8D5355", "#DB48A3", "#BC92F2")

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

disasters <- ggplot(data = merged_data, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = decil)) +
  scale_fill_manual(values = my_colors) +
  ggtitle("Distribution of natural disasters by country") +
  plain

disasters + 
  geom_path(data = world, aes(x = long, y = lat, group = group), 
            color = "black", linewidth = 0.5)


plots <- lapply(names(merged_data_disasters), function(name) {
  df <- merged_data_disasters[[name]]
  ggplot(data = df, mapping = aes(x = long, y = lat, group = group)) +
    coord_fixed(1.3) +
    geom_polygon(aes(fill = decile)) +
    scale_fill_manual(values = my_colors) +
    ggtitle(name) +
    plain+
    geom_path(data = world, aes(x = long, y = lat, group = group), 
              color = "black", linewidth = 0.5)
})

# Combinar los mapamundis por tipo de desastre en un solo grafico
gridExtra::grid.arrange(grobs = plots, ncol = 2)
