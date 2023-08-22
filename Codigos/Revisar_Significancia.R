# Intentar obtener todos los eventos que tienen geophysical de <all.events.list>
all.events.geophysical <- all.events.list[purrr::map(all.events.list,~.x@evento$Disaster.Subgroup) == 'Geophysical']

i=10
plot(cumsum(tail(all.events.geophysical[[i]]@retornos$Abnormal,16)))
View(tail(all.events.geophysical[[i]]@retornos,16))
    

sort(colSums(data.frame(purrr::map(all.events.geophysical, ~.x@retornos$Abnormal))[202:216,])) 


