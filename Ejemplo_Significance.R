
set.seed(2)
n. = 20
vector <- rnorm(n.,mean=0.2)
test   <- wilcox.test(vector, mu=0,alternative="two.sided")
stat   <- test$statistic
# Siguiendo a la prueba <wilcox.test(vector)>, el estadistico de Wilcoxon es 152 y es significativo al 10% pero no al 5%
# Con los codigos anteriores:
significance <- ""
significance[stat >= stats::qsignrank(1 - 0.1, n = n.) | 
               stat <= n. * (n. + 1)/2 - stats::qsignrank(1 - 0.1/2, n = n.)] <- "*"
significance[stat >= stats::qsignrank(1 - 0.05, n = n.) | 
               stat <= n. * (n. + 1)/2 - stats::qsignrank(1 - 0.05/2, n = n.)] <- "**"
significance[stat >= stats::qsignrank(1 - 0.01, n = n.) | stat <= n. * (n. + 1)/2 - stats::qsignrank(1 - 0.01/2, 
                                                                                                     n = n.)] <- "***"
resultado_df <- data.frame("Wilcoxon_statistic" = stat,"Significancia" = significance); resultado_df
# Indica una significancia del 5% (**)

# Con el codigo corregido
significance2 <- ""
significance2[stat >= stats::qsignrank(1 - 0.1/2,n=n.)|
                stat <= stats::qsignrank(0.1/2, n=n.)] <- "*"
# Para evaluar al 5%:
significance2[stat >= stats::qsignrank(1 - 0.05/2, n=n.)|
                stat <= stats::qsignrank(0.05/2, n=n.)] <- "**"
# Al 1%:
significance2[stat >= stats::qsignrank(1 - 0.01/2, n=n.)|
                stat <= stats::qsignrank(0.01/2, n=n.)] <- "***"
resultado_2 <- data.frame("Wilcoxon_statistic" = stat,"Significancia" = significance2); resultado_2
# El cual da la conclusion correcta: significativo al 10% pero no al 5% (*)

# Relacionando con el codigo <Wilcoxon_Eventos.R>, si se establece <paises.usados> = "Switzerland", se vera la discrepancia en el codigo
# de la funcion de estudy2 con la funcion wilcox.test().

