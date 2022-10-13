message( paste( rep('-', 100 ), collapse = '' ) )
message( '\t Datos de la piramide poblacional de los egresados por edad y sexo' )

# --------------------------------------------------------------------------------------------------
# Cargamos la base de datos completas de los egresados de la EPN
# --------------------------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_PM_infor_egresados.RData' ))

# Tabla de rangos del número de egresados por edad y sexo
n_egre_male <- piramide_edad_sexo[ sexo == "M"]
n_egre_male[ edad <= 24, R_edad := '$[21,24]$' ]
n_egre_male[ edad <= 29 & edad >= 25, R_edad := '$[25,29]$' ]
n_egre_male[ edad <= 33 & edad >= 30, R_edad := '$[30,33]$' ]
n_egre_male[ edad <= 37 & edad >= 34, R_edad := '$[34,37]$' ]
n_egre_male[ edad <= 41 & edad >= 38, R_edad := '$[38,41]$' ]
n_egre_male[ edad <= 46 & edad >= 42, R_edad := '$[42,46]$' ]
n_egre_male[ edad <= 50 & edad >= 47, R_edad := '$[47,50]$' ]
n_egre_male <- n_egre_male[ ,  list(Numero_hombres = sum(n)), by = c('R_edad')]
setorder(n_egre_male, R_edad)
n_egre_male[ , Prom_Hom := Numero_hombres/sum(n_egre_male$Numero)]

n_egre_female <- piramide_edad_sexo[ sexo == "F"]
n_egre_female[ edad <= 24, R_edad := '$[21,24]$' ]
n_egre_female[ edad <= 29 & edad >= 25, R_edad := '$[25,29]$' ]
n_egre_female[ edad <= 33 & edad >= 30, R_edad := '$[30,33]$' ]
n_egre_female[ edad <= 37 & edad >= 34, R_edad := '$[34,37]$' ]
n_egre_female[ edad <= 41 & edad >= 38, R_edad := '$[38,41]$' ]
n_egre_female[ edad <= 46 & edad >= 42, R_edad := '$[42,46]$' ]
n_egre_female[ edad <= 50 & edad >= 47, R_edad := '$[47,50]$' ]
n_egre_female <- n_egre_female[ ,  list(Numero_mujeres = sum(n)), by = c('R_edad')]
setorder(n_egre_female, R_edad)
n_egre_female[ , Prom_Muj := Numero_mujeres/sum(n_egre_female$Numero)]

n_per_sexo_edad <- merge(n_egre_male, n_egre_female, all.x = TRUE, by = c('R_edad') )
n_per_sexo_edad[is.na(n_per_sexo_edad)] <- 0

# Total de personas
n_per_sexo_edad[ , total := Numero_hombres + Numero_mujeres ]
n_per_sexo_edad[ , Prom_Total := total/sum(n_per_sexo_edad$total)]
n_per_sexo_edad <- rbind( n_per_sexo_edad , data.table(R_edad = 'Total', 
                                          Numero_hombres = c(colSums(n_per_sexo_edad[, 2:2]) ),
                                          Prom_Hom = colSums(n_per_sexo_edad[, 3:3] ),
                                          Numero_mujeres = colSums(n_per_sexo_edad[, 4:4] ),
                                          Prom_Muj = colSums(n_per_sexo_edad[, 5:5] ),
                                          total = colSums(n_per_sexo_edad[, 6:6] ),
                                          Prom_Total = colSums(n_per_sexo_edad[, 7:7] )) )
n_per_sexo_edad[, Prom_Hom := Prom_Hom*100]
n_per_sexo_edad[, Prom_Muj := Prom_Muj*100]
n_per_sexo_edad[, Prom_Total := Prom_Total*100]

rm(n_egre_male, n_egre_female)


# Tabla de rangos del numero de aportes de los egresados por edad y sexo
Datos_egresados <- as.data.frame(Datos_egresados)
egresados_impo <- as.data.frame(egresados_impo)
n_imp_sexo_edad <- left_join( Datos_egresados, egresados_impo, all.x = TRUE, 
                              by = c("cedula") )

# n_imp_sexo_edad <- as.data.table(n_imp_sexo_edad)
# numero_imposiciones <- Egresados_EPN[ , list( N = .N), by = "cedula"]



# --------------------------------------------------------------------------------------------------
# Guardamos en un RData
# --------------------------------------------------------------------------------------------------
resultados <- c( "n_per_sexo_edad" )
save( list =  resultados
      , file = paste0( parametros$RData, 'Tablas_egresados.RData' ) )

# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
