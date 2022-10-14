message( paste( rep('-', 100 ), collapse = '' ) )
message( '\t Datos de la piramide poblacional de los egresados por edad y sexo' )

# --------------------------------------------------------------------------------------------------
# Cargamos la base de datos completas de los egresados de la EPN
# --------------------------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_PM_infor_egresados.RData' ))

# Tabla de rangos del número de egresados por edad y sexo
# male -----------
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
n_egre_male[ , Prom_Hom := 100*(Numero_hombres/sum(n_egre_male$Numero))]
# female -----
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
n_egre_female[ , Prom_Muj := 100*(Numero_mujeres/sum(n_egre_female$Numero))]

n_per_sexo_edad <- merge(n_egre_male, n_egre_female, all.x = TRUE, by = c('R_edad') )
n_per_sexo_edad[is.na(n_per_sexo_edad)] <- 0

# Total de personas
n_per_sexo_edad[ , total := Numero_hombres + Numero_mujeres ]
n_per_sexo_edad[ , Prom_Total := 100*(total/sum(n_per_sexo_edad$total))]
n_per_sexo_edad <- rbind( n_per_sexo_edad , data.table(R_edad = 'Total', 
                                          Numero_hombres = c(colSums(n_per_sexo_edad[, 2:2]) ),
                                          Prom_Hom = colSums(n_per_sexo_edad[, 3:3] ),
                                          Numero_mujeres = colSums(n_per_sexo_edad[, 4:4] ),
                                          Prom_Muj = colSums(n_per_sexo_edad[, 5:5] ),
                                          total = colSums(n_per_sexo_edad[, 6:6] ),
                                          Prom_Total = colSums(n_per_sexo_edad[, 7:7] )) )

rm(n_egre_male, n_egre_female)


# Tabla de rangos del numero de aportes de los egresados por edad y sexo
Datos_egresados <- as.data.frame(Datos_egresados)
egresados_impo <- as.data.frame(egresados_impo)
setnames(egresados_impo, c("RUCEMP", "NOMEMP", "...3", "cedula", "IMPOSICIONES"))
n_imp_sexo_edad <- as.data.table(left_join( Datos_egresados, egresados_impo, all.x = TRUE, 
                              by = c("cedula") ))
n_imp_sexo_edad <- n_imp_sexo_edad[!(is.na(IMPOSICIONES))]

cortes_monto <-c(-1,30, 60, 90, 120, 150, 180, 210, 300)
etiquetas <- c(paste0("$[$", formatC( c(0, 31, 91, 91, 121, 151, 181, 211),
                                      digits = 0, format = 'f', big.mark = '.', 
                                      decimal.mark = ',' ),
                      ",",formatC( c(30, 60, 90, 120, 150, 180, 210, 300),
                                   digits = 0, format = 'f', big.mark = '.', 
                                   decimal.mark = ',' ),
                      "$]$"))

aux <- data.frame(n_imp_sexo_edad)
aux <- aux %>%
  mutate(rango = cut(IMPOSICIONES, breaks = cortes_monto,
                     labels = etiquetas,
                     right = TRUE)) %>%
  group_by(sexo, rango) %>% 
  mutate(Imposiciones = n()) %>%
  ungroup() %>%
  mutate(dist=Imposiciones/n()) %>%
  distinct(sexo,rango,.keep_all = TRUE) %>%
  select(sexo,Imposiciones,rango,dist) %>%
  arrange(rango,sexo)

auxa <- spread(select(aux,-dist),sexo,value = c(Imposiciones)) %>%
  select(rango,M_ben := M, F_ben := F)
auxb <- spread(select(aux,-Imposiciones),sexo,value = c(dist)) %>%
  select(rango,M_dist:= M,F_dist:= F)

aux<-left_join(auxa,auxb,by='rango') %>%
  select(rango,M_ben,M_dist,F_ben,F_dist) %>%
  mutate(M_dist=100*M_dist,
         F_dist=100*F_dist,
         rango=as.character(rango))
aux[is.na(aux)] <- 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)
aux <- as.data.table(aux)
aux <- aux[rango == '$[$211,300$]$', rango := '>210']
n_imp_sexo_edad <- aux
rm(aux,auxb,auxa)


# Tabla de rangos del número de egresados por sector por edad y sexo 
n_sec <- Egresados_EPN[ !duplicated( Egresados_EPN, 
                            by = c('cedula', 'CODSEC'), 
                            fromLast = TRUE ) ]
n_sec <- n_sec[ CODSEC == 'P' | CODSEC == 'R' ]
n_sec <- n_sec[ , edad := ( 2022 - year(fecha_nacimiento)- 1 + 1 * ( 3 > month(fecha_nacimiento) ))]
n_sec <- n_sec[ , list( sexo, edad, CODSEC )]

# -------------------------------------------
# PUBLICO
# -------------------------------------------

# male -------
n_sec_p_male <- n_sec[ CODSEC == 'P' & sexo == 'M' ]
n_sec_p_male[ edad <= 24, R_edad := '$[21,24]$' ]
n_sec_p_male[ edad <= 29 & edad >= 25, R_edad := '$[25,29]$' ]
n_sec_p_male[ edad <= 33 & edad >= 30, R_edad := '$[30,33]$' ]
n_sec_p_male[ edad <= 37 & edad >= 34, R_edad := '$[34,37]$' ]
n_sec_p_male[ edad <= 41 & edad >= 38, R_edad := '$[38,41]$' ]
n_sec_p_male[ edad <= 46 & edad >= 42, R_edad := '$[42,46]$' ]
n_sec_p_male[ edad <= 50 & edad >= 47, R_edad := '$[47,50]$' ]
setorder(n_sec_p_male, edad)
n_sec_p_male <- n_sec_p_male[ , list(M = .N), by = c('R_edad')]
n_sec_p_male <- n_sec_p_male[ , Dist_M := 100*(M/sum(n_sec_p_male$M)) ]
# female -----
n_sec_p_female <- n_sec[ CODSEC == 'P' & sexo == 'F' ]
n_sec_p_female[ edad <= 24, R_edad := '$[21,24]$' ]
n_sec_p_female[ edad <= 29 & edad >= 25, R_edad := '$[25,29]$' ]
n_sec_p_female[ edad <= 33 & edad >= 30, R_edad := '$[30,33]$' ]
n_sec_p_female[ edad <= 37 & edad >= 34, R_edad := '$[34,37]$' ]
n_sec_p_female[ edad <= 41 & edad >= 38, R_edad := '$[38,41]$' ]
n_sec_p_female[ edad <= 46 & edad >= 42, R_edad := '$[42,46]$' ]
n_sec_p_female[ edad <= 50 & edad >= 47, R_edad := '$[47,50]$' ]
setorder(n_sec_p_female, edad)
n_sec_p_female <- n_sec_p_female[ , list(F = .N), by = c('R_edad')]
n_sec_p_female <- n_sec_p_female[ , Dist_F := 100*(F/sum(n_sec_p_female$F)) ]
# Total -----
n_sec_p_sexo_edad <- merge(n_sec_p_male, n_sec_p_female, all.x = TRUE, by = c('R_edad') )
n_sec_p_sexo_edad[is.na(n_sec_p_sexo_edad)] <- 0
n_sec_p_sexo_edad <- n_sec_p_sexo_edad[ , T := M + F]
n_sec_p_sexo_edad <- n_sec_p_sexo_edad[ , Dist_T := 100*(T/sum(n_sec_p_sexo_edad$T))]
# Totales
n_sec_p_sexo_edad <- rbind( n_sec_p_sexo_edad , data.table(R_edad = 'Total', 
                                                       M = c(colSums(n_sec_p_sexo_edad[, 2:2]) ),
                                                       Dist_M = colSums(n_sec_p_sexo_edad[, 3:3] ),
                                                       F = colSums(n_sec_p_sexo_edad[, 4:4] ),
                                                       Dist_F = colSums(n_sec_p_sexo_edad[, 5:5] ),
                                                       T = colSums(n_sec_p_sexo_edad[, 6:6] ),
                                                       Dist_T = colSums(n_sec_p_sexo_edad[, 7:7] )))
rm(n_sec_p_male, n_sec_p_female)

# -------------------------------------------
# PRIVADO
# -------------------------------------------

# male -------
n_sec_pr_male <- n_sec[ CODSEC == 'R' & sexo == 'M' ]
n_sec_pr_male[ edad <= 24, R_edad := '$[21,24]$' ]
n_sec_pr_male[ edad <= 29 & edad >= 25, R_edad := '$[25,29]$' ]
n_sec_pr_male[ edad <= 33 & edad >= 30, R_edad := '$[30,33]$' ]
n_sec_pr_male[ edad <= 37 & edad >= 34, R_edad := '$[34,37]$' ]
n_sec_pr_male[ edad <= 41 & edad >= 38, R_edad := '$[38,41]$' ]
n_sec_pr_male[ edad <= 46 & edad >= 42, R_edad := '$[42,46]$' ]
n_sec_pr_male[ edad <= 50 & edad >= 47, R_edad := '$[47,50]$' ]
setorder(n_sec_pr_male, edad)
n_sec_pr_male <- n_sec_pr_male[ , list(M = .N), by = c('R_edad')]
n_sec_pr_male <- n_sec_pr_male[ , Dist_M := 100*(M/sum(n_sec_pr_male$M)) ]
# female -----
n_sec_pr_female <- n_sec[ CODSEC == 'R' & sexo == 'F' ]
n_sec_pr_female[ edad <= 24, R_edad := '$[21,24]$' ]
n_sec_pr_female[ edad <= 29 & edad >= 25, R_edad := '$[25,29]$' ]
n_sec_pr_female[ edad <= 33 & edad >= 30, R_edad := '$[30,33]$' ]
n_sec_pr_female[ edad <= 37 & edad >= 34, R_edad := '$[34,37]$' ]
n_sec_pr_female[ edad <= 41 & edad >= 38, R_edad := '$[38,41]$' ]
n_sec_pr_female[ edad <= 46 & edad >= 42, R_edad := '$[42,46]$' ]
n_sec_pr_female[ edad <= 50 & edad >= 47, R_edad := '$[47,50]$' ]
setorder(n_sec_pr_female, edad)
n_sec_pr_female <- n_sec_pr_female[ , list(F = .N), by = c('R_edad')]
n_sec_pr_female <- n_sec_pr_female[ , Dist_F := 100*(F/sum(n_sec_pr_female$F)) ]
# Total -----
n_sec_pr_sexo_edad <- merge(n_sec_pr_male, n_sec_pr_female, all.x = TRUE, by = c('R_edad') )
n_sec_pr_sexo_edad[is.na(n_sec_pr_sexo_edad)] <- 0
n_sec_pr_sexo_edad <- n_sec_pr_sexo_edad[ , T := M + F]
n_sec_pr_sexo_edad <- n_sec_pr_sexo_edad[ , Dist_T := 100*(T/sum(n_sec_pr_sexo_edad$T))]
# Totales
n_sec_pr_sexo_edad <- rbind( n_sec_pr_sexo_edad , data.table(R_edad = 'Total', 
                                                       M = c(colSums(n_sec_pr_sexo_edad[, 2:2]) ),
                                                       Dist_M = colSums(n_sec_pr_sexo_edad[, 3:3] ),
                                                       F = colSums(n_sec_pr_sexo_edad[, 4:4] ),
                                                       Dist_F = colSums(n_sec_pr_sexo_edad[, 5:5] ),
                                                       T = colSums(n_sec_pr_sexo_edad[, 6:6] ),
                                                       Dist_T = colSums(n_sec_pr_sexo_edad[, 7:7] )))
rm(n_sec_pr_male, n_sec_pr_female, n_sec)

# --------------------------------------------------------------------------------------------------
# Guardamos en un RData
# --------------------------------------------------------------------------------------------------
resultados <- c( "n_per_sexo_edad", "n_imp_sexo_edad", "n_sec_p_sexo_edad", "n_sec_pr_sexo_edad" )
save( list =  resultados
      , file = paste0( parametros$RData, 'Tablas_egresados.RData' ) )

# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
