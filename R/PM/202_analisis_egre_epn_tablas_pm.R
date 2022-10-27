message( paste( rep('-', 100 ), collapse = '' ) )
message( '\t Datos de la piramide poblacional de los egresados por edad y sexo' )

# --------------------------------------------------------------------------------------------------
# Cargamos la base de datos completas de los egresados de la EPN
# --------------------------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_PM_infor_egresados.RData' ))

# Tabla de rangos del número de egresados por edad y sexo ------------------------------------------
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


# Tabla de rangos del numero de aportes de los egresados por edad y sexo ---------------------------
Datos_egresados <- as.data.frame(Datos_egresados)
egresados_impo <- as.data.frame(egresados_impo)
setnames(egresados_impo, c("RUCEMP", "NOMEMP", "...3", "cedula", "IMPOSICIONES"))
n_imp <- as.data.table(left_join( Datos_egresados, egresados_impo, all.x = TRUE, 
                              by = c("cedula") ))
n_imp <- n_imp[!(is.na(IMPOSICIONES))]
n_imp <- n_imp[ , list( sexo, edad, IMPOSICIONES )]

# male -------
n_imp_male <- n_imp[ sexo == 'M' ]
n_imp_male[ edad <= 24, R_edad := '$[21,24]$' ]
n_imp_male[ edad <= 29 & edad >= 25, R_edad := '$[25,29]$' ]
n_imp_male[ edad <= 33 & edad >= 30, R_edad := '$[30,33]$' ]
n_imp_male[ edad <= 37 & edad >= 34, R_edad := '$[34,37]$' ]
n_imp_male[ edad <= 41 & edad >= 38, R_edad := '$[38,41]$' ]
n_imp_male[ edad <= 46 & edad >= 42, R_edad := '$[42,46]$' ]
n_imp_male[ edad <= 50 & edad >= 47, R_edad := '$[47,50]$' ]
setorder(n_imp_male, edad)
n_imp_male <- n_imp_male[ , list(IM = sum(IMPOSICIONES)), by = c('R_edad')]
n_imp_male <- n_imp_male[ , Dist_IM := 100*(IM/sum(n_imp_male$IM)) ]
# female -----
n_imp_female <- n_imp[ sexo == 'F' ]
n_imp_female[ edad <= 24, R_edad := '$[21,24]$' ]
n_imp_female[ edad <= 29 & edad >= 25, R_edad := '$[25,29]$' ]
n_imp_female[ edad <= 33 & edad >= 30, R_edad := '$[30,33]$' ]
n_imp_female[ edad <= 37 & edad >= 34, R_edad := '$[34,37]$' ]
n_imp_female[ edad <= 41 & edad >= 38, R_edad := '$[38,41]$' ]
n_imp_female[ edad <= 46 & edad >= 42, R_edad := '$[42,46]$' ]
n_imp_female[ edad <= 50 & edad >= 47, R_edad := '$[47,50]$' ]
setorder(n_imp_female, edad)
n_imp_female <- n_imp_female[ , list(IF = sum(IMPOSICIONES)), by = c('R_edad')]
n_imp_female <- n_imp_female[ , Dist_IF := 100*(IF/sum(n_imp_female$IF)) ]
# Total -----
n_imp_sexo_edad <- merge(n_imp_male, n_imp_female, all.x = TRUE, by = c('R_edad') )
n_imp_sexo_edad[is.na(n_imp_sexo_edad)] <- 0
n_imp_sexo_edad <- n_imp_sexo_edad[ , IT := IM + IF]
n_imp_sexo_edad <- n_imp_sexo_edad[ , Dist_IT := 100*(IT/sum(n_imp_sexo_edad$IT))]
# Totales
n_imp_sexo_edad <- rbind( n_imp_sexo_edad , data.table(R_edad = 'Total', 
                                                           IM = c(colSums(n_imp_sexo_edad[, 2:2]) ),
                                                           Dist_IM = colSums(n_imp_sexo_edad[, 3:3] ),
                                                           IF = colSums(n_imp_sexo_edad[, 4:4] ),
                                                           Dist_IF = colSums(n_imp_sexo_edad[, 5:5] ),
                                                           IT = colSums(n_imp_sexo_edad[, 6:6] ),
                                                           Dist_IT = colSums(n_imp_sexo_edad[, 7:7] )))
rm(n_imp_male, n_imp_female, n_imp)


# Tabla de rangos del número de egresados por sector por edad y sexo -------------------------------
n_sec <- Egresados_EPN[ !duplicated( Egresados_EPN, 
                            by = c('cedula', 'CODSEC'), 
                            fromLast = TRUE ) ]
n_sec <- n_sec[ CODSEC == 'P' | CODSEC == 'R' ]
n_sec <- n_sec[ , edad := ( 2022 - year(fecha_nacimiento)- 1 + 1 * ( 3 > month(fecha_nacimiento) ))]
n_sectores <- n_sec
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
                                                       Dist_T = colSums(n_sec_pr_sexo_edad[, 7:7])))

rm(n_sec_pr_male, n_sec_pr_female, piramide_edad_sexo, egresados_impo, n_sec)


# Tabla de rangos del sueldo promedio de egresados por sector por edad y sexo ----------------------

sld_sec <- Egresados_EPN[ , list( sexo, CODSEC, VALSUE , fecha_nacimiento)]
sld_sec <- sld_sec[ , edad := ( 2022 - year(fecha_nacimiento)- 1 + 1 * ( 3 > month(fecha_nacimiento) ))]
sld_sec <- sld_sec[ VALSUE > 0]
sld_sec <- sld_sec[ , list( sexo, CODSEC, edad, VALSUE)]

# -----------------------------
# PUBLICO 
# -----------------------------
# male --------
sld_sec_p_male <- sld_sec[ sexo == 'M' &  CODSEC == 'P' ]
sld_sec_p_male[ edad <= 24, R_edad := '$[21,24]$' ]
sld_sec_p_male[ edad <= 29 & edad >= 25, R_edad := '$[25,29]$' ]
sld_sec_p_male[ edad <= 33 & edad >= 30, R_edad := '$[30,33]$' ]
sld_sec_p_male[ edad <= 37 & edad >= 34, R_edad := '$[34,37]$' ]
sld_sec_p_male[ edad <= 41 & edad >= 38, R_edad := '$[38,41]$' ]
sld_sec_p_male[ edad <= 46 & edad >= 42, R_edad := '$[42,46]$' ]
sld_sec_p_male[ edad <= 50 & edad >= 47, R_edad := '$[47,50]$' ]
setorder(sld_sec_p_male, edad)
sld_sec_p_male <- sld_sec_p_male[ , list(SM = mean(VALSUE)), by = c('R_edad')]
# sld_sec_p_male <- sld_sec_p_male[ , Dist_SM := 100*(SM/sum(sld_sec_p_male$SM)) ]
# female -------
sld_sec_p_female <- sld_sec[ sexo == 'F' &  CODSEC == 'P' ]
sld_sec_p_female[ edad <= 24, R_edad := '$[21,24]$' ]
sld_sec_p_female[ edad <= 29 & edad >= 25, R_edad := '$[25,29]$' ]
sld_sec_p_female[ edad <= 33 & edad >= 30, R_edad := '$[30,33]$' ]
sld_sec_p_female[ edad <= 37 & edad >= 34, R_edad := '$[34,37]$' ]
sld_sec_p_female[ edad <= 41 & edad >= 38, R_edad := '$[38,41]$' ]
sld_sec_p_female[ edad <= 46 & edad >= 42, R_edad := '$[42,46]$' ]
sld_sec_p_female[ edad <= 50 & edad >= 47, R_edad := '$[47,50]$' ]
setorder(sld_sec_p_female, edad)
sld_sec_p_female <- sld_sec_p_female[ , list(SF = mean(VALSUE)), by = c('R_edad')]
# sld_sec_p_female <- sld_sec_p_female[ , Dist_SF := 100*(SF/sum(sld_sec_p_female$SF)) ]
# Total -------
sld_sec_p_sexo_edad <- merge(sld_sec_p_male, sld_sec_p_female, all.x = TRUE, by = c('R_edad') )
sld_sec_p_sexo_edad[is.na(sld_sec_p_sexo_edad)] <- 0
sld_sec_p_sexo_edad <- sld_sec_p_sexo_edad[ , ST := SM + SF]
#sld_sec_p_sexo_edad <- sld_sec_p_sexo_edad[ , Dist_ST := 100*(ST/sum(sld_sec_p_sexo_edad$ST))]

# Totales
sld_sec_p_sexo_edad[sld_sec_p_sexo_edad==0] <- NA
sld_sec_p_sexo_edad <- rbind( sld_sec_p_sexo_edad , data.table( R_edad = '{\\bf Promedio}', 
                                                                SM = c(colMeans(sld_sec_p_sexo_edad[, 2:2]) ),
                                                                SF = colMeans(sld_sec_p_sexo_edad[, 3:3],na.rm = T ),
                                                                ST = colMeans(sld_sec_p_sexo_edad[, 4:4] )))

rm(sld_sec_p_male, sld_sec_p_female)

# -----------------------------
# PRIVADO
# -----------------------------
# male --------
sld_sec_pr_male <- sld_sec[ sexo == 'M' &  CODSEC == 'R' ]
sld_sec_pr_male[ edad <= 24, R_edad := '$[21,24]$' ]
sld_sec_pr_male[ edad <= 29 & edad >= 25, R_edad := '$[25,29]$' ]
sld_sec_pr_male[ edad <= 33 & edad >= 30, R_edad := '$[30,33]$' ]
sld_sec_pr_male[ edad <= 37 & edad >= 34, R_edad := '$[34,37]$' ]
sld_sec_pr_male[ edad <= 41 & edad >= 38, R_edad := '$[38,41]$' ]
sld_sec_pr_male[ edad <= 46 & edad >= 42, R_edad := '$[42,46]$' ]
sld_sec_pr_male[ edad <= 50 & edad >= 47, R_edad := '$[47,50]$' ]
setorder(sld_sec_pr_male, edad)
sld_sec_pr_male <- sld_sec_pr_male[ , list(SM = mean(VALSUE)), by = c('R_edad')]
sld_sec_pr_male <- sld_sec_pr_male[ , Dist_SM := 100*(SM/sum(sld_sec_pr_male$SM)) ]
# female -------
sld_sec_pr_female <- sld_sec[ sexo == 'F' &  CODSEC == 'R' ]
sld_sec_pr_female[ edad <= 24, R_edad := '$[21,24]$' ]
sld_sec_pr_female[ edad <= 29 & edad >= 25, R_edad := '$[25,29]$' ]
sld_sec_pr_female[ edad <= 33 & edad >= 30, R_edad := '$[30,33]$' ]
sld_sec_pr_female[ edad <= 37 & edad >= 34, R_edad := '$[34,37]$' ]
sld_sec_pr_female[ edad <= 41 & edad >= 38, R_edad := '$[38,41]$' ]
sld_sec_pr_female[ edad <= 46 & edad >= 42, R_edad := '$[42,46]$' ]
sld_sec_pr_female[ edad <= 50 & edad >= 47, R_edad := '$[47,50]$' ]
setorder(sld_sec_pr_female, edad)
sld_sec_pr_female <- sld_sec_pr_female[ , list(SF = mean(VALSUE)), by = c('R_edad')]
sld_sec_pr_female <- sld_sec_pr_female[ , Dist_SF := 100*(SF/sum(sld_sec_pr_female$SF)) ]
# Total -------
sld_sec_pr_sexo_edad <- merge(sld_sec_pr_male, sld_sec_pr_female, all.x = TRUE, by = c('R_edad') )
sld_sec_pr_sexo_edad[is.na(sld_sec_pr_sexo_edad)] <- 0
sld_sec_pr_sexo_edad <- sld_sec_pr_sexo_edad[ , ST := SM + SF]
sld_sec_pr_sexo_edad <- sld_sec_pr_sexo_edad[ , Dist_ST := 100*(ST/sum(sld_sec_pr_sexo_edad$ST))]
# Totales
sld_sec_pr_sexo_edad <- rbind( sld_sec_pr_sexo_edad,data.table(R_edad = 'Total', 
                                                               SM = c(colSums(sld_sec_pr_sexo_edad[, 2:2]) ),
                                                               Dist_SM = colSums(sld_sec_pr_sexo_edad[, 3:3] ),
                                                               SF = colSums(sld_sec_pr_sexo_edad[, 4:4] ),
                                                               Dist_SF = colSums(sld_sec_pr_sexo_edad[, 5:5] ),
                                                               ST = colSums(sld_sec_pr_sexo_edad[, 6:6] ),
                                                               Dist_ST = colSums(sld_sec_pr_sexo_edad[, 7:7])))

rm(sld_sec_pr_male, sld_sec_pr_female)


# --------------------------------------------------------------------------------------------------
# Guardamos en un RData
# --------------------------------------------------------------------------------------------------
resultados <- c( "n_per_sexo_edad", "n_imp_sexo_edad", "n_sec_p_sexo_edad", "n_sec_pr_sexo_edad",
                 "sld_sec_p_sexo_edad", "sld_sec_pr_sexo_edad")
save( list =  resultados
      , file = paste0( parametros$RData, 'Tablas_egresados.RData' ) )

# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

