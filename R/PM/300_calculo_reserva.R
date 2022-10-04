message( paste( rep('-', 100 ), collapse = '' ) )
message('\tCalculo reserva')
# Lectura de datos
load( paste0( parametros$RData, 'IESS_HIP_anexos.RData' ) )
# Lectura de conmutados
file <- paste0( 'C:/Users/jendry.toapanta/Documents/PROYECTO/ACUMULADO 2022/IESS_estudio_magisterio/MAGISTERIO DOCUMENTACION/', 'Coeficientes.xlsx' )

conmu_hombres <- read_excel(file, sheet = "Hoja3"
                            ,col_names = TRUE, guess_max = 24000 )
conmu_hombres <- as.data.table( conmu_hombres )
conmu_hombres <- conmu_hombres[ , list( edad, vDxs, vNxs, SEXO = 1 )]

conmu_mujeres <- read_excel( file, sheet = "Hoja4"
                             ,col_names = TRUE, guess_max = 24000 )
conmu_mujeres <- as.data.table( conmu_mujeres )
conmu_mujeres <- conmu_mujeres[ , list( edad, vDxs, vNxs, SEXO = 2 )]

conmutados <- rbind( conmu_hombres, conmu_mujeres )
conmutados_n <- conmutados[ , list( edad_n = edad, SEXO, vDxs_n = vDxs, vNxs_n = vNxs ) ]

# Proceso datos anexo 1
anexo1[ , ANIO_CONCESION := as.numeric( str_sub( FECHA_CONCESION, start = 1L, end = 4L) ) ]
anexo1[ , MES_CONCESION := as.numeric( str_sub( FECHA_CONCESION, start = 5L, end = 7L) ) ]
anexo1[ , ANIOS_PLAZO := `PLAZO_CREDITO (MESES)`/ 12 ]
anexo1[ , ANIO_FIN := as.numeric( ANIO_CONCESION + ANIOS_PLAZO ) ]
anexo1[ , MES_FIN := MES_CONCESION ]
anexo1[ , MESES_MORA := DIAS_MORA / 30 ]
anexo1[ , ANIOS_MORA := round( MESES_MORA / 12, 0 ) ]
anexo1[ , edad := 2022 - year( FECHA_NACIMIENTO ) ]

# Registro civil
file <- paste0( 'C:/Users/jendry.toapanta/Documents/PROYECTO/ACUMULADO 2021/ESTUDIO ACTUARIAL ENFERMEDADES CATASTROFICAS/BASES/', 'RegistroCivil_07_01_2021.csv' )
reg <- fread( file, header = TRUE, sep = 'auto', dec = ",", showProgress = TRUE )
reg[ , F_NACPER := as.Date( FECNACPER, "%d/%m/%Y" ) ]
reg[ , F_DEFPER := as.Date( FECDEFPER, "%d/%m/%Y" ) ]
reg[ , NUMERO_CEDULA := CEDIDEUSU ]
reg <- reg[ , list( NUMERO_CEDULA, GENPER, F_NACPER, F_DEFPER ) ]

# Une registro civil con anexo 1
anexo1 <- merge( anexo1, reg, all.x = TRUE, by = 'NUMERO_CEDULA' )
anexo1[ , SEXO := GENPER ]

# Anios por pagar
anexo1[ , anios_pago := ANIO_FIN - 2022 ]

# Conmutados en la tabla anexo 1
m_base <- merge( anexo1, conmutados, all.x = TRUE, by = c( 'edad', 'SEXO' ) )
m_base[ , edad_n := edad + anios_pago ]
m_base <- merge( m_base, conmutados_n, all.x = TRUE, by = c( 'edad_n', 'SEXO' ) )
# m_base$fil_aporte <- NULL
# m_base$fil_edad <- NULL
# m_base$aux_fil <- NULL

# Calculo de la reserva
m_base[ , cuota_anual := ( 12 * CUOTA_MENSUAL ) ]
m_base[ , reserva := ( ( vNxs - vNxs_n ) / vDxs ) * cuota_anual ]
sum( m_base$reserva )
sum(m_base$SALDO_CAPITAL)-sum( m_base$reserva )



# Guardando ---------------------------------------------------------------

lista <- c( 'm_base' )

save( list = lista,
      file = paste0( parametros$RData, 'IESS_HIP_reserva.RData' ) )


###########################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()