message( paste( rep('-', 100 ), collapse = '' ) )
message('\t\t\t\t Lectura de la base de datos de los egresados de la EPN')

# --------------------------------------------------------------------------------------------------
# Lectura de la base de datos
# --------------------------------------------------------------------------------------------------
file <- paste0( parametros$Data, 'Egresados_cedula_nombre_base.xlsx' )

egresados_epn <- as.data.table( read_excel( file, 
                                            sheet = 2,
                                            skip = 0,
                                            col_names = TRUE ))



egresados_impo <- as.data.table( read_excel( file, 
                                             sheet = 3,
                                             skip = 0,
                                             col_names = TRUE ))


# --------------------------------------------------------------------------------------------------
# Guardamos en un RData
# --------------------------------------------------------------------------------------------------
resultados <- c("egresados_epn", "egresados_impo")
save( list = resultados, 
      file = paste0( parametros$RData, 'IESS_PM_Lectura_egresados_epn.RData' ) )

# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

