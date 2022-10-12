message( paste( rep('-', 100 ), collapse = '' ) )
message('\t\t\t\t Lectura de labase de datos de los egresados de la EPN')

# --------------------------------------------------------------------------------------------------
# Lectura de la base de datos
# --------------------------------------------------------------------------------------------------
file <- paste0( parametros$Data, 'Egresados_cedula_nombre_base.xlsx' )

egresados_epn <- read_excel( file, 
                             sheet = 2,
                             skip = 0,
                             col_names = TRUE )
egresados_epn <- as.data.table( egresados_epn )

# --------------------------------------------------------------------------------------------------
# Guardamos en un RData
# --------------------------------------------------------------------------------------------------
save( egresados_epn, file = paste0( parametros$RData, 'egresados_epn.RData' ) )

# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

