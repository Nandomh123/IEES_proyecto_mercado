message( paste( rep('-', 100 ), collapse = '' ) )
message('\t\t Lectura de la base de datos de egresados de la EPN')

# --------------------------------------------------------------------------------------------------
# Lectura de la base de datos ehresados EPN
# --------------------------------------------------------------------------------------------------
colClasses = sapply( fread( paste0( parametros$Data,'Egresados_sueldos.txt'),
                            header = TRUE,
                            sep = 'auto',
                            showProgress = TRUE,
                            nrows = 10), class )
colClasses[6] <- 'character'


Egresados <- fread(paste0( parametros$Data, 'Egresados_sueldos.txt'),
                   header = TRUE, sep = 'auto', 
                   showProgress =  TRUE,
                   colClasses = colClasses)

# --------------------------------------------------------------------------------------------------
# Lectura de la base de datos
# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message('\t\t Lectura de la base de datos de los egresados de la EPN')
file <- paste0( parametros$Data, 'Egresados_cedula_nombre_base.xlsx' )
Egresados_EPN <- as.data.table( read_excel( file, 
                                            sheet = 2,
                                            skip = 0,
                                            col_names = TRUE ))
# --------------------------------------------------------------------------------------------------
# Lectura del numero de Imposiciones de la base de datos
# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message('\t Lectura del numero de imposiciones la base de datos de los egresados de la EPN')
Egresados_EPN_IMPO <- as.data.table( read_excel( file, 
                                                 sheet = 3,
                                                 skip = 0,
                                                 col_names = TRUE ))



# --------------------------------------------------------------------------------------------------
# Lectura del numero de los sueldos por meses
# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
message('\t Lectura del numero de imposiciones la base de datos de los egresados de la EPN')
file1 <- paste0( parametros$Data, 'CZ_SBU.xlsx' )
Sueldos_anios <- as.data.table( read_excel( file1, 
                                            sheet = 1,
                                            skip = 0,
                                            col_names = TRUE ))
# --------------------------------------------------------------------------------------------------
# Guardamos en un RData
# --------------------------------------------------------------------------------------------------

resultados <- c( 'Egresados', 'Egresados_EPN', 'Egresados_EPN_IMPO', 'Sueldos_anios' )
save( list = resultados, 
      file = paste0( parametros$RData, 'IESS_PM_Egresados_EPN.RData' ) )

# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
