message( paste( rep('-', 100 ), collapse = '' ) )
message('\t\t\t\t Lectura del egresados de la EPN')

# --------------------------------------------------------------------------------------------------
# Lectura de la base de datos
# --------------------------------------------------------------------------------------------------
colClasses = sapply( fread( paste0( parametros$Data,'Egresados.txt'),
                            header = TRUE,
                            sep = 'auto',
                            showProgress = TRUE,
                            nrows = 10), class )

Egresados <- fread(paste0( parametros$Data, 'Egresados.txt'),
                   header = TRUE, sep = 'auto', 
                   showProgress =  TRUE,
                   colClasses = colClasses)

# --------------------------------------------------------------------------------------------------
# Guardamos en un RData
# --------------------------------------------------------------------------------------------------
save( Egresados, file = paste0( parametros$RData, 'Egresados.RData' ) )

# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

