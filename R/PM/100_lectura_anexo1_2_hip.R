message( paste( rep('-', 100 ), collapse = '' ) )
# Anexo 1
message('\tLectura del Anexo 1')
file <- paste0( parametros$Data, 'anexo10529668001656970546.xls')
anexo1 <- as.data.table( read_excel( file, sheet = "IFISCERRADAS", col_names = TRUE ) )
sapply( anexo1, class )

# Corrección de fechas
anexo1$FECHA_NACIMIENTO <- as.Date( anexo1$FECHA_NACIMIENTO, format = "%Y/%m/%d" )
anexo1$FECHA_DEFUNCION <- as.Date( anexo1$FECHA_DEFUNCION, format = "%Y/%m/%d" )

# Anexo 2
message('\t Lectura del Anexo 2')
file <- paste0( parametros$Data, 'anexo20920720001656970548.xls' )
anexo2 <- as.data.table( read_excel( file, sheet = 'Segurosporcubrir', col_names = TRUE ) )

# Corrección de fechas
anexo2$`FECHA DESEMBOLSO` <- as.Date( anexo2$`FECHA DESEMBOLSO`, format = "%d/%m/%Y" ) 

# Guardando ---------------------------------------------------------------

lista <- c( 'anexo1', 'anexo2' )

save( list = lista,
      file = paste0( parametros$RData, 'IESS_HIP_anexos.RData' ) )


###########################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()