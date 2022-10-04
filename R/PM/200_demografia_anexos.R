message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tAnalisis demografico' )

# Lectura datos BIESS
load( paste0( parametros$RData, 'IESS_HIP_anexos.RData' ) )
gc() 



#### Tabla1
anexo1$Anio_Concesion <- as.numeric(str_sub(anexo1$FECHA_CONCESION, start = 1L, end = 4L))
anexo1$Mes_Concesion <- as.numeric(str_sub(anexo1$FECHA_CONCESION, start = 5L, end = 7L))
anexo1$Anios_Plazo <- anexo1$`PLAZO_CREDITO (MESES)`/12
anexo1$Anio_Fin <- as.numeric(anexo1$Anio_Concesion + anexo1$Anios_Plazo)
anexo1$Mes_Fin <- anexo1$Mes_Concesion

tabla1 <- anexo1[ , .(Numero_Creditos = uniqueN(NUMERO_CEDULA), Monto = sum(MONTO_CREDITO), Sueldo = sum(SALDO_CAPITAL) ), 
                by = c('Anio_Concesion') ]


#### Tabla2 
tabla2 <- anexo1
tabla2[, DC := 1 ]
tabla2[, date := as.Date( paste ( Anio_Concesion, Mes_Concesion, DC, sep = "/") ) ]
tabla2[, date1 := as.Date( paste ( Anio_Fin, Mes_Fin, DC, sep = "/") ) ]
tabla2[, Edad := 2022 - year( FECHA_NACIMIENTO ) ]
tabla2[ , A_C := as.character(year( date )) ]
tabla2[ , A_F := as.character(year( date1 )) ]
tabla2 <- tabla2[ , list(Numero = c(1:14),
                         Anio_Concesion = A_C,
                         Anio_Final = A_F,
                         Monto = MONTO_CREDITO,
                         Saldo = SALDO_CAPITAL,
                         Edad = Edad,
                         Dias_Mora = DIAS_MORA,
                         Tipo_afiliado = `ESTADO ACTUAL` ) ]
#### Tabla3
tabla3 <- anexo1[ , .(Numero_Creditos = uniqueN(NUMERO_CEDULA),Sueldo = sum(SALDO_CAPITAL) ),
                  by = c('ESTADO_CREDITO') ]

tabla3[, Porcentaje_Cred := (tabla3$Numero_Creditos/nrow(anexo1))*100]
tabla3[, Porcentaje_Sueldo := (tabla3$Sueldo/sum(Sueldo))*100]
tabla3 <- tabla3[ , .(ESTADO_CREDITO,
                      Numero_Creditos,
                      Porcentaje_Cred,
                      Sueldo,
                      Porcentaje_Sueldo)]


#### Tabla4 
tabla4 <- anexo2
tabla4 <- tabla4[ , .(Numero_Creditos = uniqueN(IDENTIFICACION), Saldo_Cubrir = sum(`SALDO A CUBRR`))]



# Guarda resultados ----
lista <- c("tabla1","tabla2","tabla3","tabla4")
save( list =  lista,
      file = paste0( parametros$RData, 'IESS_HIP_demografia.RData' ) )
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc() 


