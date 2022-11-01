message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tAnálisis del sueldo de las egresados de la EPN' )

# --------------------------------------------------------------------------------------------------
# Cargamos las bases de los egresados de la EPN
# --------------------------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_PM_Egresados_EPN.RData' ) )
gc()


# --------------------------------------------------------------------------------------------------
# Base Egresados
# --------------------------------------------------------------------------------------------------

# Transformamos a mayusculas el titulo de la tesis
Egresados[ , tesis := toupper( tesis ) ]

# Eliminamos las tildes de los nombres y apellidos
tildes_a_latex <- function(xtb_aux) {
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Á", "A", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("É", "E", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Í", "I", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Ó", "O", x, fixed = TRUE) else x }))
  xtb_aux <- data.frame(lapply(xtb_aux, function(x) { if(is.character(x)) gsub("Ú", "U", x, fixed = TRUE) else x }))
  xtb_aux <- xtable(xtb_aux)
  return(xtb_aux)
}
Egresados <- as.data.table(tildes_a_latex(Egresados))

# Transformación a formatos tipo fechas
Egresados[ , fecha := as.Date( fecha, format = "%d/%m/%Y" ) ]
Egresados[ , fecha_nacimiento := as.Date( fecha_nacimiento, format = "%d/%m/%Y" ) ]

# Filtro de las variables de interes
Egresados <- Egresados[ , 1:8 ]
Egresados <- Egresados[ , - c( 'universidad' ) ]


# --------------------------------------------------------------------------------------------------
# Base Egresados EPN 
# --------------------------------------------------------------------------------------------------
setnames( Egresados_EPN , c( "cedula", "ruc", "anio_per", "mes_per", "cod_sector", "cod_tipo_planilla",
                             "num_dias_lab", "val_sueldo", "fecha_cred_plazo", "nombre_empeador" ) )
Egresados_EPN <- as.data.frame( Egresados_EPN )
Egresados <- as.data.frame( Egresados)
Egresados_epn_inf <- left_join( Egresados_EPN, Egresados, by = "cedula" )
Egresados_epn_inf <- as.data.table( Egresados_epn_inf )
Egresados_epn_inf <- Egresados_epn_inf[ num_dias_lab >= 30, num_dias_lab := 30 ]
Egresados_epn_inf <- Egresados_epn_inf[ num_dias_lab <= -30, num_dias_lab := -30 ]

# Creación de la columna de fecha y mes
Egresados_epn_inf[ , Fecha_1 := paste ( anio_per, mes_per, sep = "/" ) ]
Egresados_epn_inf[ , Fecha_1 := paste ( Fecha_1, '1', sep = '/' ) ]
Egresados_epn_inf[ , Fecha_1 := as.Date( Fecha_1, format = "%Y/%m/%d") ]

# Creación de los meses de diferencia
Egresados_epn_inf[ , Meses_Apor := interval( fecha, Fecha_1 ) %/% months(1) ]

# Filtamos solo los que tengan mayor a enero del 2003
Egresados_epn_inf <- Egresados_epn_inf[ Fecha_1 >= '2003-01-01' ]
Egresados_epn_inf <- Egresados_epn_inf[ val_sueldo > 0]

# Sueldos promedios
Egresados_epn_inf_sueldo_promedio <- Egresados_epn_inf[ , list( Suma_Suel = sum(val_sueldo) ), 
                                                        by = c( 'Meses_Apor', 'cedula' )]

Egresados_epn_informacion <- Egresados_epn_inf_sueldo_promedio[ ,list( Prom_Sueldo = mean( Suma_Suel ),
                                                                       N = uniqueN( cedula )), 
                                                                by = c( 'Meses_Apor')]

ggplot(data = Egresados_epn_informacion, aes( x = Meses_Apor, y = Prom_Sueldo)) + 
  geom_line() + theme_bw() + theme( legend.position = "none" ) 
  