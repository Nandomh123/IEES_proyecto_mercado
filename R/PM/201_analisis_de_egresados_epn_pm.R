message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tAnalisis de los egresados de la EPN' )

# --------------------------------------------------------------------------------------------------
# Cargamos la base de datos completas de los egresados de la EPN
# --------------------------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_PM_Lectura_egresados_epn.RData' ))
load( paste0( parametros$RData, 'IESS_Reg_Civil.Rdata' ))

# Intersección  base del Registro civil
setnames(egresados_epn, c("cedula", "RUCEMP", "ANIPER", "MESPER", "CODSEC", "CODTIPPLA", 
                          "NUMDIALAB", "VALSUE", "FECCREPLA", "NOMBRE_EMPLEADOR"))
egresados_epn <- as.data.frame(egresados_epn)
Egresados_EPN  <- left_join( egresados_epn, rc, all.x = TRUE, by = "cedula" )

# obtenemos la edad de los egresados, con la fecha de corte hasta el 31 de septiembre de 2022
Egresados_EPN <- as.data.table(Egresados_EPN)

# Eliminemos los registros que tienen ruc igual al numero de cedula
Egresados_EPN <- Egresados_EPN[ , Ced := substr(RUCEMP, start = 1, stop = 10)] 
Egresados_EPN[ cedula == Ced, Var := "Elimi"]
Egresados_EPN[ is.na(Var) , Var := "Aprue"]
Egresados_EPN <- Egresados_EPN[ Var == "Aprue"]
Egresados_EPN$Ced <- NULL
Egresados_EPN$Var <- NULL

# obtenemos la edad de los egresados, con la fecha de corte hasta el 31 marzo de 2022

piramide_edad_sexo <- Egresados_EPN[ !duplicated( Egresados_EPN, 
                                                  by = c('cedula', 'sexo', 'fecha_nacimiento'), 
                                                  fromLast = TRUE ) ]



# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------

# NOTA: Guardamos la informacion de los egresados para futuras consultas

Datos_egresados <- piramide_edad_sexo

# --------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------


message( '\tTabla para la piramide poblacional de egresados de la epn por edad y sexo ' )
piramide_edad_sexo <- piramide_edad_sexo[ , list( cedula, sexo, fecha_nacimiento )]

# obtención de la edad hasta el 31 de marzo de 2022 de los egresados.
piramide_edad_sexo[ , y_b := year(fecha_nacimiento)]
piramide_edad_sexo[ , m_b := month(fecha_nacimiento)]

# Fecha de corte
y <- 2022 
m <- 3 
piramide_edad_sexo <- piramide_edad_sexo[ ,  edad := ( y -  y_b  - 1 + 1 * ( m > m_b  )   )]
Datos_egresados <- piramide_edad_sexo
piramide_edad_sexo <- piramide_edad_sexo[ , list( n = .N), by = c('sexo', 'edad')]


# --------------------------------------------------------------------------------------------------
# Guardamos en un RData
# --------------------------------------------------------------------------------------------------
resultados <- c( "Egresados_EPN", "piramide_edad_sexo", "Datos_egresados", "egresados_impo" )
save( list =  resultados
      , file = paste0( parametros$RData, 'IESS_PM_infor_egresados.RData' ) )

# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

