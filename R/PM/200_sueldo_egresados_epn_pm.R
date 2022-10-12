message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tAnalisis del sueldo de las egresados de la EPN' )

# --------------------------------------------------------------------------------------------------
# Cargamos la base de datos del registro civil y la base de los egresados de la EPN
# --------------------------------------------------------------------------------------------------
load(paste0( parametros$RData, 'IESS_Reg_Civil.RData' ))
load(paste0( parametros$RData, 'Egresados.RData'))
gc()

# Transformamos a mayusculas el nombre
Egresados[ , `NOMBRES Y APELLIDOS`:= toupper(`NOMBRES Y APELLIDOS`) ]

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
setnames(Egresados, c('nombre','carrera','universidad'))
Egresados[ carrera == "Matemática", carrera := "MATEMATICA"]
Egresados[ carrera == "Ingeniería Matemática", carrera := "INGENIERIA MATEMATICA"]
Egresados[ carrera == "Matemática Aplicada", carrera := "MATEMATICA APLICADA"]

# Intersección  base del Registro civil
Egresados <- as.data.frame(Egresados)
Egresados_sueldos <- left_join( Egresados, rc, by = "nombre" )


# Duplicados por nombre
Egresados_dupli <- as.data.table(Egresados_sueldos)
Egresados_dupli <- Egresados_dupli[ , .N, by = c( 'nombre' )]

# Analisis para ver que persona que tienen el mismo nombre se graduo en la EPN
Egresados_sueldos <- as.data.table(Egresados_sueldos)
persona1 <- Egresados_sueldos[nombre == "GONZALEZ VALLEJO CARLOS LUIS"]
Egresados_sueldos <- Egresados_sueldos[!(cedula == '0915389753' & nombre == "GONZALEZ VALLEJO CARLOS LUIS")]
persona2 <- Egresados_sueldos[nombre == "ORTIZ LOPEZ DIEGO FERNANDO"]
Egresados_sueldos <- Egresados_sueldos[!(cedula == '0401316682' & nombre == "ORTIZ LOPEZ DIEGO FERNANDO")]
persona3 <- Egresados_sueldos[nombre == "ORTIZ CASTRO JONATHAN ALEJANDRO" ]
Egresados_sueldos <- Egresados_sueldos[!(cedula == '0926191990' & nombre == "ORTIZ CASTRO JONATHAN ALEJANDRO")]

# Guardamos en un archivo de excel
write.xlsx(Egresados_sueldos, "Egresados_sueldos.xlsx")

# Solo los nombres y numero de cédula
Egresados_cedula_nombre <- Egresados_sueldos[ , list(cedula, nombre)]
write.xlsx(Egresados_cedula_nombre, "Egresados_cedula_nombre.xlsx")


# Guarda resultados ----
lista <- c("Egresados_sueldos")
save( list =  lista,
      file = paste0( parametros$RData, 'IESS_PM_Egresados_sueldos.RData' ) )
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc() 


