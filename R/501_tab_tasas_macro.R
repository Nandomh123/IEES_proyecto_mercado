message( paste( rep('-', 100 ), collapse = '' ) )

# Cargando información -----------------------------------------------------------------------------
message( '\tCargando datos' )
load( paste0( parametros$RData, 'IESS_tasas_macro_predicciones.RData' ) )

#Función de tildes a latex--------------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

#Tabla predicciones---------------------------------------------------------------------------------
aux <- tasas_macro_pred %>% filter( anio >= 2021 ) %>%
  mutate( tasa_activa = 100 * tasa_activa,
          tasa_pasiva = 100 * tasa_pasiva,
          inflación_prom = 100 * inflación_prom )

aux$pib_nominal <- aux$pib_nominal/1000

n <- dim(aux)[2]
aux$anio<-as.character(aux$anio)

aux_xtab <- xtable( aux, digits = c( 0, rep(2,n) ) )

print( aux_xtab, 
       file = paste0( parametros$resultado_tablas, 'iess_tasas_macro_prediccion','.tex' ),
       type = 'latex',
       include.colnames = FALSE, include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux),
       sanitize.text.function = identity)

#hipótesis macroeconómicas -------------------------------------------------------------------------
aux <- copy(hip_macro_resumen)
aux_xtable <- xtable( aux, digits = c( 0,0,3) )
aux_xtable <- tildes_a_latex(aux_xtable)

print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_hip_macro', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )



message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCreación Tablas Variables Macroecoomicas IVM (para informe IVM)' ) 

# Nota: se ha cambiado los nombres "incremento" a "variación" pues no siempre crece, a veces baja. 

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IVM/', 'IESS_IVM_variables_macroeconomicas.RData' ) ) # Tablas generadas por Magaly

# IPC ---------------------------------------------------------------------------

aux <- copy( IPC_ivm)
aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux, # set column names
                 c('Año',
                   'IPC',
                   'Inflacion'))
# View(aux)

aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_IPC_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )
# otro -----------------------------------------
aux <- copy( IPCgraf_ivm)
aux[ , Anio := as.numeric( Anio ) ]
aux <- setnames( aux, # set column names
                 c('Año',
                   'IPC',
                   'Inflacion'))
# View(aux)

aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_IPCgraf_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )
#IPC -------------------------------------------------------------------------------
# aux <- as.data.table( IPC_ivm )
# aux <- aux[ Anio > as.Date( "2002-12-31" ),
#             list( Anio = as.character(Anio), IPC, inflacion ) ]
# aux_xtab <- xtable( aux, digits = c( 0, 0, 2, 2 ) )
# 
# print( aux_xtab, 
#        file = paste0( parametros$resultado_tablas, 'iess_ipc_hist', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE, include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = NULL,
#        sanitize.text.function = identity )

# SBU -----------------------------------------------------------------------------
aux <- copy(SBU_ivm)
aux[ , Anio := as.character( Anio ) ]
aux_cuentas <- c('Año',
                 'SBU',
                 'Tasa_de_crecimiento')
# View(aux)
aux_xtable <- xtable( aux, digits = rep( 0, 4))
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_SBU_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

aux_ssc <- aux[ Anio!='2021']
aux_xtable <- xtable( aux_ssc, digits = rep( 0, 4))
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_SBU_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Salario Promedio ---------------------------------------------------------------------------

aux <- copy( Salprom_ivm)
aux[ , Salario_declarado_promedio:=Salario_declarado_promedio*12]
aux <- aux[Anio<=2020]
aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux, # set column names
                 c('Año',
                   'Salario_declarado_promedio',
                   'Incremento anual (USD)',
                   'Tasa de crecimiento (%)'))
# View(aux)

aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_Salprom_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# PIB --------------------------------------------------------------------

aux <- copy( PIB_ivm )
aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux, # set column names
                 c('Año',
                   'Crecimiento_real_del_PIB',
                   'Año',
                   'Crecimiento_real_del_PIB'))
# View(aux)

aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_PIB_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Crecimiento PIB ----------------------------------------------------

aux <- copy(crec_PIB_ivm )
aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux, # set column names
                 c('Año',
                   'CrecimientoPIB'))
# View(aux)

aux_xtable <- xtable( aux, digits = c( 0, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_crec_PIB_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# PIB VS IPC --------------------------------------------------------------------

aux <- copy( PIBvsIPC_ivm)
aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux, # set column names
                 c('Año',
                   'CrecimientoPIB',
                   'Inflacion'))
# View(aux)

aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_PIBvsIPC_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Pensiones minimas -----------------------------------------------

aux <- copy(penmin_ivm)
#aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux, # set column names
                 c('', '', '2010', '2011', '2012','2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020'))
# View(aux)
aux_xtable <- xtable( aux, digits = rep( 0, 14))

#aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_penmin_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Pensiones maximas  -----------------------------------------------

aux <- copy(penmax_ivm)
#aux[ , Anio := as.character( Anio ) ]
aux <- setnames( aux, # set column names
                 c('', '', '2010', '2011', '2012','2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020'))
# View(aux)
aux_xtable <- xtable( aux, digits = rep( 0, 14))

#aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_penmax_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Clean --------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()


#---------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
