message( paste( rep('-', 100 ), collapse = '' ) )
message( '\t Gráfico del Análisis del sueldo de las egresados de la EPN' )

# Plantilla gráfica ------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )


# --------------------------------------------------------------------------------------------------
# CARGAMOS LAS BASES DE DATOS
# --------------------------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_PM_Analisis_II.RData' ) )

# Graficando informacion completa ------------------------

y_lim <- c( 0, 8 )
y_brk <- seq( y_lim[1], y_lim[2], by = 1 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
x_lim <- c( -200, 240 )
x_brk <- seq( x_lim[1], x_lim[2], by = 40 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_sld_total <- ggplot( Egresados_epn_informacion, aes( x = Meses_Apor, y = Prom_Sueldo ) ) +
  geom_line( color = parametros$iess_total ) + 
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  plt_theme +
  theme_bw() +
  labs( x = 'Meses', y = 'Saldo Básicos (USD)')
   
iess_sld_total

ggsave( plot = iess_sld_total,
        filename = paste0( parametros$resultado_graficos, 'iess_sld_total', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi)

# Graficando informacion de los sueldos por sectores ------------------------
aux <- copy( Egresados_epn_informacion_sectores )
aux[ , Sector := as.character( cod_sector ) ]
aux <- aux[ Sector == 'P' | Sector == 'R' ]
aux <- aux[ Sector == 'P', Sector := 'Público' ]
aux <- aux[ Sector == 'R', Sector := 'Privado' ]

y_lim <- c( 0, 9 )
y_brk <- seq( y_lim[1], y_lim[2], by = 1 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
x_lim <- c( -200, 240 )
x_brk <- seq( x_lim[1], x_lim[2], by = 40 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_sld_sectores <- ggplot(  ) +
  # geom_line( data = Egresados_epn_informacion, aes( x = Meses_Apor, y = Prom_Sueldo ) ) +
  geom_line( data = aux, aes( x = Meses_Apor, y = Prom_Sueldo, color = Sector )  ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  plt_theme +
  theme_bw() +
  theme( legend.position = c(0.087, 0.88),
         legend.background = element_rect( fill = "white" ) ) +
  labs( x = 'Meses', y = 'Saldo Básicos (USD)') +
  scale_color_manual( values = c( parametros$iess_blue, 
                                  parametros$iess_green ),
                      labels = c( 'Privado', 'Público' ))

iess_sld_sectores


ggsave( plot = iess_sld_sectores,
        filename = paste0( parametros$resultado_graficos, 'iess_sld_sectores', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi)

