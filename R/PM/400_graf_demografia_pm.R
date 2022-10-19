message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGraficando demografia 2009-2018' )

# Plantilla gráfica ------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Cargamos nuetra base de datos
load( paste0( parametros$RData, 'IESS_PM_infor_egresados.RData' ) )
load( paste0( parametros$RData, 'IESS_PM_sld_banca_seg.RData' ) )


# Graficando población egresada por edad y sexo de la EPN --------------------------
message( '\tGraficando población egresada por edad y sexos de la EPN' )
aux <- copy( piramide_edad_sexo )
aux <- aux[ , sexo := as.character( sexo ) ]
aux <- aux[ , n := as.numeric( n ) ]
N <- as.data.frame( ( aux[ , sum( n ), by = sexo ] ) ) 
aux[ sexo == "F", n:= -n ]
aux[ sexo == "F", n:= n/N[1,2] ]
aux[ sexo == "M", n:= n/N[2,2] ]
M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) 

salto_y <- 2
salto_x <- 0.02
brks_y <- seq( -0.12, 0.12, salto_x)
lbls_y <- paste0( as.character( c(seq(0.12, 0, -salto_x)*100, seq(salto_x, 0.12, salto_x)*100)), "%")
brks_x <- seq(20, 53, salto_y)
lbls_x <- paste0( as.character(brks_x) )

iess_pir_egresados <- ggplot(aux, aes(x = edad, y = n, fill = sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity', colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'F' ], stat = 'identity', colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "left",
                             label.hjust = 0, label.vjust = 0.5)) +
  theme( legend.position = "bottom" ) +
  scale_fill_manual(values = c(parametros$iess_green, parametros$iess_blue),
                    labels = c("Mujeres", "Hombres"))

iess_pir_egresados

ggsave( plot = iess_pir_egresados, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_egresados', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



# Graficando sueldos de la población egresada en marzo de 2022 de la EPN -----------------------------------

y_lim <- c( 0, 3000)
y_brk <- seq( y_lim[1], y_lim[2], by = 500 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_sld_banca_seg <- ggplot(sld_banca_seg, aes(x = reorder(trabajo,-Sldo_Pro), y = Sldo_Pro)) +
  geom_bar(stat="identity", fill = parametros$iess_green, color = "black", size = 0.3 ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  labs( x = '', y = 'Saldo Promedio (USD)')+
  geom_text( aes( label = paste( '$', round( Sldo_Pro, 2 ) ) ),  
             position = position_dodge( 0.9 ), 
             vjust = -0.5, 
             size = 3.5,
             color = parametros$iess_green
  ) 
iess_sld_banca_seg

ggsave( plot = iess_sld_banca_seg,
        filename = paste0( parametros$resultado_graficos, 'iess_sld_banca_seg', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi)


# Graficando sueldos de la población egresada por años a partir del 2010 de la EPN -----------------

aux <- copy(sld_banca_anio)
setnames(aux, c('Anio', 'Trabajo', 'Sueldo'))
aux <- aux[ , Anio := as.character(Anio)]
y_lim <- c( 0, 2500)
y_brk <- seq( y_lim[1], y_lim[2], by = 500 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_sld_banca_seg_anio <-  ggplot(aux, aes(x = Anio, y = Sueldo, fill = Trabajo )) +
  geom_bar(stat="identity", position="dodge") + 
  labs(x= "Año", y = "Sueldo Promedio (USD)") +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim )+
  theme_bw() +
  plt_theme_legend +
  scale_fill_manual(values=c(parametros$iess_green, parametros$iess_blue, parametros$iess_total))

iess_sld_banca_seg_anio

ggsave( plot = iess_sld_banca_seg_anio,
        filename = paste0( parametros$resultado_graficos, 'iess_sld_banca_seg_anio', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi)




# Graficando sueldos en banca de la población egresada por año -------------------------------------

aux <- copy(sld_banca_anio)
aux <- aux[ trabajo == 'Banca']
aux <- aux[ , ANIPER := as.character(ANIPER)]
y_lim <- c( 0, 2400)
y_brk <- seq( y_lim[1], y_lim[2], by = 300 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

sld_banca_anio1 <- ggplot(aux, aes(x = ANIPER, y = Sld_Pro)) +
  geom_bar(stat="identity", fill = parametros$iess_green, color = "black", size = 0.3 ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  labs( x = 'Año', y = 'Saldo Promedio (USD)')+
  geom_text( aes( label = paste( '$', round( Sld_Pro, 2 ) ) ),  
             position = position_dodge( 0.9 ), 
             vjust = -0.5, 
             size = 2,
             color = parametros$iess_green
  ) 
sld_banca_anio1

ggsave( plot = sld_banca_anio1,
        filename = paste0( parametros$resultado_graficos, 'sld_banca_anio1', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi)


#-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
