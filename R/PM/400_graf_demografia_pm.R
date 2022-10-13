message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGraficando demografia 2009-2018' )

# Plantilla gráfica ------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Cargamos nuetra base de datos
load( paste0( parametros$RData, 'IESS_PM_infor_egresados.RData' ) )

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
#-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
