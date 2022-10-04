message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGraficando demografia 2009-2018' )

# Plantilla gráfica ------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

load(paste0( parametros$RData, 'IESS_HIP_demografia.RData' ))

# Grafico anual de concesiones y final de creditos
tabla2 <- copy(tabla2)
f1 <- tabla2[, list(Anio_Concesion)]
f1[, Fecha := "Año Concesión" ]
f1 <- as.data.table(rename(f1, Anio = Anio_Concesion))
f2 <- tabla2[, list(Anio_Final)]
f2[, Fecha := "Año Final" ]
f2 <- as.data.table(rename(f2, Anio = Anio_Final))
fig <- rbind(f1,f2)
fig <- plot_ly( fig, x = ~Anio, type = 'histogram',   color = ~Fecha, #colors = "Accent",
               opacity = 0.8 )%>% 
  layout(#title = none,
    #xaxis = list(title = "Año de Concesión y año final"),
    yaxis = list(title = "Número de Créditos "),
    autosize = TRUE,
    showlegend = TRUE,
    barmode = "group" )
# export( fig, file = paste0( parametros$resultado_graficos, 'iess_creditos', parametros$graf_ext ) )

# Exportar imagenes estaticas
Sys.setenv("plotly_username"="jendrytg")
Sys.setenv("plotly_api_key"="PfR2ZQ79vDpVraVdtkWR")
plotly_IMAGE(fig, 
             out_file = paste0( parametros$resultado_graficos, 'iess_creditos', parametros$graf_ext ),
             format = "png")
#-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
