message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tDemografia creditos hipotecarios' )

load( file = paste0( parametros$RData, 'IESS_HIP_demografia.RData' ) )

# Creditos anuales ----
aux <- copy( tabla1 )
aux[, Anio_Concesion := as.character( Anio_Concesion ) ]

aux_xtable <- xtable( aux, digits = c(rep(0,5)) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_creditos_anual', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Créditos por persona ----

aux1 <- copy( tabla2 )
aux1[, Anio_Concesion := as.character( Anio_Concesion ) ]
aux1[, Anio_Final := as.character( Anio_Final ) ]

aux_xtable1 <- xtable( aux1, digits = c(rep(0,9)) )
print( aux_xtable1,
       file = paste0( parametros$resultado_tablas, 'iess_creditos_persona', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


# Estado de Créditos  ----

aux2 <- copy( tabla3 )
aux2[, ESTADO_CREDITO := as.character( ESTADO_CREDITO ) ]

aux_xtable2 <- xtable( aux2, digits = c(rep(0,6)) )
print( aux_xtable2,
       file = paste0( parametros$resultado_tablas, 'estado_credito_hip', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity)

# Credito y Saldo  ----

aux3 <- copy( tabla4 )

aux_xtable3 <- xtable( aux3, digits = c(rep(0,3)) )
print( aux_xtable3,
       file = paste0( parametros$resultado_tablas, 'credito_saldo_hip', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

########################################################

# 
# \definecolor{skyblue2}{rgb}{0.94, 0.97, 1}
# 
# \begin{table}[H]
# \captionsetup{justification=centering}
# \begin{center}
# \caption{Créditos y saldo}
# \label{tab:credito_saldo_hip}
# {\fontsize{10}{12}\selectfont
#         \multicolsep
#         \begingroup
#         \setlenght\aboverulesep{}
#         \setlenght\belowrulesep{}
#         \begin{tabular}{cr}
#         \toprule
#         \rowcolor{skyblue2}
#         Número de Créditos  &  Saldo a Cubrir \\
#         \midrule
#         \input{tablas/credito_saldo_hip.tex}
#         \bottomrule
#         \end{tabular}
#         \endgroup
# }
# \caption*{\scriptsize {\bf Fuente:} Instituto Nacional de Estadísticas y Censos. \\
#         {\bf Elaborado:} DAIE}
# \end{center}
# \end{table}


# ------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()