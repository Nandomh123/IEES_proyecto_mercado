message( '\tEstableciendo información para la configuración del reporte' )

REP <- new.env()
load( paste0( parametros$RData, 'IESS_HIP_reserva.RData' ) )
load( paste0( parametros$RData, 'IESS_HIP_demografia.RData' ) )

# Resultados
REP$reserva_total <- format( tabla2[Tipo_afiliado=='FALLECIDO']$Saldo + tabla4$Saldo_Cubrir + sum( m_base[`ESTADO ACTUAL` != 'FALLECIDO']$SALDO_CAPITAL ) - sum( m_base[`ESTADO ACTUAL` != 'FALLECIDO']$reserva )
                                ,digits = 0, nsmall = 0, big.mark = '.', 
                                decimal.mark = ',', format = 'f' )

REP$reserva_variable <- format( sum( m_base[`ESTADO ACTUAL` != 'FALLECIDO']$SALDO_CAPITAL ) - sum( m_base[`ESTADO ACTUAL` != 'FALLECIDO']$reserva )
                            ,digits = 0, nsmall = 0, big.mark = '.', 
                            decimal.mark = ',', format = 'f' )

REP$reserva_fija <- format( tabla2[Tipo_afiliado=='FALLECIDO']$Saldo + tabla4$Saldo_Cubrir
                               ,digits = 0, nsmall = 0, big.mark = '.', 
                               decimal.mark = ',', format = 'f' )

REP$saldo_fallecido <- format( tabla2[Tipo_afiliado=='FALLECIDO']$Saldo
                           ,digits = 0, nsmall = 0, big.mark = '.', 
                           decimal.mark = ',', format = 'f' )

REP$saldo_biess <- format( tabla4$Saldo_Cubrir
                     ,digits = 0, nsmall = 0, big.mark = '.', 
                     decimal.mark = ',', format = 'f' )

REP$monto <- format( sum( m_base$MONTO_CREDITO)
                     ,digits = 0, nsmall = 0, big.mark = '.', 
                     decimal.mark = ',', format = 'f' )

REP$saldo <- format( sum( m_base$SALDO_CAPITAL )
                     ,digits = 0, nsmall = 0, big.mark = '.', 
                     decimal.mark = ',', format = 'f' )

REP$edad_pro <- format( mean( m_base$edad )
                     ,digits = 0, nsmall = 0, big.mark = '.', 
                     decimal.mark = ',', format = 'f' )

REP$vigente <- format( 100*(5/14)
                        ,digits = 0, nsmall = 0, big.mark = '.', 
                        decimal.mark = ',', format = 'f' )

REP$vigentemora <- format( 100*(9/14)
                       ,digits = 0, nsmall = 0, big.mark = '.', 
                       decimal.mark = ',', format = 'f' )
