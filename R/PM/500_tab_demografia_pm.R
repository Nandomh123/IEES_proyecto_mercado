message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tNumero de egresados de la EPN por edad y sexo' )

load( paste0( parametros$RData, 'Tablas_egresados.RData' ) ) 
load( paste0( parametros$RData, 'IESS_PM_infor_egresados.RData' ) )


# --------------------------------------------------------------------------------------------------
# Primera forma 
# --------------------------------------------------------------------------------------------------

# Número de egresados por sexo y edad ----
aux <- copy( n_per_sexo_edad )
aux_xtable <- xtable(aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'n_per_sexo_edad', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 7,
       sanitize.text.function = identity )


# --------------------------------------------------------------------------------------------------
# Segunda Forma
# --------------------------------------------------------------------------------------------------

# Número de egresados por sexo y edad ----
cortes_monto <-c(0,24, 29, 33, 37, 41, 46, 50)
etiquetas <- c(paste0("$[$", formatC( c(20, 24, 29, 33, 37, 41, 46),
                                   digits = 0, format = 'f', big.mark = '.', 
                                   decimal.mark = ',' ),
                      ",",formatC( c(24, 29, 33, 37, 41, 46, 50),
                                   digits = 0, format = 'f', big.mark = '.', 
                                   decimal.mark = ',' ),
                      "]"))
aux <- data.frame(Egresados_EPN)
aux <- aux %>%
        mutate(y_b = year(fecha_nacimiento),
               m_b = month(fecha_nacimiento),
               edad = ( 2022 -  y_b  - 1 + 1 * ( 3 > m_b  ) )) %>%
        distinct(cedula,.keep_all = TRUE) %>%
        mutate(rango = cut(edad, breaks = cortes_monto,
                               labels = etiquetas,
                               right = TRUE)) %>%
        group_by(sexo, rango) %>%
        mutate(Personas = n()) %>%
        ungroup() %>%
        mutate(dist=Personas/n()) %>%
        distinct(sexo,rango,.keep_all = TRUE) %>%
        select(sexo,Personas,rango,dist) %>%
        arrange(rango,sexo)

auxa <- spread(select(aux,-dist),sexo,value = c(Personas)) %>%
        select(rango,M_ben := M, F_ben := F)
auxb <- spread(select(aux,-Personas),sexo,value = c(dist)) %>%
        select(rango,M_dist:= M,F_dist:= F)

aux<-left_join(auxa,auxb,by='rango') %>%
        select(rango,M_ben,M_dist,F_ben,F_dist) %>%
        mutate(M_dist=100*M_dist,
               F_dist=100*F_dist,
               rango=as.character(rango))

aux[is.na(aux)] <- 0
aux <- rbind((aux), c("Total", as.character(colSums(aux[,2:ncol(aux)]))))
aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))
aux <- aux %>% mutate(T_ben=M_ben+F_ben,
                      T_dist=M_dist+F_dist)

aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_edades_dist_rtr', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 7,
       sanitize.text.function = identity)


# Número de imposiciones de los egresados por sexo y edad ----
aux <- copy( n_imp_sexo_edad )
aux_xtable <- xtable(aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'n_imp_sexo_edad', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 8,
       sanitize.text.function = identity )

# Número de egresados en el sector público por sexo y edad ----
aux <- copy( n_sec_p_sexo_edad )
aux_xtable <- xtable(aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'n_sec_p_sexo_edad', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 7,
       sanitize.text.function = identity )


# Número de egresados en el sector privado por sexo y edad ----
aux <- copy( n_sec_pr_sexo_edad )
aux_xtable <- xtable(aux, digits = c( 0, 0, 0, 2, 0 , 2, 0, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'n_sec_pr_sexo_edad', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 7,
       sanitize.text.function = identity )

