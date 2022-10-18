
message("\t\tSueldos en banca y seguros \n \t\t   Egresados de la EPN  ")
load( paste0( parametros$RData, 'IESS_PM_infor_egresados.RData' ))

banca_seg <- Egresados_EPN[ ANIPER == 2022 & MESPER == 3]
banca_seg <- banca_seg[ , list(cedula, ANIPER, MESPER, sexo,CODTIPPLA, NOMBRE_EMPLEADOR, VALSUE)]
a <- banca_seg[ VALSUE < 0] 
b <- banca_seg[ cedula == '1720334901'] # Tres observaciones

# sumamos los sueldos negativos AA (Aportes ajustados)
b <- b[ NOMBRE_EMPLEADOR == 'AMANCHA AGUIRRE MARLENE DEL ROCIO', VALSUE := sum(VALSUE) ] 
b <- b[ CODTIPPLA == 'A']

c <- banca_seg[ cedula == '1725414161' ]
# sumamos los sueldos negativos AA (Aportes ajustados)
c <- c[ NOMBRE_EMPLEADOR == 'DELIVERY HERO DH E-COMMERCE ECUADOR S.A.', VALSUE := sum(VALSUE) ] 
c <- c[ CODTIPPLA == 'A']

# Suma de sueldo de A y AA en la misma empresa , con valores negativos
banca_seg <- banca_seg[ !(cedula=="1720334901" | cedula=='1725414161' ) ]
banca_seg <- rbind(banca_seg,b,c)


# Solo los que tengan o trabajen en Banca

banca_seg <- banca_seg[ , Tipo := substr(NOMBRE_EMPLEADOR, start = 1, stop = 5) ]
banca_seg <- banca_seg[ Tipo == 'BANCO' | Tipo == 'COOPE' | Tipo == 'BANEC' |
                        Tipo == 'EQUIF' | NOMBRE_EMPLEADOR =='EQUIECUA ANALYTICS SERVICES OF RISK S.A.'|
                        NOMBRE_EMPLEADOR == 'COBRA FACIL FABRACILISA S.A.'| 
                        NOMBRE_EMPLEADOR == 'SUPERINTENDENCIA DE BANCOS' |
                        NOMBRE_EMPLEADOR == 'MICROFACTORYCAPITAL-MFC S.A.'|
                        NOMBRE_EMPLEADOR == 'GESTION EXTERNA GESTIONA GTX S.A.'|
                        NOMBRE_EMPLEADOR == 'CAJA CENTRAL FINANCOOP'|
                        NOMBRE_EMPLEADOR == 'COMPAÑIA DE SERVICIOS AUXILIARES DEL SISTEMA FINANCIERO INTERDIN S A'|
                        NOMBRE_EMPLEADOR == 'COMPAÑIA DE SERVICIOS AUXILIARES DE GESTION DE COBRANZA RECYCOB S.A.'|
                        NOMBRE_EMPLEADOR == 'COBISCORP ECUADOR S.A'|
                        NOMBRE_EMPLEADOR == 'DATAEXPERTO - BURO DE INFORMACION CREDITICIA S.A.',
                        trabajo :=  "Banca" ]

banca_seg <- banca_seg[ NOMBRE_EMPLEADOR == 'INSTITUTO ECUATORIANO DE SEGURIDAD SOCIAL IESS' |
                        NOMBRE_EMPLEADOR == 'SEGUROS EQUINOCCIAL S. A.' |
                        NOMBRE_EMPLEADOR == 'ASEGURADORA DEL SUR C. A.' |
                        NOMBRE_EMPLEADOR == 'CONFIAMED SA',
                        trabajo := 'Seguro' ]

banca_seg <- banca_seg[ is.na(trabajo), trabajo := 'Otros']
banca_seg <- banca_seg[ , list(sexo, VALSUE, trabajo) ]

# Sueldo en banca y seguros
sld_banca_seg <- banca_seg[ , list(Sldo_Pro = mean(VALSUE)), by = c('trabajo') ]
resultados <- c("sld_banca_seg")
save( list =  resultados
      , file = paste0( parametros$RData, 'IESS_PM_sld_banca_seg.RData' ) )





