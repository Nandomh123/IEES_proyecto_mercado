message("\t\tSueldos en banca y seguros \n \t\t   Egresados de la EPN  ")
load( paste0( parametros$RData, 'IESS_PM_infor_egresados.RData' ))

# sueldo promedio en banca y seguros ---------------------------------------------------------------
# Creamos la variable con el tipo de empleo Banca, seguros y Otros.
# Para ver el sueldo simplemente filtramos a partir del 2010
banca_seg <- Egresados_EPN[ ANIPER <= 2022 ]
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
                          NOMBRE_EMPLEADOR == 'DATAEXPERTO - BURO DE INFORMACION CREDITICIA S.A.'|
                          NOMBRE_EMPLEADOR == 'BANGARA S.A.'|
                          NOMBRE_EMPLEADOR == 'CORPORACION FINANCIERA NACIONAL B.P.'|
                          NOMBRE_EMPLEADOR == 'CREDI FE DESARROLLO MICROEMPRESARIAL S.A.'|
                          NOMBRE_EMPLEADOR == 'DATAEXPERTO - BURO DE INFORMACION CREDITICIA S.A.'|
                          NOMBRE_EMPLEADOR == 'INSTITUTO ECUATORIANO DE CREDITO EDUCATIVO Y BECAS'|
                          NOMBRE_EMPLEADOR == 'PERUZZI S.A.'|
                          NOMBRE_EMPLEADOR == 'SERVICIO DE ACREDITACION ECUATORIANO'|
                          NOMBRE_EMPLEADOR == 'SICONTAC CENTER S.A.'|
                          NOMBRE_EMPLEADOR == 'COBRANZAS DEL ECUADOR S.A. RECAUDADORA'|
                          NOMBRE_EMPLEADOR == 'SICOBRA S.A.'|
                          NOMBRE_EMPLEADOR == 'RECAPT RECUPERACION DE CAPITAL CONTAC CENTER S.A.',
                        trabajo :=  "Banca" ]


banca_seg <- banca_seg[ NOMBRE_EMPLEADOR == 'INSTITUTO ECUATORIANO DE SEGURIDAD SOCIAL IESS' |
                          NOMBRE_EMPLEADOR == 'SEGUROS EQUINOCCIAL S. A.' |
                          NOMBRE_EMPLEADOR == "AFILIACION VOLUNTARIA"|
                          NOMBRE_EMPLEADOR == 'ASEGURADORA DEL SUR C. A.' |
                          NOMBRE_EMPLEADOR == 'CONFIAMED SA'|
                          NOMBRE_EMPLEADOR == 'HORIZONTES BUSINESS COMPANY S.A EN LIQUIDACION'|
                          NOMBRE_EMPLEADOR == 'MEDICINA PARA EL ECUADOR MEDIECUADOR HUMANA S.A.'|
                          NOMBRE_EMPLEADOR == 'ZURICH SEGUROS ECUADOR S.A.'|
                          NOMBRE_EMPLEADOR == 'INSTITUTO DE SEGURIDAD SOCIAL DE LAS FUERZAS ARMADAS'|
                          NOMBRE_EMPLEADOR == 'SERVICIOS ACTUARIALES CIA LTDA'|
                          NOMBRE_EMPLEADOR == 'ACTUARIA CONSULTORES S.A.',
                        trabajo := 'Seguro' ]


banca_seg <- banca_seg[ NOMBRE_EMPLEADOR == "UNIVERSIDAD CENTRAL DEL ECUADOR"
                        | NOMBRE_EMPLEADOR == "INSTITUTO NACIONAL DE EFICIENCIA ENERGETICA Y ENERGIAS RENOVABLES"
                        | NOMBRE_EMPLEADOR == "ESCUELA POLITECNICA NACIONAL"
                        | NOMBRE_EMPLEADOR == "ADECCOBUSINESS S.A."
                        | NOMBRE_EMPLEADOR == "AGENCIA NACIONAL DE REGULACION Y CONTROL DEL TRAN"
                        | NOMBRE_EMPLEADOR == "AGESINTGESTION INTEGRAL CIA LTDA."
                        | NOMBRE_EMPLEADOR == "ALMEIDA MONTALEZA JUAN GONZALO"
                        | NOMBRE_EMPLEADOR == "AMANCHA AGUIRRE MARLENE DEL ROCIO"
                        | NOMBRE_EMPLEADOR == "ANDEANECUADOR CONSULTORES ESTRATEGICOS C.L."
                        | NOMBRE_EMPLEADOR == "ANGOS VELASQUEZ FLORES INGENIERIA PROCURA CONSTRUCCION ANGVELIPC CIA.LTDA."
                        | NOMBRE_EMPLEADOR == "APARTAMENTOS Y HOTELES ECUATORIANOS APARTEC SA"
                        | NOMBRE_EMPLEADOR == "ARTEAGA GARCIA ELIECER JUVENAL"
                        | NOMBRE_EMPLEADOR == "ASAMBLEA NACIONAL."
                        | NOMBRE_EMPLEADOR == "AUCAPIÑA ESCOBAR GERMAN BOLIVAR"
                        | NOMBRE_EMPLEADOR == "AVIANCA - ECUADOR S.A."
                        | NOMBRE_EMPLEADOR == "BASTIDAS CASTRO MARIANA DE JESUS"
                        | NOMBRE_EMPLEADOR == "BINFORMA BUSINESS INFORMATION SOLUTIONS S.A."
                        | NOMBRE_EMPLEADOR == "BODEGAS PRIVADAS TERAN CIA. LTDA."
                        | NOMBRE_EMPLEADOR == "BSOFT DEVELOPERS S.A."
                        | NOMBRE_EMPLEADOR == "CAJAS Y EMPAQUES INDUSTRIALES CLUSTERPACK S.A."
                        | NOMBRE_EMPLEADOR == "CALERO LARREA LORENA ALEXANDRA"
                        | NOMBRE_EMPLEADOR == "CALERO LARREA LORENA ALEXANDRA LORENA ALEXANDRA"
                        | NOMBRE_EMPLEADOR == "CAPACITACION Y CONSULTORIA ASOMIP CIA. LTDA."
                        | NOMBRE_EMPLEADOR == "CASABACA S.A."
                        | NOMBRE_EMPLEADOR == "CENTRO DE INVESTIGACIONES MATEMATICAS APLICADAS A LA CIENCIA Y TECNOLOGIA CIMACYT C. LTDA."
                        | NOMBRE_EMPLEADOR == "CENTRO INTERNACIONAL DE ESTUDIOS SUPERIORES DE COMUNICACION PARA AMERICA LATINA"
                        | NOMBRE_EMPLEADOR == "CENTRO TERAPEUTICO MANANTIAL"
                        | NOMBRE_EMPLEADOR == "CHUGCHUKARA SERVICES S.A"
                        | NOMBRE_EMPLEADOR == "COMERCIAL ETATEX C.A."
                        | NOMBRE_EMPLEADOR == "CONESTACOMU COMUNICACIONES S.A."
                        | NOMBRE_EMPLEADOR == "CONSEJO DE ASEGURAMIENTO DE LA CALIDAD DE LA EDUCACION SUPERIOR"
                        | NOMBRE_EMPLEADOR == "CONSEJO DE LA JUDICATURA"
                        | NOMBRE_EMPLEADOR == "CONSERVAS ISABEL ECUATORIANA SA"
                        | NOMBRE_EMPLEADOR == "CONTROL DE CALIDAD HCG HIGH CONTROL CIA.LTDA."
                        | NOMBRE_EMPLEADOR == "COORDINACION ZONAL 9 - INSPI"
                        | NOMBRE_EMPLEADOR == "CORPOMEDICA CIA. LTDA."
                        | NOMBRE_EMPLEADOR == "CORPORACION DE DESARROLLO DE MERCADO SECUNDARIO DE HIPOTECAS CTH S.A."
                        | NOMBRE_EMPLEADOR == "CORPORACION FAVORITA C.A."
                        | NOMBRE_EMPLEADOR == "CORPORACION METROPOLITANA DE SEGURIDAD Y CONVIVENCIA CIUDADANA"
                        | NOMBRE_EMPLEADOR == "CORPORACION NACIONAL DE TELECOMUNICACIONES - CNT EP"
                        | NOMBRE_EMPLEADOR == "CRUZ PONCE ANDRES GONZALO"
                        | NOMBRE_EMPLEADOR == "DELIVERY HERO DH E-COMMERCE ECUADOR S.A."
                        | NOMBRE_EMPLEADOR == "DESARRA & IMEX IMPORTADORA Y EXPORTADORA S.A."
                        | NOMBRE_EMPLEADOR == "DINADEC S.A."
                        | NOMBRE_EMPLEADOR == "DIRECCION DISTRITAL 17D05 PARROQUIAS URBANAS LA CONCEPCION A JIPIJAPA Y PARROQUIAS RURALES NAYON ZAMBIZA EDUCACION"
                        | NOMBRE_EMPLEADOR == "DIRECCION GENERAL DE REGISTRO CIVIL IDENTIFICACION Y CEDULACION"
                        | NOMBRE_EMPLEADOR == "DIRECTV ECUADOR C. LTDA."
                        | NOMBRE_EMPLEADOR == "DISTRIBUIDORA IMPORTADORA DIPOR S.A."
                        | NOMBRE_EMPLEADOR == "EMPRESA MUNICIPAL DE MOVILIDAD Y OBRAS PUBLICAS"
                        | NOMBRE_EMPLEADOR == "EMPRESA PUBLICA DE HIDROCARBUROS DEL ECUADOR EP PETROECUADOR"
                        | NOMBRE_EMPLEADOR == "EMPRESA PUBLICA METROPOLITANA DE AGUA POTABLE Y SANEAMIENTO"
                        | NOMBRE_EMPLEADOR == "EMPRESA PUBLICA METROPOLITANA DE MOVILIDAD Y OBRAS PUBLICAS"
                        | NOMBRE_EMPLEADOR == "ENERGYPETROL S.A."
                        | NOMBRE_EMPLEADOR == "ESTOCASTICANALYTICS SA"
                        | NOMBRE_EMPLEADOR == "ESTRUCTURAS DE ACERO ESACERO S.A."
                        | NOMBRE_EMPLEADOR == "ETERNIT ECUATORIANA SA"
                        | NOMBRE_EMPLEADOR == "EUREKNOW SOCIEDAD ANONIMA"
                        | NOMBRE_EMPLEADOR == "EY ADDVALUE ASESORES CIA. LTDA"
                        | NOMBRE_EMPLEADOR == "F.V - AREA ANDINA S.A."
                        | NOMBRE_EMPLEADOR == "FINDES FUNDACION INTERNACIONAL PARA EL DESARROLLO EDUCATIVO Y SOCIAL"
                        | NOMBRE_EMPLEADOR == "FISCALIA GENERAL DEL ESTADO"
                        | NOMBRE_EMPLEADOR == "FORUMCONSULTOR CIA. LTDA."
                        | NOMBRE_EMPLEADOR == "FUERTES JUSTICIA RAUL OSWALDO"
                        | NOMBRE_EMPLEADOR == "FUNDACION COLEGIO AMERICANO DE QUITO"
                        | NOMBRE_EMPLEADOR == "FUNDACION DESAROLLO INTEGRAL PARA EL FUTURO FUDEN"
                        | NOMBRE_EMPLEADOR == "FUNDACION PARA EL AVANCE DE LAS REFORMAS Y LAS OPORTUNIDADES GRUPO FARO"
                        | NOMBRE_EMPLEADOR == "GARCIA CONSTANTE CARLOS IVAN"
                        | NOMBRE_EMPLEADOR == "GFKECUADOR S.A. INVESTIGACION ESTRATEGICA"
                        | NOMBRE_EMPLEADOR == "GOBIERNO AUTONOMO DESCENTRALIZADO DEL DISTRITO METROPOLITANO DE QUITO"
                        | NOMBRE_EMPLEADOR == "GOBIERNO PROVINCIAL DE IMBABURA"
                        | NOMBRE_EMPLEADOR == "GUAYASAMIN CRUZ MONICA SOLEDAD"
                        | NOMBRE_EMPLEADOR == "GUERRERO TUMIPAMBA MARIANELA CATALINA"
                        | NOMBRE_EMPLEADOR == "HABITUSINVESTIGACION S.A."
                        | NOMBRE_EMPLEADOR == "HERRERA PANCHI MARIO EDUARDO"
                        | NOMBRE_EMPLEADOR == "HOSPITAL DE ESPECIALIDADES CARLOS ANDRADE MARIN"
                        | NOMBRE_EMPLEADOR == "HUILCAMAIGUA DIAZ LUZ AURORA"
                        | NOMBRE_EMPLEADOR == "IBM DEL ECUADOR C.A."
                        | NOMBRE_EMPLEADOR == "ICESA S.A."
                        | NOMBRE_EMPLEADOR == "IMPORTADORA VEGA S.A."
                        | NOMBRE_EMPLEADOR == "INDUCALSA INDUSTRIA NACIONAL DE CALZADO S.A."
                        | NOMBRE_EMPLEADOR == "INDUSTRIAS LACTEAS TONI SA"
                        | NOMBRE_EMPLEADOR == "INFOANDINO S.A."
                        | NOMBRE_EMPLEADOR == "INGELSI CIA. LTDA."
                        | NOMBRE_EMPLEADOR == "INNOVAR CONSULTING SERVICES INCS C.L."
                        | NOMBRE_EMPLEADOR == "INSTITUTO ESPACIAL ECUATORIANO"
                        | NOMBRE_EMPLEADOR == "INSTITUTO NACIONAL DE ECONOMIA POPULAR Y SOLIDARIA"
                        | NOMBRE_EMPLEADOR == "INSTITUTO NACIONAL DE EFICIENCIA ENERGETICA Y ENERGIAS RENOVABLES"
                        | NOMBRE_EMPLEADOR == "INSTITUTO NACIONAL DE ESTADÍSTICA Y CENSOS"
                        | NOMBRE_EMPLEADOR == "INSTITUTO NACIONAL DE EVALUACION EDUCATIVA"
                        | NOMBRE_EMPLEADOR == "INSTITUTO NACIONAL DE INVESTIGACIÓN EN SALUD PUBLICA -INSPI- DR. LEOPOLDO IZQUIETA PÉREZ"
                        | NOMBRE_EMPLEADOR == "INSTITUTO NACIONAL DE INVESTIGACIONES AGROPECUARIAS INIAP"
                        | NOMBRE_EMPLEADOR == "INSTITUTO NACIONAL DE METEOROLOGIA E HIDROLOGIA"
                        | NOMBRE_EMPLEADOR == "INTEGRAL SOLUTIONS S.A."
                        | NOMBRE_EMPLEADOR == "INTENDENCIA REGIONAL DE QUITO"
                        | NOMBRE_EMPLEADOR == "INVEDELCA"
                        | NOMBRE_EMPLEADOR == "KINLENDING S.A"
                        | NOMBRE_EMPLEADOR == "LABORATORIOS BAGO DEL ECUADOR S.A."
                        | NOMBRE_EMPLEADOR == "LIBROEXPRES C.A."
                        | NOMBRE_EMPLEADOR == "LOGICIEL CIA LTDA"
                        | NOMBRE_EMPLEADOR == "LUCAS ARRIAGA NALY MARIA"
                        | NOMBRE_EMPLEADOR == "MAFLA PATINO RODRIGO MARCELO"
                        | NOMBRE_EMPLEADOR == "MARCSEAL S.A."
                        | NOMBRE_EMPLEADOR == "MARTINEZ PEREZ ELVIA DEL PILAR"
                        | NOMBRE_EMPLEADOR == "MAYA VIVAR MARIANA NOHEMI"
                        | NOMBRE_EMPLEADOR == "MEGAPROFER S.A."
                        | NOMBRE_EMPLEADOR == "MINISTERIO DE AGRICULTURA Y GANADERÍA"
                        | NOMBRE_EMPLEADOR == "MINISTERIO DE COORDINACION DE LA POLITICA ECONOMICA"
                        | NOMBRE_EMPLEADOR == "MINISTERIO DE COORDINACION DE LA PRODUCCION, EMPLEO Y COMPETITIVIDAD"
                        | NOMBRE_EMPLEADOR == "MINISTERIO DE COORDINACION DE SEGURIDAD"
                        | NOMBRE_EMPLEADOR == "MINISTERIO DE ECONOMÍA Y FINANZAS"
                        | NOMBRE_EMPLEADOR == "MINISTERIO DE EDUCACION"
                        | NOMBRE_EMPLEADOR == "MINISTERIO DE INDUSTRIAS Y PRODUCTIVIDAD"
                        | NOMBRE_EMPLEADOR == "MINISTERIO DE PRODUCCION COMERCIO EXTERIOR INVERSIONES Y PESCA"
                        | NOMBRE_EMPLEADOR == "MINISTERIO DE SALUD PUBLICA"
                        | NOMBRE_EMPLEADOR == "MINISTERIO DE TRANSPORTE Y OBRAS PUBLICAS"
                        | NOMBRE_EMPLEADOR == "MINISTERIO DEL AMBIENTE, AGUA Y TRANSICION ECOLOGICA"
                        | NOMBRE_EMPLEADOR == "MINISTERIO DEL TRABAJO"
                        | NOMBRE_EMPLEADOR == "MIVSELL TECHNOLOGY COMPANY S.A."
                        | NOMBRE_EMPLEADOR == "MODINTER S.A."
                        | NOMBRE_EMPLEADOR == "NOUX C.A"
                        | NOMBRE_EMPLEADOR == "NUOTECNOLOGICA CIA. LTDA"
                        | NOMBRE_EMPLEADOR == "OLX CLASIFICADOS ECUADOR CIA LTDA"
                        | NOMBRE_EMPLEADOR == "OPTICA LOS ANDES S.A."
                        | NOMBRE_EMPLEADOR == "OTECEL S.A."
                        | NOMBRE_EMPLEADOR == "PEPSICO ALIMENTOS ECUADOR CIA. LTDA."
                        | NOMBRE_EMPLEADOR == "PEREZ PAZMIÑO OLGA MARCIA"
                        | NOMBRE_EMPLEADOR == "PICHINCHA SISTEMAS ACOVI CA"
                        | NOMBRE_EMPLEADOR == "PIEDRA&TOLEDO S.A.S."
                        | NOMBRE_EMPLEADOR == "POLANCO PORTILLA CARLOS GERMAN"
                        | NOMBRE_EMPLEADOR == "PONTIFICIA UNIVERSIDAD CATOLICA DEL ECUADOR"
                        | NOMBRE_EMPLEADOR == "PORTALES DISTRIBUTORS INC."
                        | NOMBRE_EMPLEADOR == "PRESIDENCIA DE LA REPUBLICA"
                        | NOMBRE_EMPLEADOR == "PREX S.A.S."
                        | NOMBRE_EMPLEADOR == "PROCESADORA NACIONAL DE ALIMENTOS C A PRONACA"
                        | NOMBRE_EMPLEADOR == "PROVEFARMA S.A."
                        | NOMBRE_EMPLEADOR == "QUALA ECUADOR S A"
                        | NOMBRE_EMPLEADOR == "RADIO-HIT S.A."
                        | NOMBRE_EMPLEADOR == "REINO CHAFLA ANGEL"
                        | NOMBRE_EMPLEADOR == "SAMPEDRO CLERQUE ANA MILDRED"
                        | NOMBRE_EMPLEADOR == "SECRETARIA DE DERECHOS HUMANOS"
                        | NOMBRE_EMPLEADOR == "SECRETARIA DE EDUCACION SUPERIOR, CIENCIA, TECNOLOGIA E INNOVACION"
                        | NOMBRE_EMPLEADOR == "SECRETARIA NACIONAL DE PLANIFICACION Y DESARROLLO SENPLADES"
                        | NOMBRE_EMPLEADOR == "SECRETARIA TECNICA ECUADOR CRECE SIN DESNUTRICION INFANTIL"
                        | NOMBRE_EMPLEADOR == "SELECTFOOD S.A. EN LIQUIDACION"
                        | NOMBRE_EMPLEADOR == "SEMVRA-ECUADOR S.A."
                        | NOMBRE_EMPLEADOR == "SERTECPET S.A."
                        | NOMBRE_EMPLEADOR == "SERVICIO DE RENTAS INTERNAS"
                        | NOMBRE_EMPLEADOR == "SERVICIO INTEGRADO DE SEGURIDAD ECU 911"
                        | NOMBRE_EMPLEADOR == "SERVICIO NACIONAL DE CONTRATACION PUBLICA"
                        | NOMBRE_EMPLEADOR == "SERVICIO RAPIDO DE MOVILIZACION FASTLINE CIA. LTDA."
                        | NOMBRE_EMPLEADOR == "SERVICIOS ECUATORIANOS ATICA S.A."
                        | NOMBRE_EMPLEADOR == "SERVICIOS ESTRATEGICOS VEKTOR - SEVEKSA S.A."
                        | NOMBRE_EMPLEADOR == "SOCIEDAD DE PRODUCTORES DE FONOGRAMAS SOPROFON"
                        | NOMBRE_EMPLEADOR == "SPECTRUM OPINION Y MERCADO S.A."
                        | NOMBRE_EMPLEADOR == "SUPERINTENDENCIA DE ECONOMIA POPULAR Y SOLIDARIA"
                        | NOMBRE_EMPLEADOR == "TABARES JARA MARIA EUGENIA"
                        | NOMBRE_EMPLEADOR == "TATASOLUTION CENTER S.A."
                        | NOMBRE_EMPLEADOR == "TIENDAS INDUSTRIALES ASOCIADAS TIA S. A."
                        | NOMBRE_EMPLEADOR == "UNIDAD DEL REGISTRO SOCIAL"
                        | NOMBRE_EMPLEADOR == "UNIDAD EDUCATIVA PARTICULAR NUESTRA MADRE DE LA MERCED"
                        | NOMBRE_EMPLEADOR == "UNIVERSIDAD DE LAS AMERICAS"
                        | NOMBRE_EMPLEADOR == "UNIVERSIDAD DE INVESTIGACION DE TECNOLOGIA EXPERIMENTAL YACHAY"
                        | NOMBRE_EMPLEADOR == "UNIVERSIDAD DE LAS FUERZAS ARMADAS ESPE"
                        | NOMBRE_EMPLEADOR == "UNIVERSIDAD POLITECNICA SALESIANA"
                        | NOMBRE_EMPLEADOR == "UNIVERSIDAD REGIONAL AMAZONICA IKIAM"
                        | NOMBRE_EMPLEADOR == "UNIVERSIDAD SAN FRANCISCO DE QUITO USFQ"
                        | NOMBRE_EMPLEADOR == "UNIVERSIDAD TECNICA DE COTOPAXI"
                        | NOMBRE_EMPLEADOR == "UNIVERSIDAD TECNICA DEL NORTE"
                        | NOMBRE_EMPLEADOR == "URGILES IZURIETA ACENETH DEL ROSARIO"
                        | NOMBRE_EMPLEADOR == "VARGAS SANTANDER OSCAR EDUARDO"
                        | NOMBRE_EMPLEADOR == "VASCONEZ CHILUISA CARLOS ANTONIO"
                        | NOMBRE_EMPLEADOR == "VELEZ & VELEZ ENTERPRISE RISK MANAGEMENT S.A."
                        | NOMBRE_EMPLEADOR == "VILLARREAL HERNANDEZ RODRIGO ANIBAL"
                        | NOMBRE_EMPLEADOR == "ZAIMELLA DEL ECUADOR S. A."
                        | NOMBRE_EMPLEADOR == "EMPRESA PUBLICA METROPOLITANA DE LOGISTICA PARA LA SEGURIDAD Y LA CONVIVENCIA CIUDADANA EP"
                        | NOMBRE_EMPLEADOR == "CONSEJO NACIONAL ELECTORAL"
                        | NOMBRE_EMPLEADOR == "SECRETARIA TECNICA DEL SISTEMA NACIONAL DE CUALIFICACIONES PROFESIONALES"
                        | NOMBRE_EMPLEADOR == "VASCONEZ GALARZA CARLOS VLADIMIR"
                        | NOMBRE_EMPLEADOR == "SERVICIOS EDUCATIVOS SEGALCUS S.A."
                        | NOMBRE_EMPLEADOR == "MINISTERIO DE GOBIERNO"
                        | NOMBRE_EMPLEADOR == "AGENCIA NACIONAL DE REGULACION Y CONTROL DEL TRANSPORTE TERRESTRE TRANSITO Y SEGURIDAD VIAL."
                        | NOMBRE_EMPLEADOR == "BAKELSECUADOR S. A."
                        | NOMBRE_EMPLEADOR == "CENTRO DE TRANSFERENCIA Y DESARROLLO TECNOLOGICO ESPE - CECAI"
                        | NOMBRE_EMPLEADOR == "CONFITECA C.A."
                        | NOMBRE_EMPLEADOR == "CONSEJO NACIONAL ELECTORAL"
                        | NOMBRE_EMPLEADOR == "CORPORACION DE LA ASOCIACION DE LOS ADVENTISTAS DEL SEPTIMO DIA DEL ECUADOR"
                        | NOMBRE_EMPLEADOR == "GUERRERO TUGA ESTHER MAGDALENA"
                        | NOMBRE_EMPLEADOR == 'EMPRESA PUBLICA METROPOLITANA DE LOGISTICA PARA LA SEGURIDAD Y LA CONVIVENCIA CIUDADANA'
                        | NOMBRE_EMPLEADOR == 'SERVICIOS TEMPORARIOS S A'
                        | NOMBRE_EMPLEADOR == 'SERPAL S C C'
                        | NOMBRE_EMPLEADOR == 'TERAN NARVAEZ JORGE HUMBERTO'
                        | NOMBRE_EMPLEADOR == 'MINDMARKETING CIA. LTDA.'
                        | NOMBRE_EMPLEADOR == 'SECRETARIA TECNICA DEL MINISTERIO DE COORDINACION DE DESARROLLO SOCIAL'
                        | NOMBRE_EMPLEADOR == 'SERVICIO INTEGRAL PARA LA INDUSTRIA ALIMENTICIA SIPIA S.A.'
                        | NOMBRE_EMPLEADOR == '0'
                        | NOMBRE_EMPLEADOR == 'INT NAC ESTD Y CENSOS INEC'
                        | NOMBRE_EMPLEADOR == 'HCDA FERNANDEZ SALVADOR'
                        | NOMBRE_EMPLEADOR == 'CORPORACION NACIONAL DE TELECOMUNICACIONES CNT S.A.'
                        | NOMBRE_EMPLEADOR == 'SAMANIEGO VELEZ ALFREDO GONZALO'
                        | NOMBRE_EMPLEADOR == 'JERVES VAZQUEZ HOMERO'
                        | NOMBRE_EMPLEADOR == 'ALBAN COBOS CLARITA LUCIANA'
                        | NOMBRE_EMPLEADOR == 'UNIDAD OPERADORA DEL SISTEMA TROLEBUS'
                        | NOMBRE_EMPLEADOR == 'GLC ECUADOR S.A.'
                        | NOMBRE_EMPLEADOR == 'BESTPEOPLE S.A.'
                        | NOMBRE_EMPLEADOR == 'SOLUCION TEMPORAL CIA LTDA'
                        | NOMBRE_EMPLEADOR == 'MOSS FERREIRA ROBERT'
                        | NOMBRE_EMPLEADOR == 'PACIFIC TRADING PACITRA C.A.'
                        | NOMBRE_EMPLEADOR == 'ORDONEZ VILLACRESES FERNANDO I'
                        | NOMBRE_EMPLEADOR == 'UNIDAD DE EJECUCION ESPECIALIZADA'
                        | NOMBRE_EMPLEADOR == 'SEGURIVITAL CIA LTDA'
                        | NOMBRE_EMPLEADOR == 'SANCHEZ MARCO GABRIEL'
                        | NOMBRE_EMPLEADOR == 'BRANDIM CIA. LTDA.'
                        | NOMBRE_EMPLEADOR == 'ALBUJA DAZA ALFONSO'
                        | NOMBRE_EMPLEADOR == 'UNIDAD EDUCATIVA SAN FRANCISCO DE SALES'
                        | NOMBRE_EMPLEADOR == 'YANBAL ECUADOR S.A.'
                        | NOMBRE_EMPLEADOR == 'ALBAN COBOS CLARITA LUCIANA'
                        | NOMBRE_EMPLEADOR == 'COERH ESPECIALIZ RECURS HUMANO'
                        | NOMBRE_EMPLEADOR == 'JERVIS GONZALEZ ANDRES'
                        | NOMBRE_EMPLEADOR == 'FERNANDEZ SALVADOR SICLES RICA'
                        | NOMBRE_EMPLEADOR == 'SOSERVI S.A. EN LIQUIDACION'
                        | NOMBRE_EMPLEADOR == 'ORBISTEL S.A. EN LIQUIDACION'
                        | NOMBRE_EMPLEADOR == 'KANTAR IBOPE MEDIA ECUADOR EC-KIM S.A.'
                        | NOMBRE_EMPLEADOR == 'PERSONAL TEMPS CIA. LTDA.'
                        | NOMBRE_EMPLEADOR == 'QUIMBITA UNDA SHANTALL'
                        | NOMBRE_EMPLEADOR == 'ALARCON UTRERAS MARIA AUGUSTA'
                        | NOMBRE_EMPLEADOR == 'LICEO MATOVELLE CONGREGACION DE MISIONEROS OBLATO'
                        | NOMBRE_EMPLEADOR == 'APOLO CUENCA LUCILA NUEMIA'
                        | NOMBRE_EMPLEADOR == 'HUMANFORCE MANAGEMENT CIA. LTDA'
                        | NOMBRE_EMPLEADOR == 'ALIMENTOS Y SERVICIOS ECUATORIANOS ALISERVIS S.A.'
                        | NOMBRE_EMPLEADOR == 'MULTICINES S.A.'
                        | NOMBRE_EMPLEADOR == 'MULTICOBRO S.A.'
                        | NOMBRE_EMPLEADOR == 'AMERICAN EMPLOYEE CENTER S.A. AMERIEMPLOY'
                        | NOMBRE_EMPLEADOR == 'ACIST INTERNACIONAL, ASESORIA CONSTRUCCION, INGEN'
                        | NOMBRE_EMPLEADOR == 'CRONIX CIA. LTDA.'
                        | NOMBRE_EMPLEADOR == 'PROVEEDORA SUPERSONAL S.A'
                        | NOMBRE_EMPLEADOR == 'AIRTIFICIAL INTELLIGENCE STRUCTURES S.A.'
                        | NOMBRE_EMPLEADOR == 'EMPRESA METROPOLITANA DE ALCANTARILLADO Y AGUA POTABLE QUITO'
                        | NOMBRE_EMPLEADOR == 'CORPSISTEM CIA LTDA'
                        | NOMBRE_EMPLEADOR == 'ACIST INTERNACIONAL, ASESORIA CONSTRUCCION, INGENIERIA, SERVICIO TECNICO C.L.'
                        | NOMBRE_EMPLEADOR == 'PROVEEDORA SUPERSONAL S.A.'
                        | cedula == '1715020309',
                        trabajo := 'Otros']

# Existencia de valores nulos en la nueva variable creada
banca_seg_na <- banca_seg[ is.na(trabajo)]
banca_seg$Tipo <- NULL

# Sueldo promedio en banca, seguros y otros para el 3 de Marzo de 2022
banca_seg_3_2022 <- banca_seg[ ANIPER == 2022 & MESPER == 3]
banca_seg_3_2022 <- banca_seg_3_2022[ , list(cedula, ANIPER, MESPER, sexo,CODTIPPLA, NOMBRE_EMPLEADOR, VALSUE, trabajo)]
a <- banca_seg_3_2022[ VALSUE < 0] 
b <- banca_seg_3_2022[ cedula == '1720334901'] # Tres observaciones

# sumamos los sueldos negativos AA (Aportes ajustados)
b <- b[ NOMBRE_EMPLEADOR == 'AMANCHA AGUIRRE MARLENE DEL ROCIO', VALSUE := sum(VALSUE) ] 
b <- b[ CODTIPPLA == 'A']

c <- banca_seg_3_2022[ cedula == '1725414161' ]
# sumamos los sueldos negativos AA (Aportes ajustados)
c <- c[ NOMBRE_EMPLEADOR == 'DELIVERY HERO DH E-COMMERCE ECUADOR S.A.', VALSUE := sum(VALSUE) ] 
c <- c[ CODTIPPLA == 'A']

# Suma de sueldo de A y AA en la misma empresa , con valores negativos
banca_seg_3_2022 <- banca_seg_3_2022[ !(cedula=="1720334901" | cedula=='1725414161' ) ]
banca_seg_3_2022 <- rbind(banca_seg_3_2022,b,c)

# Sueldo en banca, seguros y otros al 3 de marzo de 2022
sld_banca_seg <- banca_seg_3_2022[ , list(Sldo_Pro = mean(VALSUE)), by = c('trabajo') ]


# Sueldo en banca por años a partir del 2010 -------------------------------------------------------
sld_banca_anio <- banca_seg[ !(VALSUE < 0) & ANIPER >= 2010]
sld_banca_anio <- sld_banca_anio[ , list( Sld_Pro = mean( VALSUE ) ), by = c( 'ANIPER', 'trabajo' )]
sld_banca_seg_otros <- cast(sld_banca_anio, ANIPER ~ trabajo)

resultados <- c("sld_banca_seg", "sld_banca_anio", "sld_banca_seg_otros")
save( list =  resultados
      , file = paste0( parametros$RData, 'IESS_PM_sld_banca_seg.RData' ) )


#-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
