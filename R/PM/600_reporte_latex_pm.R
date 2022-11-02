# Preparación
source( 'R/400_preparar_reporte.R', encoding = 'UTF-8', echo = FALSE )


# Gráficos genéricos de HIP
source( 'R/PM/400_graf_demografia_pm.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/PM/401_graf_analisis_pm.R', encoding = 'UTF-8', echo = FALSE )

# Tablas genéricas HIP
source( 'R/PM/500_tab_demografia_pm.R', encoding = 'UTF-8', echo = FALSE )


# Reporte LaTeX
source( parametros$reporte_script, encoding = 'UTF-8', echo = FALSE )
