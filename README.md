# Southern-Cone-Nonnative-plants

🔄 Workflow del análisis de proporción de plantas exóticas en ciudades andinas
1. Carga y preparación de datos
Entrada: shapefile con datos (pred3.shp).
Convertir tipo a carácter → recodificar en categorías urbanas (Metropoli, Big cities, etc.).
Completar valores faltantes de tipo con “Remote areas” y convertir en factor ordenado.
Eliminar filas con NA en predictores principales.
Calcular proporción de exóticas = exoticas / (exóticas + nativas).
Transformar la proporción para ajustarse a modelo beta (evitar 0 y 1).
2. Exploración inicial
Estadísticas descriptivas (media, desviación estándar, sumas) por nivel urbano (tipo).
Totales de exóticas vs. nativas.
3. Preprocesamiento de predictores
Escalar (centrar y reducir) variables continuas: cultivomea, div, N, precipmean, fundacionm, fuegosum, gravedadlo, tempmean.
4. Modelado estadístico
Modelo mixto beta inflado en 0/1 con glmmTMB:
proporcion ~ predictores + (1|tipo), 
family = beta_family(link="logit"), 
ziformula = ~1

Incluye efectos cuadráticos (temp², precip²).
Intercepto aleatorio por nivel urbano (tipo).
5. Diagnóstico y resumen del modelo
summary(r4_v2): coeficientes e interpretación.
exp(coef(r4_v2)$cond): odds ratios.
Efectos aleatorios (ranef()) → estimaciones por nivel urbano.
Efectos fijos y aleatorios con sjPlot::plot_model().
Varianza explicada (VarCorr, r.squaredGLMM).
ANOVA tipo II (Anova(r4_v2, type="II")) → importancia relativa de predictores.
6. Curvas de predicción marginales
Construir cuadrículas (new_temp, new_prec) manteniendo otros predictores en su media.
Generar predicciones (predict(..., type="response")).
Graficar con ggplot2:
Puntos observados.
Ajuste exploratorio (loess).
Línea del modelo.
7. Visualizaciones geográficas
Mapa base (lim2.shp + OSM tiles).
Colorear unidades administrativas según proporción de exóticas.
Escalas y leyendas con viridis.
8. Resumen por nivel urbano
Calcular por tipo:
Proporción promedio de exóticas.
Área total (area_km).
Graficar:
Barras de proporción (coloreadas por intensidad).
Barras de superficie total por nivel urbano.
🔎 Salidas principales
Modelo mixto beta con efectos de clima, socioeconomía y urbanización.
Curvas predichas para temperatura y precipitación.
Mapa espacial de proporción de exóticas.
Gráficos de resumen por nivel urbano (proporción y área).
📂 Organización de archivos
Entrada: Shapefile/pred3.shp (datos de biodiversidad + predictores), Shapefile/lim2.shp (mapa base).
Salida:
pred3.shp actualizado con nuevas variables (proporcion, escalados).
Figuras (curvas, mapas, barras).
Tablas de efectos fijos/aleatorios, ANOVA y R².
