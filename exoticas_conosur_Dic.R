
############################################################
# Análisis de proporción de plantas exóticas en ciudades andinas
# Script reproducible en R
############################################################

# ----------------------------------------------------------------
# 0. Cargar librerías
# ----------------------------------------------------------------
library(dplyr)        # manipulación de datos
library(sf)           # lectura de shapefiles
library(glmmTMB)      # modelos mixtos generalizados
library(sjmisc)       # utilidades
library(sjPlot)       # visualización de modelos
library(ggplot2)      # gráficos
library(RColorBrewer) # paletas de colores
library(ggspatial)    # mapas
library(scales)       # escalas
library(classInt)     # intervalos para mapas
library(ggmap)        # mapas base OSM
library(report)       # reportes de modelos
library(lme4)         # comparación modelos mixtos
library(broom.mixed)  # tidy de modelos mixtos
library(MuMIn)        # R² marginal/condicional
library(car)          # ANOVA tipo II
library(patchwork)    # combinar gráficos
library(ggthemes)     # temas para ggplot
library(viridis)      # escalas viridis

# ----------------------------------------------------------------
# 1. Cargar shapefile con datos
# ----------------------------------------------------------------
data <- st_read("Shapefile/pred3.shp")
str(data)

# ----------------------------------------------------------------
# 2. Estadísticas descriptivas
# ----------------------------------------------------------------
data$tipo <- as.character(data$tipo)

mean_tax_per_group <- aggregate(data$proporcion, list(data$tipo), FUN = mean, na.rm = TRUE)
sd_tax_per_group   <- aggregate(data$proporcion, list(data$tipo), FUN = sd, na.rm = TRUE)
merge(mean_tax_per_group, sd_tax_per_group, by = "Group.1")

sum_tax_per_group <- aggregate(data$ocurrencia, list(data$tipo), FUN = sum, na.rm = TRUE)

sum_exoticas <- sum(data$exoticas, na.rm = TRUE)
sum_nativas  <- sum(data$ocurrencia, na.rm = TRUE)
sum_total    <- sum_exoticas + sum_nativas

# ----------------------------------------------------------------
# 3. Escalar variables predictoras
# ----------------------------------------------------------------
data <- data %>%
  mutate(across(c(cultivomea, div, N, precipmean, 
                  fundacionm, fuegosum, gravedadlo, tempmean),
                ~ as.numeric(scale(.)), 
                .names = "{.col}_scaled"))

# ----------------------------------------------------------------
# 4. Recodificar niveles urbanos
# ----------------------------------------------------------------
data$tipo <- ifelse(data$tipomajori == 1.00, 'Metropoli',
             ifelse(data$tipomajori == 2.00, 'Big cities',
             ifelse(data$tipomajori == 3.00, 'Intermediate cities',
             ifelse(data$tipomajori == 4.00, 'Small cities',
             ifelse(data$tipomajori == 5.00, 'Towns', NA)))))

data$tipo[is.na(data$tipo)] <- "Remote areas"

data$tipo <- factor(data$tipo, levels = c('Metropoli', 'Big cities', 
                                          'Intermediate cities', 'Small cities', 
                                          'Towns', 'Remote areas'))

# ----------------------------------------------------------------
# 5. Calcular proporción de exóticas y transformar
# ----------------------------------------------------------------
data <- data[!is.na(data$precipmean), ]
data$proporcion <- data$exoticas / (data$exoticas + data$ocurrencia)
data$proporcion <- (data$proporcion * (nrow(data) - 1) + 0.5) / nrow(data)

# ----------------------------------------------------------------
# 6. Ajustar modelo mixto beta inflado en 0/1
# ----------------------------------------------------------------
r4_v2 <- glmmTMB(
  proporcion ~ cultivomea_scaled + div_scaled + fundacionm_scaled + N_scaled +
    precipmean_scaled + I(precipmean_scaled^2) +
    tempmean_scaled   + I(tempmean_scaled^2) +
    (1|tipo),
  data = data,
  family = beta_family(link = "logit"),
  ziformula = ~1
)

summary(r4_v2)
exp(coef(r4_v2)$cond)

# ----------------------------------------------------------------
# 7. Efectos aleatorios y fijos
# ----------------------------------------------------------------
random_effects <- ranef(r4_v2, condVar = TRUE)
re_tidy <- tidy(r4_v2, effects = "ran_vals", conf.int = TRUE)

plot_model(r4_v2, type = "re", show.data = TRUE)
plot_model(r4_v2, type = "est", show.data = TRUE)

# ----------------------------------------------------------------
# 8. Varianza y ANOVA
# ----------------------------------------------------------------
VarCorr(r4_v2)
r2 <- r.squaredGLMM(r4_v2)
anova_tab <- Anova(r4_v2, type = "II")
anova_tab$Chisq / sum(anova_tab$Chisq) * 100

# ----------------------------------------------------------------
# 9. Mapa espacial
# ----------------------------------------------------------------
map <- read_sf("Shapefile/lim2.shp")

ggplot() +
  annotation_map_tile(type = "osm", zoom = 2) +
  geom_sf(data = data, aes(fill = proporcion), color = NA) +
  geom_sf(data = map, fill = NA, color = "black") +
  scale_fill_viridis_c(name = "Exotic plants proportion") +
  annotation_scale() +
  theme_classic()

# ----------------------------------------------------------------
# 10. Curvas de predicción TEMP y PRECIP
# ----------------------------------------------------------------
mean_temp <- mean(data$tempmean, na.rm = TRUE)
sd_temp   <- sd(data$tempmean, na.rm = TRUE)
mean_prec <- mean(data$precipmean, na.rm = TRUE)
sd_prec   <- sd(data$precipmean, na.rm = TRUE)
mean_cult <- mean(data$cultivomea, na.rm = TRUE)
sd_cult   <- sd(data$cultivomea, na.rm = TRUE)
mean_div  <- mean(data$div, na.rm = TRUE)
sd_div    <- sd(data$div, na.rm = TRUE)
mean_fund <- mean(data$fundacionm, na.rm = TRUE)
sd_fund   <- sd(data$fundacionm, na.rm = TRUE)
mean_N    <- mean(data$N, na.rm = TRUE)
sd_N      <- sd(data$N, na.rm = TRUE)

# Grilla TEMP
new_temp <- data.frame(
  tempmean    = seq(min(data$tempmean, na.rm=TRUE),
                    max(data$tempmean, na.rm=TRUE),
                    length.out = 200),
  precipmean  = mean_prec,
  cultivomea  = mean_cult,
  div         = mean_div,
  fundacionm  = mean_fund,
  N           = mean_N,
  tipo        = "Metropoli"
) %>%
  mutate(
    tempmean_scaled      = (tempmean - mean_temp) / sd_temp,
    precipmean_scaled    = (precipmean - mean_prec) / sd_prec,
    cultivomea_scaled    = (cultivomea - mean_cult) / sd_cult,
    div_scaled           = (div - mean_div) / sd_div,
    fundacionm_scaled    = (fundacionm - mean_fund) / sd_fund,
    N_scaled             = (N - mean_N) / sd_N,
    `I(tempmean_scaled^2)`   = tempmean_scaled^2,
    `I(precipmean_scaled^2)` = precipmean_scaled^2
  )

new_temp$fit <- predict(r4_v2, newdata = new_temp, type = "response", re.form = NA)

# Grilla PRECIP
new_prec <- data.frame(
  precipmean  = seq(min(data$precipmean, na.rm=TRUE), 
                    max(data$precipmean, na.rm=TRUE), 
                    length.out = 200),
  tempmean    = mean_temp,
  cultivomea  = mean_cult,
  div         = mean_div,
  fundacionm  = mean_fund,
  N           = mean_N,
  tipo        = "Metropoli"
) %>%
  mutate(
    tempmean_scaled      = (tempmean - mean_temp) / sd_temp,
    precipmean_scaled    = (precipmean - mean_prec) / sd_prec,
    cultivomea_scaled    = (cultivomea - mean_cult) / sd_cult,
    div_scaled           = (div - mean_div) / sd_div,
    fundacionm_scaled    = (fundacionm - mean_fund) / sd_fund,
    N_scaled             = (N - mean_N) / sd_N,
    `I(tempmean_scaled^2)`   = tempmean_scaled^2,
    `I(precipmean_scaled^2)` = precipmean_scaled^2
  )

new_prec$fit <- predict(r4_v2, newdata = new_prec, type = "response", re.form = NA)

# Graficar curvas
new_temp <- new_temp %>% arrange(tempmean)
new_prec <- new_prec %>% arrange(precipmean)

p_temp <- ggplot(data, aes(x = tempmean, y = proporcion)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue", linewidth = 2) +
  geom_line(data = new_temp, aes(x = tempmean, y = fit),
            inherit.aes = FALSE, color = "darkred", linewidth = 2) +
  theme_minimal(base_size = 14) +
  labs(x = "Mean temperature (°C)", y = "Nonnative plant proportion") +
  coord_cartesian(ylim = c(0, 1))

p_prec <- ggplot(data, aes(x = precipmean, y = proporcion)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue", linewidth = 2) +
  geom_line(data = new_prec, aes(x = precipmean, y = fit),
            inherit.aes = FALSE, color = "darkred", linewidth = 2) +
  theme_minimal(base_size = 14) +
  labs(x = "Mean precipitation (mm)", y = "") +
  coord_cartesian(ylim = c(0, 1))

final_plot <- p_temp | p_prec
final_plot

# ----------------------------------------------------------------
# 11. Resumen por nivel urbano (barras)
# ----------------------------------------------------------------
data <- as.data.frame(data)

resumen <- data %>%
  group_by(tipo) %>%
  summarise(
    mean_non_native = mean(proporcion, na.rm = TRUE),
    total_area      = sum(area_km, na.rm = TRUE),
    .groups = "drop"
  )

# Barras de proporción
ggplot(resumen, aes(x = reorder(tipo, -mean_non_native), 
                    y = mean_non_native, fill = mean_non_native)) +
  geom_col() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  labs(x = "Urban level", y = "Nonnative plants proportion") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# Barras de superficie
ggplot(resumen, aes(x = reorder(tipo, -total_area), 
                    y = total_area, fill = total_area)) +
  geom_col() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  labs(title = "Surface area (km) for each urban level") +
  theme_base(base_size = 14) +
  theme(legend.position = "none")
