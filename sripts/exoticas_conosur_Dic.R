
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
data <- st_read("Submission/dataset.gpkg")
str(data)
names(data)
# ----------------------------------------------------------------
# 2. Estadísticas descriptivas
# ----------------------------------------------------------------
data$urban.level <- as.character(data$urban.level)

mean_tax_per_group <- aggregate(data$nn.plant.proportion, list(data$urban.level), FUN = mean,native.richnessa.rm = TRUE)
sd_tax_per_group   <- aggregate(data$nn.plant.proportion, list(data$urban.level), FUN = sd,native.richnessa.rm = TRUE)
merge(mean_tax_per_group, sd_tax_per_group, by = "Group.1")

sum_tax_per_group <- aggregate(data$native_occurrence, list(data$urban.level), FUN = sum,native.richnessa.rm = TRUE)

sum_exoticas <- sum(data$non.native.plant.occurrence,native.richnessa.rm = TRUE)
sum_nativas  <- sum(data$native_occurrence,native.richnessa.rm = TRUE)
sum_total    <- sum_exoticas + sum_nativas

# ----------------------------------------------------------------
# 3. Escalar variables predictoras
# ----------------------------------------------------------------
data <- data %>%
  mutate(across(c(crop.percentage, lc.lc.diversityersity,native.richnessative.richness, precipmean, 
    mean.foundation, fire.frequency, urban.gravity, tempmean),
                ~ as.numeric(scale(.)), 
                .names = "{.col}_scaled"))

# ----------------------------------------------------------------
# 4. Recodificarnative.richnessiveles urbanos
# ----------------------------------------------------------------
data$urban.level <- ifelse(data$urban.level == 1.00, 'Metropoli',
             ifelse(data$urban.level == 2.00, 'Big cities',
             ifelse(data$urban.level == 3.00, 'Intermediate cities',
             ifelse(data$urban.level == 4.00, 'Small cities',
             ifelse(data$urban.level == 5.00, 'Towns',native.richnessA)))))

data$urban.level[is.na(data$urban.level)] <- "Remote areas"

data$urban.level <- factor(data$urban.level, levels = c('Metropoli', 'Big cities', 
                                          'Intermediate cities', 'Small cities', 
                                          'Towns', 'Remote areas'))

# ----------------------------------------------------------------
# 5. Calcular proporción de exóticas y transformar
# ----------------------------------------------------------------
data <- data[!is.na(data$precipmean), ]
data$nn.plant.proportion <- data$non.native.plant.occurrence / (data$non.native.plant.occurrence + data$native_occurrence)
data$nn.plant.proportion <- (data$nn.plant.proportion * (nrow(data) - 1) + 0.5) /native.richnessrow(data)

# ----------------------------------------------------------------
# 6. Ajustar modelo mixto beta inflado en 0/1
# ----------------------------------------------------------------
r4_v2 <- glmmTMB(
 nn.plant.proportion ~ crop.percentage_scaled + lc.diversity_scaled + mean.foundation_scaled +native.richness_scaled +
    precipmean_scaled + I(precipmean_scaled^2) +
    tempmean_scaled   + I(tempmean_scaled^2) +
    (1|urban.level),
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
map <- read_sf("lim2.shp")

ggplot() +
  annotation_map_tile(type = "osm", zoom = 2) +
  geom_sf(data = data, aes(fill = proporcion), color =native.richness) +
  geom_sf(data = map, fill =native.richness, color = "black") +
  scale_fill_viridis_c(name = "Non-native plants proportion") +
  annotation_scale() +
  theme_classic()

# ----------------------------------------------------------------
# 10. Curvas de predicción TEMP y PRECIP
# ----------------------------------------------------------------
# ----------------------------------------------------------------
# Grilla TEMP
# ----------------------------------------------------------------
# Calcular medias y desviaciones de cada predictor
mean_temp <- mean(data$tempmean, na.rm = TRUE)
sd_temp   <- sd(data$tempmean, na.rm = TRUE)

mean_prec <- mean(data$precipmean, na.rm = TRUE)
sd_prec   <- sd(data$precipmean, na.rm = TRUE)

mean_cult <- mean(data$crop.percentage, na.rm = TRUE)
sd_cult   <- sd(data$crop.percentage, na.rm = TRUE)

mean_lc.diversity <- mean(data$lc.diversity, na.rm = TRUE)
sd_lc.diversity   <- sd(data$lc.diversity, na.rm = TRUE)

mean_fund <- mean(data$mean.foundation, na.rm = TRUE)
sd_fund   <- sd(data$mean.foundation, na.rm = TRUE)

mean_N <- mean(data$native.richness, na.rm = TRUE)
sd_N   <- sd(data$native.richness, na.rm = TRUE)

# ----------------------------------------------------------------
# Grilla TEMP
# ----------------------------------------------------------------
new_temp <- data.frame(
  tempmean = seq(min(data$tempmean, na.rm = TRUE),
                 max(data$tempmean, na.rm = TRUE),
                 length.out = 2431),
  precipmean = mean_prec,
  crop.percentage = mean_cult,
  lc.diversity = mean_lc.diversity,
  mean.foundation = mean_fund,
  native.richness = mean_N,
  urban.level = "Metropoli"
) %>%
  mutate(
    tempmean_scaled   = (tempmean - mean_temp) / sd_temp,
    precipmean_scaled = (precipmean - mean_prec) / sd_prec,
    crop.percentage_scaled = (crop.percentage - mean_cult) / sd_cult,
    lc.diversity_scaled    = (lc.diversity - mean_lc.diversity) / sd_lc.diversity,
    mean.foundation_scaled = (mean.foundation - mean_fund) / sd_fund,
    native.richness_scaled = (native.richness - mean_N) / sd_N,
    `I(tempmean_scaled^2)`   = tempmean_scaled^2,
    `I(precipmean_scaled^2)` = precipmean_scaled^2
  )

# Predicciones
new_temp$fit <- predict(r4_v2, newdata = new_temp, type = "response", re.form = NA)


# ----------------------------------------------------------------
# Grilla PRECIP
# ----------------------------------------------------------------
new_prec <- data.frame(
  precipmean  = seq(min(data$precipmean, na.rm = TRUE), 
                    max(data$precipmean, na.rm = TRUE), 
                    length.out = 2431),
  tempmean    = mean_temp,
  crop.percentage  = mean_cult,
  lc.diversity     = mean_lc.diversity,
  mean.foundation  = mean_fund,
  native.richness  = mean_N,
  urban.level      = "Metropoli"
) %>%
  mutate(
    tempmean_scaled   = (tempmean - mean_temp) / sd_temp,
    precipmean_scaled = (precipmean - mean_prec) / sd_prec,
    crop.percentage_scaled = (crop.percentage - mean_cult) / sd_cult,
    lc.diversity_scaled    = (lc.diversity - mean_lc.diversity) / sd_lc.diversity,
    mean.foundation_scaled = (mean.foundation - mean_fund) / sd_fund,
    native.richness_scaled = (native.richness - mean_N) / sd_N,
    `I(tempmean_scaled^2)`   = tempmean_scaled^2,
    `I(precipmean_scaled^2)` = precipmean_scaled^2
  )

# Predicciones
new_prec$fit <- predict(r4_v2, newdata = new_prec, type = "response", re.form = NA)


# ----------------------------------------------------------------
# Graficar curvas
# ----------------------------------------------------------------
p_temp <- ggplot(data, aes(x = tempmean, y = nn.plant.proportion)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue", linewidth = 2) +
  geom_line(data = new_temp %>% arrange(tempmean),
            aes(x = tempmean, y = fit),
            inherit.aes = FALSE, color = "darkred", linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(x = "Mean temperature (°C)", y = "Nonnative plant proportion") +
  coord_cartesian(ylim = c(0, 1))

p_prec <- ggplot(data, aes(x = precipmean, y = nn.plant.proportion)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue", linewidth = 2) +
  geom_line(data = new_prec %>% arrange(precipmean),
            aes(x = precipmean, y = fit),
            inherit.aes = FALSE, color = "darkred", linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(x = "Mean precipitation (mm)", y = "") +
  coord_cartesian(ylim = c(0, 1))

# Plot combinado
final_plot <- p_temp | p_prec
final_plot




# =====================================================
# GRÁFICOS
# =====================================================

# (1) Exploratorio temp (respuesta)
p_temp_resp <- ggplot(data, aes(x = tempmean, y = nn.plant.proportion)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue", linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(x = "Mean temperature (°C)", y = "Proportion (response)") +
  coord_cartesian(ylim = c(0, 1))

# (2) Modelo temp (logit)
p_temp_logit <- ggplot(new_temp, aes(x = tempmean_scaled, y = fit_link)) +
  geom_line(color = "darkred", linewidth = 1.2) +
  geom_ribbon(aes(ymin = fit_link_low, ymax = fit_link_high),
              alpha = 0.2, fill = "darkred") +
  theme_minimal(base_size = 14) +
  labs(x = "Mean temperature (°C)", y = "Logit (linear predictor)")

# (3) Exploratorio precip (respuesta)
p_prec_resp <- ggplot(data, aes(x = precipmean, y = nn.plant.proportion)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue", linewidth = 1.2) +
  theme_minimal(base_size = 14) +
  labs(x = "Mean precipitation (mm)", y = "Proportion (response)") +
  coord_cartesian(ylim = c(0, 1))

# (4) Modelo precip (logit)
p_prec_logit <- ggplot(new_prec, aes(x = precipmean_scaled, y = fit_link)) +
  geom_line(color = "darkred", linewidth = 1.2) +
  geom_ribbon(aes(ymin = fit_link_low, ymax = fit_link_high),
              alpha = 0.2, fill = "darkred") +
  theme_minimal(base_size = 14) +
  labs(x = "Mean precipitation (mm)", y = "Logit (linear predictor)")

# =====================================================
# COMBINAR 4 PANELES
# =====================================================
library(patchwork)
final_plot <- (p_temp_resp | p_temp_logit) /
              (p_prec_resp | p_prec_logit)

final_plot




# ----------------------------------------------------------------
# 11. Resumen pornative.richnessivel urbano (barras)
# ----------------------------------------------------------------
data <- as.data.frame(data)

resumen <- data %>%
  group_by(tipo) %>%
  summarise(
    mean_non_native = mean(nn.plant.proportion,na.rm = TRUE),
    total_area      = sum(area_km,na.rm = TRUE),
    .groups = "drop"
  )

# Barras de proporción
ggplot(resumen, aes(x = reorder(tipo, -mean_non_native), 
                    y = mean_non_native, fill = mean_non_native)) +
  geom_col() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  labs(x = "Urban level", y = "Non-native plants proportion") +
  theme_classic(base_size = 20) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, face = "bold"),  # 👈 etiquetas eje x rotadas
      axis.text.y = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )

# Barras de superficie
ggplot(resumen, aes(x = reorder(tipo, -total_area), 
                    y = total_area, fill = total_area)) +
  geom_col() +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  labs(title = "Surface area (km) for each urban level") +
  theme_base(base_size = 14) +
  theme(legend.position = "none")
