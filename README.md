# 🌱 Exotic Plant Proportion Analysis in Andean Cities

This repository contains the workflow and code for analyzing the proportion of exotic plants across urban areas in the Andean region.

---

## 🔄 Workflow

### 1. Data Loading & Preparation
- **Input:** shapefile with data (`dataset.gpkg`)
- Convert `tipo` to character → recode into urban categories (`Metropolis`, `Big cities`, etc.)
- Fill missing values of `tipo` with `"Remote areas"` and convert into an ordered factor
- Drop rows with `NA` in main predictors
- Compute **exotic proportion** = exotics / (exotics + natives)
- Transform proportion to fit a beta model (avoid 0 and 1)

### 2. Initial Exploration
- Descriptive stats (mean, SD, sums) by urban level (`tipo`)
- Totals of exotics vs. natives

### 3. Predictor Preprocessing
- Scale (center & standardize) continuous variables:
  `crop percentage`, `lc diversity`, `native plants richness`, `precipmean`, `foundation date`, `fire frequency`, `urban gravity`, `tempmean`

### 4. Statistical Modeling
- **Zero/one-inflated beta mixed model** with `glmmTMB`:

  ```r
  proportion ~ predictors + (1|tipo), 
  family = beta_family(link="logit"), 
  ziformula = ~1

### 4. Statistical Modeling
- Includes quadratic effects (`temp²`, `precip²`)
- Random intercept by urban level (`tipo`)

### 5. Model Diagnostics & Summary
- `summary(r4_v2)` → coefficients & interpretation
- `exp(coef(r4_v2)$cond)` → odds ratios
- Random effects (`ranef()`) → estimates by urban level
- Fixed & random effects plots with `sjPlot::plot_model()`
- Explained variance: `VarCorr`, `r.squaredGLMM`
- Type II ANOVA (`Anova(r4_v2, type="II")`) → relative importance of predictors

### 6. Marginal Prediction Curves
- Build grids (`new_temp`, `new_prec`) keeping other predictors at their mean
- Generate predictions (`predict(..., type="response")`)
- Plot with `ggplot2`:
  - Observed points
  - Exploratory loess smoothing
  - Model prediction line

### 7. Geographical Visualizations
- Base map (`lim2.shp` + OSM tiles)
- Color administrative units by exotic proportion
- Scales and legends using `viridis`

### 8. Summary by Urban Level
- Compute by `urban level`:
  - Average exotic proportion
  - Total area (`area_km`)
- Plot:
  - Proportion bars (colored by intensity)
  - Total area bars by urban level

---

## 🔎 Main Outputs
- Beta mixed model with climate, socioeconomic, and urbanization effects
- Predicted curves for temperature and precipitation
- Spatial map of exotic plant proportion
- Summary charts by urban level (proportion and area)

---

## 📂 File Organization

**Input**
- `dataset.gpkg` → biodiversity data + predictors
- `lim2.shp` → base map

**Output**
- Updated `dataset.gpkg` with new variables (`nn-plant proportion`, scaled predictors)
- Figures (prediction curves, maps, bar plots)
- Tables of fixed/random effects, ANOVA, R²
