---
  title: "Econ project"
date: "2026-04-04"
output: html_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
# Analyzing the Impact of Socioeconomic and Demographic Factors  on Voter Turnout Across Indian Assembly Constituencies

# The analysis dataset was built by merging two files from the SHRUG maintained of the Development Data Lab (devdatalab.org/shrug_download). FILE 1: pc11_pca_clean_con08.csv with  Metadata: https://docs.devdatalab.org/SHRUG-Metadata/Population%20Census/Tables/pca11-metadata/#ac08 (all census data)

# FILE 2: trivedi_elections_clean.csv with Metadata: https://docs.devdatalab.org/SHRUG-Metadata/Elections/elections-metadata/ (all electoral data)

# MERGE PROCESS
#   1. Filtered elections to post-2008 rows with valid ac08_id
#   2. Removed bye-elections (bye_election == 0) and rare "BL" constituency types
#   3. Kept only the most recent election per constituency (mostly 2018-2022)
#   4. Computed derived Census variables as rates/shares from raw counts:
#      - literacy_rate = literate population / total population * 100
#      - sc_share = SC population / total population * 100
#      - st_share = ST population / total population * 100
#      - sex_ratio = female population / male population * 1000
#      - work_participation = total workers / total population * 100
#   5. Created is_agrarian binary: 1 if (cultivators + ag laborers) > 50% of workers
#   6. Mapped state codes to regions (North, South, East, West, Central, Northeast)
#   7. Merged on ac08_id. All 3,280 PCA constituencies matched. Final N = 3,253
#      after dropping rows with missing turnout.
```


```{r}
# SETUP AND DATA LOADING

df <- read.csv('/Users/tayyabzahid/Downloads/voter_turnout_analysis_data.csv',
               stringsAsFactors = FALSE)

str(df)
head(df)
dim(df)  # Should be 3253 rows, 14 columns
```


```{r}
# DATA PREPARATION

# Convert categorical variables to factors with explicit reference levels
# constituency_type has 3 levels: GEN (General), SC (Scheduled Caste), ST (Scheduled Tribe)
# We set GEN as the reference category (most common, ~71% of seats)
df$constituency_type <- factor(df$constituency_type,
                               levels = c("GEN", "SC", "ST"))

# region has 6 levels mapped from state codes in the ac08_id
# We set South as the reference category
df$region <- factor(df$region,
                    levels = c("South", "Central", "East", 
                               "North", "Northeast", "West"))

# is_agrarian is our binary variable (0/1)
# 1 = agricultural workers (cultivators + ag laborers) exceed 50% of total workers
# 0 = non-agrarian constituency
df$is_agrarian <- factor(df$is_agrarian, levels = c(0, 1),
                         labels = c("Non-Agrarian", "Agrarian"))

# Drop any rows with missing turnout
df <- df[!is.na(df$turnout_percentage), ]

# Summary statistics for all analysis variables
summary(df[, c("turnout_percentage", "literacy_rate", "sc_share", 
               "st_share", "sex_ratio", "work_participation", 
               "n_cand", "is_agrarian", "constituency_type", "region")])
```


```{r}
# MULTIPLE LINEAR REGRESSION MODEL

# Model specification:
# DV: turnout_percentage (continuous, 0-100)
# Continuous IVs: literacy_rate, sc_share, st_share, sex_ratio,
#                 work_participation, n_cand
# Binary IV: is_agrarian (Agrarian vs Non-Agrarian) made from two raw count columns — pc11_pca_main_cl_p (total cultivators) and pc11_pca_main_al_p (total agricultural laborers) — added them together, and divided by pc11_pca_tot_work_p; threshold of 50% to create the binary variable.
# Categorical IVs: constituency_type (GEN/SC/ST), region (6 levels)

regression <- lm(turnout_percentage ~ literacy_rate + sc_share + st_share +
                   sex_ratio + work_participation + n_cand +
                   is_agrarian + constituency_type + region,
                 data = df)

# Full regression summary — this is the main results table
summary(regression)
```

```{r}
# REGRESSION EQUATION (for the report write-up)

# Print coefficients neatly
cat("\n======================================\n")
cat("REGRESSION COEFFICIENTS\n")
cat("======================================\n")
coef_table <- summary(regression)$coefficients
print(round(coef_table, 4))

# Print model fit statistics
cat("\nR-squared:", round(summary(regression)$r.squared, 4))
cat("\nAdjusted R-squared:", round(summary(regression)$adj.r.squared, 4))
cat("\nResidual Std Error:", round(summary(regression)$sigma, 3))
cat("\nF-statistic:", round(summary(regression)$fstatistic[1], 2))
cat("\n")
```


```{r}
# DIAGNOSTIC PLOTS

# Set up a 2x2 plot layout (same as the sample project of TA)
par(mfrow = c(2, 2))
plot(regression)

# Reset layout
par(mfrow = c(1, 1))

# Individual diagnostic plots with interpretation 

# 5.1 Residuals vs Fitted
# Look for: random scatter around 0 (linearity), constant spread (homoscedasticity)
plot(regression, which = 1, main = "Residuals vs Fitted")

# 5.2 Normal Q-Q Plot
# Look for: points following the diagonal line (normality of residuals)
plot(regression, which = 2, main = "Normal Q-Q")

# 5.3 Scale-Location
# Look for: flat red line and even spread (homoscedasticity)
plot(regression, which = 3, main = "Scale-Location")

# 5.4 Residuals vs Leverage
# Look for: no points outside Cook's distance lines (influential outliers)
plot(regression, which = 5, main = "Residuals vs Leverage")
```


```{r}
# MULTICOLLINEARITY TEST (VIF)


# Install and load the car package if not already available
if (!require("car")) install.packages("car")
library(car)

# Variance Inflation Factors
# Rule of thumb: VIF > 5 is concerning, VIF > 10 is serious multicollinearity

cat("VARIANCE INFLATION FACTORS\n")

vif(regression)
```


```{r}
# LEAKAGE CHECK (for the report)


# All predictors are either:
#   - Observable before the election: Census demographics (literacy, SC/ST share,
#     sex ratio, work participation) are from the 2011 Census, which precedes all
#     elections in our sample (2017-2022). Region and constituency type are fixed
#     structural features set by the Delimitation Commission.
#   - Known at election time: Number of candidates (n_cand) is finalized before
#     voting begins.
# None of the variables encode post-election outcomes.
# Conclusion: No data leakage.

```


```{r}
# 8. ADDITIONAL EXPLORATION (for robustness section)


# oes the effect of literacy differ by constituency type? 
interaction_model <- lm(turnout_percentage ~ literacy_rate * constituency_type +
                          sc_share + st_share + sex_ratio + work_participation +
                          n_cand + is_agrarian + region,
                        data = df)
cat("\n--- Interaction Model (Literacy x Constituency Type) ---\n")
summary(interaction_model)

# Check if results change with region-only subsets
# Example: South-only model to check if patterns hold within a region
south_model <- lm(turnout_percentage ~ literacy_rate + sc_share + st_share +
                    sex_ratio + work_participation + n_cand +
                    is_agrarian + constituency_type,
                  data = df[df$region == "South", ])
cat("\n--- South-Only Submodel ---\n")
summary(south_model)
```


```{r}
# EXPORT SAMPLE DATA FOR APPENDIX


# Random sample of 100 rows for the report appendix (like the bike project did)
set.seed(42)
appendix_sample <- df[sample(nrow(df), 100), 
                      c("turnout_percentage", "literacy_rate", "sc_share",
                        "st_share", "sex_ratio", "work_participation",
                        "n_cand", "is_agrarian", "constituency_type", "region")]
appendix_sample <- appendix_sample[order(appendix_sample$region, 
                                         appendix_sample$constituency_type), ]

cat("\n======================================\n")
cat("APPENDIX: SAMPLE DATA (100 rows)\n")
cat("======================================\n")
print(appendix_sample, row.names = FALSE)
```

```{r}
# ============================================================
# COMPREHENSIVE DIAGNOSTICS (per Part II diagnostics handout)
# ============================================================
# This block adds formal tests + extra plots for each of the 5
# OLS assumptions: Independence, Linearity, Homoscedasticity,
# Normality of Residuals, and Multicollinearity.
# VIF (multicollinearity) is already covered in the chunk above.
# ============================================================

# Load required packages (install if missing)
if (!require("car"))     install.packages("car")
if (!require("lmtest"))  install.packages("lmtest")
library(car)
library(lmtest)

# Pull residuals and fitted values once (used throughout)
res_raw   <- resid(regression)         # raw residuals
res_std   <- rstandard(regression)     # standardized residuals (for ±2 / ±2.5 rule)
res_stud  <- rstudent(regression)      # studentized residuals (for outlier detection)
fit_vals  <- fitted(regression)


# ------------------------------------------------------------
# 1. INDEPENDENCE OF ERRORS — Durbin-Watson test
# ------------------------------------------------------------
# H0: residuals are uncorrelated (independent)
# DW statistic ~ 2 means no autocorrelation.
# DW < 1.5 or > 2.5 suggests autocorrelation.
# Note: our data is cross-sectional (one row per constituency,
# most-recent election only), so independence should hold by
# design — this test is mainly a formality for the report.
cat("\n--- Durbin-Watson Test (Independence) ---\n")
durbinWatsonTest(regression)


# ------------------------------------------------------------
# 2. LINEARITY — Component + Residual (Partial Residual) Plots
# ------------------------------------------------------------
# crPlots show the partial relationship between each continuous
# predictor and the DV, holding others constant.
# Look for: pink (residual) line tracking the blue (fitted) line.
# Curvature in the pink line => non-linear functional form,
# suggesting a transformation (log, squared term) might be needed.
cat("\n--- Component + Residual Plots (Linearity) ---\n")
crPlots(regression, terms = ~ literacy_rate + sc_share + st_share +
          sex_ratio + work_participation + n_cand)


# ------------------------------------------------------------
# 3. HOMOSCEDASTICITY — Breusch-Pagan & NCV tests
# ------------------------------------------------------------
# H0: residuals have constant variance (homoscedastic)
# p < 0.05 => evidence of heteroscedasticity.
# Visual check (Scale-Location plot) is already in the previous
# chunk via plot(regression, which = 3).
cat("\n--- Breusch-Pagan Test (Homoscedasticity) ---\n")
bptest(regression)

cat("\n--- Non-Constant Variance (NCV) Test ---\n")
ncvTest(regression)


# ------------------------------------------------------------
# 4. NORMALITY OF RESIDUALS — Shapiro-Wilk + visual checks
# ------------------------------------------------------------
# H0: residuals are normally distributed.
# Shapiro-Wilk is sensitive in large samples (N > 3000 here),
# so we report it alongside the Q-Q plot rather than relying on
# it alone. The Q-Q plot in the previous chunk is the primary
# visual check. Per the handout: most standardized residuals
# should fall within ±2; a few beyond ±2.5/±3 are tolerable.
cat("\n--- Shapiro-Wilk Test (Normality of Residuals) ---\n")
# Shapiro-Wilk caps at N=5000, so we use a random subsample for safety
set.seed(42)
shapiro_sample <- sample(res_raw, min(5000, length(res_raw)))
shapiro.test(shapiro_sample)

# Histogram of residuals (complements the Q-Q plot)
hist(res_raw, breaks = 40, col = "lightblue",
     main = "Histogram of Residuals",
     xlab = "Residuals")

# Count standardized residuals outside the ±2 / ±2.5 thresholds
cat("\nStandardized residual checks (per handout rule of thumb):\n")
cat("  |std.res| > 2  :", sum(abs(res_std) > 2),
    "(", round(100 * mean(abs(res_std) > 2), 2), "% of obs )\n")
cat("  |std.res| > 2.5:", sum(abs(res_std) > 2.5),
    "(", round(100 * mean(abs(res_std) > 2.5), 2), "% of obs )\n")
cat("  |std.res| > 3  :", sum(abs(res_std) > 3),
    "(", round(100 * mean(abs(res_std) > 3), 2), "% of obs )\n")


# ------------------------------------------------------------
# 5. INFLUENTIAL OBSERVATIONS — Cook's Distance
# ------------------------------------------------------------
# Cook's D > 4/n is a common flag; D > 1 is a strong concern.
# The Residuals vs Leverage plot in the previous chunk also
# shows Cook's distance contours.
cat("\n--- Cook's Distance Plot ---\n")
plot(regression, which = 4, main = "Cook's Distance")

cooks_d  <- cooks.distance(regression)
n_obs    <- nrow(df)
threshold <- 4 / n_obs
cat("\nCook's distance threshold (4/n):", round(threshold, 5), "\n")
cat("Observations exceeding 4/n threshold:", sum(cooks_d > threshold),
    "(", round(100 * mean(cooks_d > threshold), 2), "% of obs )\n")
cat("Observations with Cook's D > 1 (high concern):",
    sum(cooks_d > 1), "\n")

# Top 5 most influential observations
cat("\nTop 5 most influential observations (by Cook's D):\n")
top_influential <- order(cooks_d, decreasing = TRUE)[1:5]
print(data.frame(
  row_index   = top_influential,
  cooks_d     = round(cooks_d[top_influential], 4),
  std_resid   = round(res_std[top_influential], 3),
  fitted      = round(fit_vals[top_influential], 2),
  actual      = round(df$turnout_percentage[top_influential], 2)
))


# ------------------------------------------------------------
# 6. OUTLIERS IN RESPONSE — Studentized residuals (Bonferroni)
# ------------------------------------------------------------
# car::outlierTest reports the largest studentized residual and
# applies a Bonferroni correction. p < 0.05 flags a true outlier.
cat("\n--- Outlier Test (Bonferroni-corrected studentized residuals) ---\n")
outlierTest(regression)


# ------------------------------------------------------------
# DIAGNOSTICS SUMMARY (paste-ready for the report)
# ------------------------------------------------------------
cat("\n========================================================\n")
cat("DIAGNOSTICS SUMMARY\n")
cat("========================================================\n")
cat("Assumption          | Test/Plot                | Result\n")
cat("--------------------+--------------------------+--------\n")
cat("Independence        | Durbin-Watson            | see DW above\n")
cat("Linearity           | crPlots / Resid vs Fit   | see plots\n")
cat("Homoscedasticity    | Breusch-Pagan, NCV       | see p-values above\n")
cat("Normality           | Shapiro-Wilk + Q-Q plot  | see SW + Q-Q\n")
cat("Multicollinearity   | VIF                      | see VIF table\n")
cat("Influential obs.    | Cook's D, Resid v Lev    | see counts above\n")
cat("========================================================\n")
```

```{r}
# Investigating the zero-turnout cases — likely cancelled/boycotted elections
zero_turnout <- df[df$turnout_percentage < 5, ]
cat("Constituencies with turnout < 5%:", nrow(zero_turnout), "\n")
print(zero_turnout[, c("turnout_percentage", "region", "constituency_type", "n_cand")])
```


