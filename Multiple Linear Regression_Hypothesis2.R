###############################################################################
# --- Simple Volatility & Regression Analysis ---
###############################################################################
library(quantmod)
library(lubridate)
library(zoo)
library(ggplot2)
library(dplyr)
library(lmtest)
library(sandwich)
library(gridExtra)
library(car)
library(leaps)
library(lmtest)

rm(list = ls(all.names = TRUE))

###############################################################################
# 1️⃣ Download data
###############################################################################


# SOXX  → Semiconductor ETF (main target)
# SPY   → S&P 500 ETF
# ^VXN  → Nasdaq VIX (proxy for implied volatility)
# REMX  → Rare Earth/Metals ETF (commodity exposure)
# ^NYICDX → ICE US DOLLAR INDEX
# CL=F → Crude Oil Futures (WTI) – proxy for energy prices
# CNY=X → USD/CNY Exchange Rate – U.S. dollar / Chinese yuan exchange rate
# DGS10 → 10-year Treasury Yield (interest rate level)
# T10YIE → Breakeven inflation rate (inflation expectations)
# HACK → Cybersecurity ETF – exposure to the cybersecurity sector

tickers_yahoo <- c("SOXX", "SPY", "^VXN", "REMX", "^NYICDX", "CL=F", "CNY=X", "HACK")
tickers_fred  <- c("DGS10", "T10YIE")

getSymbols(tickers_yahoo, from = Sys.Date() - years(10), src = "yahoo")
getSymbols(tickers_fred,  from = Sys.Date() - years(10), src = "FRED")

###############################################################################
# 2️⃣Convert all tickers to weekly frequency
###############################################################################


to_weekly_manual <- function(x, week_day = 5) {
  # week_day: 0 = Sunday ... 5 = Friday ... 6 = Saturday. Default is 5 => Friday.
  
  # 1) Extract the last numeric column (Adjusted)
  core_vec <- as.numeric(coredata(x[, ncol(x), drop = TRUE]))
  dates    <- as.Date(index(x))
  
  # 2) Build a data.frame and remove rows where date or value is NA
  df <- data.frame(date = dates, value = core_vec)
  df <- df[!is.na(df$date) & !is.na(df$value), , drop = FALSE]
  
  if (nrow(df) == 0) {
    warning("Serie vacía después de eliminar NA; devolviendo xts vacío")
    return(xts(order.by = as.Date(character(0))))
  }
  
  # 3) Compute 'week_end' = the date corresponding to the chosen week_day (e.g., Friday)
  #    For each date, find the next occurrence of the selected week_day within the same week.
  wday_num <- as.integer(format(df$date, "%w"))  # 0=Sun ... 6=Sat
  # distance in days to reach the selected week_day (mod 7)
  delta <- ((week_day - wday_num) %% 7)
  df$week_end <- df$date + delta
  df$week_end <- as.Date(df$week_end)
  
  # 4) Group by week_end and take the OBSERVATION with the most recent date (last of the week)
  df_week <- df %>%
    group_by(week_end) %>%
    slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(week_end)
  
  # 5) Build xts object with index = week_end and value = value
  xts_week <- xts(df_week$value, order.by = df_week$week_end)
  colnames(xts_week) <- colnames(x)[ncol(x)]   # keep original column name
  
  return(xts_week)
}

# -------------------------
# Apply to all series
# -------------------------
soxx_w     <- to_weekly_manual(SOXX)     # SOXX weekly (Friday-end)
spy_w      <- to_weekly_manual(SPY)
vxn_w      <- to_weekly_manual(VXN)
remx_w     <- to_weekly_manual(REMX)
usdindex_w <- to_weekly_manual(NYICDX)
oil_w      <- to_weekly_manual(`CL=F`)
cny_w      <- to_weekly_manual(`CNY=X`)
hack_w     <- to_weekly_manual(HACK)

# FRED series 
dgs10_w    <- to_weekly_manual(DGS10)
t10yie_w   <- to_weekly_manual(T10YIE)


cat("Rows per series (weekly):\n")
cat("SOXX:", NROW(soxx_w), "  SPY:", NROW(spy_w), "  VXN:", NROW(vxn_w), "\n")
cat("REMX:", NROW(remx_w), "  NYICDX:", NROW(usdindex_w), "  OIL:", NROW(oil_w), "\n")
cat("CNY:", NROW(cny_w), "  HACK:", NROW(hack_w), "\n")
cat("DGS10:", NROW(dgs10_w), "  T10YIE:", NROW(t10yie_w), "\n")


# Convert xts object into data frame

vix_df <- data.frame(
  date = index(vxn_w),
  coredata(vxn_w)
)

# Categorize VIX levels into 5 regimes
vix_df <- vix_df %>%
  mutate(
    VIX_Regime = case_when(
      vxn_w < 18 ~ "Very low (<18)",
      vxn_w >= 18 & vxn_w <= 24 ~ "Normal (18-24)",
      vxn_w >= 25 & vxn_w <= 29 ~ "Elevated (25-29)",
      vxn_w >= 30 & vxn_w <= 39 ~ "High (30-39)",
      vxn_w >= 40 ~ "Extreme (>=40)",
      
    ),
    VIX_Regime = factor(
      VIX_Regime,
      levels = c("Very low (<18)",
                 "Normal (18-24)",
                 "Elevated (25-29)",
                 "High (30-39)",
                 "Extreme (>=40)"),
      ordered = TRUE
    )
  )

vix_df <- vix_df[c("date", "VIX_Regime")]
###############################################################################
# 3️⃣ Compute returns / first differences
###############################################################################

soxx_ret  <- diff(log(soxx_w))      # SOXX weekly log-returns
spy_ret <- diff(log(spy_w))         # SPY weekly returns
remx_ret  <- diff(log(remx_w))      # REMX weekly log-returns
dgs10_dlog  <- diff(log(dgs10_w))   # Weekly percentage change in 10-year Treasury yield
t10yie_dlog <- diff(log(t10yie_w))  # Weekly percentage change in inflation expectations
usdindex_ret <- diff(log(usdindex_w)) # Weekly  USD Index
oil_ret <- diff(log(oil_w))         # WTI weekly log-returns
cny_ret <- diff(log(cny_w))   # CNY/USD weekly log returns
hack_ret <- diff(log(hack_w)) 


###############################################################################
# 4️⃣ Build combined weekly dataset
###############################################################################
#vix_regime <- xts(vix_df[["VIX_Regime"]], order.by = index(spy_ret))

data_weekly <- merge(
  soxx_ret, remx_ret, dgs10_dlog, t10yie_dlog, usdindex_ret, 
  oil_ret, cny_ret, hack_ret,
  join = "inner"
)


colnames(data_weekly) <- c(
  "SOXX_Return", "REMX_Return", "DGS10_Diff", "T10YIE_Diff", 
  "USD_Return", "OIL_Return", "CNY_Return", "HACK_Return"
)

data_weekly <- data.frame(date = index(data_weekly),
                          coredata(data_weekly)
)
df_merged <- merge(data_weekly, vix_df, by = "date")
str(df_merged)


df_merged <- na.omit(df_merged)

# --- ONLY POST-COVID ---
post_start <- as.Date("2020-09-01")
df_merged <- df_merged[df_merged$date >= post_start, ]


# ---------------------------------------------------------------
#  Add squares predictors
# ---------------------------------------------------------------
df_merged$REMX_Return_sq   <- df_merged$REMX_Return^2
df_merged$DGS10_Diff_sq    <- df_merged$DGS10_Diff^2
df_merged$T10YIE_Diff_sq   <- df_merged$T10YIE_Diff^2
df_merged$USD_Return_sq    <- df_merged$USD_Return^2
df_merged$OIL_Return_sq    <- df_merged$OIL_Return^2
df_merged$CNY_Return_sq    <- df_merged$CNY_Return^2
df_merged$HACK_Return_sq   <- df_merged$HACK_Return^2


model <- lm(
  SOXX_Return ~ REMX_Return + DGS10_Diff + T10YIE_Diff +
    USD_Return + OIL_Return + CNY_Return + HACK_Return +
    REMX_Return_sq + DGS10_Diff_sq + T10YIE_Diff_sq +
    USD_Return_sq + OIL_Return_sq + CNY_Return_sq + HACK_Return_sq +
    VIX_Regime,
  data = df_merged
)
cat("\n--- MODEL SUMMARY ---\n")
summary(model)

###############################################################################
# --- BEST SUBSET USING BIC (COMPATIBLE WITH FACTORS) ---
###############################################################################
###############################################################################
# --- Robust best-subset with factors: expand factors with model.matrix ------
###############################################################################



# 1) Build design matrix (includes dummy variables for VIX_Regime)
#    Build a formula containing all original predictors
full_formula <- as.formula("
  SOXX_Return ~ 
    REMX_Return + DGS10_Diff + T10YIE_Diff + USD_Return +
    OIL_Return + CNY_Return + HACK_Return +
    # términos al cuadrado
    REMX_Return_sq + DGS10_Diff_sq + T10YIE_Diff_sq +
    USD_Return_sq + OIL_Return_sq + CNY_Return_sq + HACK_Return_sq +
    # factor
    VIX_Regime
")

# model.matrix creates the X matrix (includes intercept by default)
mm_all <- model.matrix(full_formula, data = df_merged)

# Remove the intercept column 
mm_predictors <- mm_all[, colnames(mm_all) != "(Intercept)", drop = FALSE]

# Response variable
y <- df_merged$SOXX_Return

# 2) Run regsubsets on the expanded design matrix (x, y interface)
model_best_mm <- regsubsets(x = mm_predictors, y = y, nvmax = 10, method = "exhaustive")

best_summary_mm <- summary(model_best_mm)
print(best_summary_mm)
# 3) Select the best model according to BIC
best_bic_index_mm <- which.min(best_summary_mm$bic)
cat("Best model size (by BIC) on expanded matrix:", best_bic_index_mm, "\n")

# 4) Extract coefficient names selected (these DO exist in mm_predictors)
coef_selected <- coef(model_best_mm, best_bic_index_mm)
cat("\nCoef names from regsubsets (expanded):\n")
print(names(coef_selected))

selected_cols <- names(coef_selected)[names(coef_selected) != "(Intercept)"]
cat("\nSelected expanded predictor columns:\n")
print(selected_cols)

# 5) Build dataframe with the response and only the selected columns
df_final_mm <- data.frame(SOXX_Return = y, mm_predictors[, selected_cols, drop = FALSE])

# 6) Fit lm() model using only those selected columns
final_model_mm <- lm(SOXX_Return ~ ., data = df_final_mm)

cat("\n--- FINAL MODEL (fit on expanded columns) ---\n")
print(summary(final_model_mm))

# 7) Additional metrics
cat("\nAIC:", AIC(final_model_mm), "\n")
cat("BIC:", BIC(final_model_mm), "\n")


###############################################################################
# 1) Scatterplot: Real vs Fitted
###############################################################################

df_plot <- data.frame(
  real = df_final_mm$SOXX_Return,
  fitted = fitted(final_model_mm)
)


plot_real_vs_fitted <- ggplot(df_plot, aes(x = real, y = fitted)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Real vs Fitted SOXX Weekly Returns",
    x = "Real SOXX Returns",
    y = "Fitted (Predicted) Returns"
  ) +
  theme_minimal(base_size = 14)

print(plot_real_vs_fitted)

################################################################################
#Assumptions tests
################################################################################
# ---------------------------
#  Residual diagnostics: normality tests
#  QQ-plot + Shapiro-Wilk (with fallback for n > 5000)
# ---------------------------

# Extract residuals
resid_final <- residuals(final_model_mm)
n_res <- length(resid_final)
cat("Number of residuals:", n_res, "\n")

# QQ-plot using ggplot2
library(ggplot2)
qq_df <- data.frame(resid = resid_final)

qq_plot <- ggplot(qq_df, aes(sample = resid)) +
  stat_qq(size = 1.5) +
  stat_qq_line(color = "steelblue", size = 0.8) +
  labs(
    title = "QQ-plot of residuals",
    subtitle = paste0("n = ", n_res),
    x = "Theoretical quantiles",
    y = "Sample quantiles (residuals)"
  ) +
  theme_minimal(base_size = 13)

print(qq_plot)

cat("\n--- Shapiro-Wilk test ---\n")
print(shapiro.test(resid_final))

# Optional: histogram with density overlay for visual inspection
hist_plot <- ggplot(qq_df, aes(x = resid)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "grey90", color = "black") +
  geom_density(size = 0.9) +
  labs(
    title = "Histogram + density of residuals",
    x = "Residuals",
    y = "Density"
  ) +
  theme_minimal(base_size = 13)

print(hist_plot)

# ---------------------------
#  Heteroskedasticity test: Breusch-Pagan (lmtest)
# ---------------------------

cat("\n--- Breusch-Pagan Test for Heteroskedasticity ---\n")
bp_test <- bptest(final_model_mm)
print(bp_test)

# Interpretation note
cat("\nNote: A low p-value (< 0.05) indicates evidence of heteroskedasticity.\n")

# ---------------------------
#  Multicollinearity diagnostics: Variance Inflation Factors (VIF)
# ---------------------------

library(car)

cat("\n--- Variance Inflation Factors (VIF) ---\n")
vif_values <- vif(final_model_mm)
print(vif_values)

# Rule-of-thumb interpretation
cat("\nInterpretation guidelines:\n")
cat("VIF = 1      : No multicollinearity\n")
cat("VIF < 5      : Acceptable\n")
cat("VIF >= 5     : Potential multicollinearity\n")
cat("VIF >= 10    : Serious multicollinearity concern\n")

# ---------------------------
#  Linearity diagnostics: Residuals vs Fitted plot
# ---------------------------

resid_df <- data.frame(
  fitted = fitted(final_model_mm),
  residuals = residuals(final_model_mm)
)

linearity_plot <- ggplot(resid_df, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", size = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue", size = 0.9) +
  labs(
    title = "Residuals vs Fitted Values — Linearity Check",
    x = "Fitted values",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 13)

print(linearity_plot)

cat("\nNote: A random cloud of points around zero suggests linearity.\n")
cat("Curved patterns or structure indicate potential non-linearity.\n")


# ---------------------------
#  Autocorrelation diagnostics: Breusch-Godfrey Test
# ---------------------------


cat("\n--- Breusch-Godfrey Test for Autocorrelation ---\n")

# Test for autocorrelation up to lag = 1 (you can increase this if desired)
bg_test <- bgtest(final_model_mm, order = 1)
print(bg_test)

cat("\nNote: A low p-value (< 0.05) indicates evidence of autocorrelation.\n")

# ACF of residuals (multiple lags)
cat("\n--- Autocorrelation Function (ACF) of residuals ---\n")
acf(resid_final, main = "ACF of Residuals", lag.max = 20)


###############################################################################
#More graphs and relevant checks
###############################################################################

###############################################################################
# 1) Scatterplot: Real vs Fitted
###############################################################################

df_plot <- data.frame(
  real = df_final_mm$SOXX_Return,
  fitted = fitted(final_model_mm)
)

library(ggplot2)

plot_real_vs_fitted <- ggplot(df_plot, aes(x = real, y = fitted)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Real vs Fitted SOXX Weekly Returns",
    x = "Real SOXX Returns",
    y = "Fitted (Predicted) Returns"
  ) +
  theme_minimal(base_size = 14)

print(plot_real_vs_fitted)

###############################################################################
# 2) Time series: Real vs Fitted + Residuals
###############################################################################

df_ts <- data.frame(
  date = df_merged$date,
  real = df_final_mm$SOXX_Return,
  fitted = fitted(final_model_mm),
  resid = residuals(final_model_mm)
)

# Panel 1: Real vs Fitted
p1 <- ggplot(df_ts, aes(x = date)) +
  geom_line(aes(y = real, color = "Real")) +
  geom_line(aes(y = fitted, color = "Fitted"), linewidth = 0.9) +
  labs(
    title = "Real vs Fitted SOXX Weekly Returns (Time Series)",
    y = "Returns",
    x = ""
  ) +
  scale_color_manual(values = c("Real" = "black", "Fitted" = "steelblue")) +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank())

# Panel 2: Residuals
p2 <- ggplot(df_ts, aes(x = date, y = resid)) +
  geom_line(color = "darkred") +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.7) +
  labs(
    title = "Residuals Over Time",
    y = "Residuals",
    x = "Date"
  ) +
  theme_minimal(base_size = 14)

# Combine plots
library(gridExtra)
grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))


##############################################################################
# Investigate effects of MCAR
##############################################################################

run_MCAR_model <- function(data, missing_pct, seed=48) {
  # Set seed for reproducibility
  set.seed(seed)
  df_temp <- data
  
  
  n <- nrow(df_temp)
  # Introduce MCAR missingness in REMX_Return
  # Randomly select 'missing_pct' proportion of rows and set values to NA
  df_temp$REMX_Return[sample(1:n, size = missing_pct * n)] <- NA
  # Introduce MCAR missingness in HACK_Return
  df_temp$HACK_Return[sample(1:n, size = missing_pct * n)] <- NA
  # Print summary of how much missingness was applied
  print(paste0("Missingness applied: ", missing_pct * 100, "%"))
  
  # Fit model on data after removing rows with missing values
  model <- lm(SOXX_Return ~ REMX_Return + HACK_Return,
              data = df_temp, na.action = na.omit)
  
  return(summary(model))
}

run_MCAR_model(df_final_mm, 0.10)
run_MCAR_model(df_final_mm, 0.20)
run_MCAR_model(df_final_mm, 0.30)
run_MCAR_model(df_final_mm, 0.50)
run_MCAR_model(df_final_mm, 0.60)


##############################################################################
# Investigate effects of MNAR
##############################################################################

df_MNAR <- df_final_mm
ret_soxx  <- df_final_mm$SOXX_Return
# Convert the magnitude of SOXX returns into probabilities of missingness
# Larger |SOXX_Return| → higher probability of becoming missing
# Resulting probabilities range from 5% to 45%
prob_miss <- scales::rescale(abs(ret_soxx), to = c(0.05, 0.45))    # magnitude → missing

# Apply MNAR missingness:
# A value becomes NA if a random draw is less than its assigned probability
df_MNAR$SOXX_Return[runif(nrow(df_MNAR)) < prob_miss] <- NA

# Fit regression model using rows without missing values
model_MNAR <- lm(SOXX_Return ~ REMX_Return + HACK_Return,
                 data = df_MNAR, na.action = na.omit)

summary(model_MNAR)


