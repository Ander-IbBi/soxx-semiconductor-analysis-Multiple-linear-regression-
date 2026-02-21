###############################################################################
# -----------------------------
#  ⚡ DATA STRUCTURE & EDA
# -----------------------------
###############################################################################
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(GGally)
library(reshape2)
library(quantmod)
library(lubridate)
library(zoo)
library(dplyr)
library(lmtest)
library(sandwich)
library(gridExtra)
library(car)
library(leaps)

rm(list = ls(all.names = TRUE))

###############################################################################
# Download data
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
# Convert all tickers to weekly frequency 
###############################################################################


to_weekly_manual <- function(x, week_day = 5) {
  # week_day: 0=Sunday ... 5=Friday ... 6=Saturday. Default 5 => Friday.
  # 1) Extract the last numeric column (Close / Adjusted)
  core_vec <- as.numeric(coredata(x[, ncol(x), drop = TRUE]))
  dates    <- as.Date(index(x))
  
  # 2) Build data.frame and remove NA in value or date
  df <- data.frame(date = dates, value = core_vec)
  df <- df[!is.na(df$date) & !is.na(df$value), , drop = FALSE]
  
  if (nrow(df) == 0) {
    warning("Empty series after removing NA; returning empty xts")
    return(xts(order.by = as.Date(character(0))))
  }
  
  # 3) Compute 'week_end' = date of the corresponding week_day (e.g., Friday)
  #    For each date, find the next week_day >= date within the week.
  wday_num <- as.integer(format(df$date, "%w"))  # 0=Sun ... 6=Sat
  # distance in days to reach chosen week_day (mod 7)
  delta <- ((week_day - wday_num) %% 7)
  df$week_end <- df$date + delta
  df$week_end <- as.Date(df$week_end)
  
  # 4) Group by week_end and take the LAST observation (latest in week)
  df_week <- df %>%
    group_by(week_end) %>%
    slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(week_end)
  
  # 5) Build xts with index = week_end and values = value
  xts_week <- xts(df_week$value, order.by = df_week$week_end)
  colnames(xts_week) <- colnames(x)[ncol(x)]  # original column name
  
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

# FRED series (univariate): same function works
dgs10_w    <- to_weekly_manual(DGS10)
t10yie_w   <- to_weekly_manual(T10YIE)

# Quick check
cat("Rows per series (weekly):\n")
cat("SOXX:", NROW(soxx_w), "  SPY:", NROW(spy_w), "  VXN:", NROW(vxn_w), "\n")
cat("REMX:", NROW(remx_w), "  NYICDX:", NROW(usdindex_w), "  OIL:", NROW(oil_w), "\n")
cat("CNY:", NROW(cny_w), "  HACK:", NROW(hack_w), "\n")
cat("DGS10:", NROW(dgs10_w), "  T10YIE:", NROW(t10yie_w), "\n")



vix_df <- data.frame(
  date = index(vxn_w),
  coredata(vxn_w)
)

vix_df <- vix_df %>%
  mutate(VIX_Regime = case_when(
    vxn_w <= 18 ~ "very low",
    vxn_w <= 24 ~ "moderate",
    TRUE      ~ "high"
  )) %>%
  mutate(VIX_Regime = factor(VIX_Regime, levels = c("low", "medium", "high")))

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
dgs10_dlog  <- diff(log(dgs10_w))   # Weekly % change in 10-year yield
t10yie_dlog <- diff(log(t10yie_w))  # Weekly % change in inflation expectations
usdindex_ret <- diff(log(usdindex_w)) # Weekly USD Index log-return
oil_ret <- diff(log(oil_w))         # WTI weekly log-returns
cny_ret <- diff(log(cny_w))         # CNY/USD weekly log-returns
hack_ret <- diff(log(hack_w)) 

###############################################################################
# Build combined weekly dataset
###############################################################################
# vix_regime <- xts(vix_df[["VIX_Regime"]], order.by = index(spy_ret))

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
#  Add squared predictors
# ---------------------------------------------------------------
df_merged$REMX_Return_sq   <- df_merged$REMX_Return^2
df_merged$DGS10_Diff_sq    <- df_merged$DGS10_Diff^2
df_merged$T10YIE_Diff_sq   <- df_merged$T10YIE_Diff^2
df_merged$USD_Return_sq    <- df_merged$USD_Return^2
df_merged$OIL_Return_sq    <- df_merged$OIL_Return^2
df_merged$CNY_Return_sq    <- df_merged$CNY_Return^2
df_merged$HACK_Return_sq   <- df_merged$HACK_Return^2
###############################################################################
# -----------------------------
#   RAW DATA STRUCTURE CHECK
# -----------------------------
# (Before any transformations)
###############################################################################

cat("\n\n========== RAW DATA CHECK ==========\n")

# Function to summarize NA and rows in raw xts objects
raw_list <- list(
  SOXX = SOXX, SPY = SPY, VXN = VXN, REMX = REMX, NYICDX = NYICDX,
  OIL = `CL=F`, CNY = `CNY=X`, HACK = HACK, 
  DGS10 = DGS10, T10YIE = T10YIE
)

for (nm in names(raw_list)) {
  x <- raw_list[[nm]]
  cat("\nSymbol:", nm,
      "\n Rows:", NROW(x),
      "\n Columns:", NCOL(x),
      "\n NA count:", sum(is.na(x)),
      "\n-------------------------------\n")
}


###############################################################################
#  🧱 BASIC DATA STRUCTURE AFTER MERGING (post-COVID only)
###############################################################################

cat("\n========== STRUCTURE AFTER MERGE ==========\n")
cat("Dimensions:", dim(df_merged)[1], "rows x", dim(df_merged)[2], "columns\n")
summary(df_merged)


###############################################################################
# -----------------------------
#   PLOT 1 & 2 SIDE BY SIDE
#   SOXX weekly price + returns
# -----------------------------
###############################################################################
library(ggplot2)
library(gridExtra)

soxx_price_df <- data.frame(
  date = index(soxx_w),
  price = as.numeric(coredata(soxx_w))
)

# (1) SOXX weekly price
p1 <- ggplot(soxx_price_df, aes(date, price)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  theme_minimal() +
  labs(title = "SOXX Weekly Price", x = "Date", y = "Price")

# (2) SOXX weekly log-returns
p2 <- ggplot(df_merged, aes(date, SOXX_Return)) +
  geom_line(color = "darkred", linewidth = 0.8) +
  theme_minimal() +
  labs(title = "SOXX Weekly Log-Returns (Post-COVID)",
       x = "Date", y = "Return")

# ------------------------------
# Stacked vertically
# ------------------------------
grid.arrange(p1, p2, ncol = 1)


###############################################################################
# 📌 2. Descriptive Statistics Pre vs Post COVID
###############################################################################

soxx_df_full <- data_weekly %>%
  select(date, SOXX_Return) %>%
  mutate(
    Period = ifelse(
      date < as.Date("2020-03-01"),
      "Pre-COVID",
      "Post-COVID"
    ),
    Period = factor(Period, levels = c("Pre-COVID", "Post-COVID"))
  )


ggplot(soxx_df_full, aes(x = Period, y = SOXX_Return, fill = Period)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "SOXX Weekly Log-Returns: Pre vs Post COVID",
    y = "Log-Returns", x = ""
  ) +
  theme(legend.position = "none")




###############################################################################
# Heavy-Tail Assessment: JB Test
###############################################################################
library(moments)

soxx_returns_vec <- as.numeric(df_merged$SOXX_Return)

cat("---- JARQUE-BERA TEST ----\n")
print(jarque.test(soxx_returns_vec))
###############################################################################

###############################################################################
# 📌 Rolling Volatility — SOXX (15-week window) with clean vertical regime lines
###############################################################################
library(zoo)
library(dplyr)

# Convert SOXX to full dataframe
df_soxx_full <- data.frame(
  date = index(soxx_w),
  price = as.numeric(coredata(soxx_w))
)

# Compute rolling volatility (15-week window)
window_size <- 15

df_soxx_full <- df_soxx_full %>%
  mutate(
    ret = c(NA, diff(log(price))),                     # log returns
    vol_15 = zoo::rollapply(ret, width = window_size,
                            FUN = sd, fill = NA, align = "right") * sqrt(52) # annualized
  )
vline_dates <- as.Date(c("2020-03-01", "2020-09-01"))

p_vol15_full <- ggplot(df_soxx_full, aes(date, vol_15)) +
  geom_line(color = "darkblue", linewidth = 1) +
  
  # --- Clean vertical regime lines (no labels) ---
  geom_vline(xintercept = vline_dates,
             color = "red", linetype = "dashed", linewidth = 0.9) +
  
  theme_minimal() +
  labs(
    title = "SOXX — Rolling Annualized Volatility (15-week window)",
    subtitle = "Computed using the full weekly SOXX history",
    x = "Date",
    y = "Annualized Volatility"
  ) +
  theme(plot.title = element_text(face = "bold"))

p_vol15_full


###############################################################################
# 📌 1. Stationarity Tests for Price Levels and Log-Returns
###############################################################################
library(tseries)
library(urca)

# --- Clean & prepare vectors (drop NAs and force numeric) ---
price_vec <- na.omit(as.numeric(coredata(soxx_w)))   # SOXX price level (numeric, no NA)
ret_vec   <- na.omit(as.numeric(coredata(soxx_ret))) # SOXX weekly log-returns (numeric, no NA)

cat("Number of non-NA observations:\n")
cat(" SOXX prices:", length(price_vec), "\n")
cat(" SOXX returns:", length(ret_vec), "\n\n")

# --- ADF tests (augmented Dickey-Fuller) ---
# Note: choose k (lag) appropriate for sample size; k = 4 is common for weekly data but you may adjust.
cat("---- ADF TEST: SOXX Price Level ----\n")
print(adf.test(price_vec, k = 4))

cat("\n---- ADF TEST: SOXX Log-Returns ----\n")
print(adf.test(ret_vec, k = 4))

# --- KPSS tests (stationarity null) ---
cat("\n---- KPSS TEST: SOXX Price Level ----\n")
print(kpss.test(price_vec))

cat("\n---- KPSS TEST: SOXX Log-Returns ----\n")
print(kpss.test(ret_vec))

###############################################################################
# 📊 RAW POST-COVID SERIES
#    Each variable plotted individually and in a single figure (last figure normalized)
###############################################################################

# -------------------------
#  POST-COVID RAW LEVELS
# -------------------------

df_levels <- merge(
  soxx_w, remx_w, dgs10_w, t10yie_w, usdindex_w,
  oil_w, cny_w, hack_w, vxn_w,
  join = "inner"
)

colnames(df_levels) <- c(
  "SOXX", "REMX", "DGS10", "T10YIE",
  "USD", "OIL", "CNY", "HACK", "VXN"
)

# Convert to dataframe
df_levels <- data.frame(date = index(df_levels),
                        coredata(df_levels))

# Keep only post-COVID
df_levels <- df_levels[df_levels$date >= as.Date("2020-09-01"), ]


df_spaghetti_levels <- df_levels %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  mutate(value_norm = 100 * value / first(value)) %>%
  ungroup()


p_facet_levels <- ggplot(df_spaghetti_levels, aes(x = date, y = value)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  theme_minimal() +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  labs(title = "Post-COVID Weekly Series",
       x = "Date", y = "Value") +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 8)
  )

p_facet_levels




# ------------------------------
# NORMALIZE EACH SERIES TO 100
# ------------------------------
df_levels <- merge(
  soxx_w, remx_w, dgs10_w, t10yie_w, usdindex_w,
  oil_w, cny_w, hack_w, vxn_w,
  join = "inner"
)

colnames(df_levels) <- c(
  "SOXX", "REMX", "DGS10", "T10YIE",
  "USD", "OIL", "CNY", "HACK", "VXN"
)

df_levels <- data.frame(
  date = index(df_levels),
  coredata(df_levels)
)

# Keep only post-COVID
df_levels <- df_levels[df_levels$date >= as.Date("2020-09-01"), ]



# --- Create tidy dataframe + normalize ---
df_spaghetti_levels <- df_levels %>%
  pivot_longer(
    cols = -date,
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  mutate(value_norm = 100 * value / first(value)) %>%
  ungroup()

# --- Normalized spaghetti plot ---
p_spaghetti_levels <- ggplot(df_spaghetti_levels,
                             aes(x = date, y = value_norm, color = variable)) +
  geom_line(alpha = 0.7, linewidth = 0.75) +
  theme_minimal() +
  labs(
    title = "Post-COVID Weekly Series — Normalized to 100",
    x = "Date",
    y = "Index (100 = first value)",
    color = "Variable"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

p_spaghetti_levels


###############################################################################
# 📊 FACET GRID — RAW POST-COVID SERIES
#    Each variable plotted individually in a single multi-panel figure
###############################################################################

df_spaghetti_raw <- df_merged %>%
  select(date, SOXX_Return, REMX_Return, DGS10_Diff, T10YIE_Diff,
         USD_Return, OIL_Return, CNY_Return, HACK_Return) %>%
  pivot_longer(cols = -date,
               names_to = "variable",
               values_to = "value")

p_facet <- ggplot(df_spaghetti_raw, aes(x = date, y = value)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  theme_minimal() +
  facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  labs(title = "Post-COVID Weekly Series — Individual Panels",
       x = "Date",
       y = "Value") +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 8)
  )

p_facet



###############################################################################
# 📌 4. Distribution Plots (Histogram + KDE)
###############################################################################

vars_to_plot <- c("SOXX_Return", "REMX_Return", "DGS10_Diff",
                  "T10YIE_Diff", "USD_Return", "OIL_Return",
                  "CNY_Return", "HACK_Return")

plot_list <- list()

for (v in vars_to_plot) {
  p <- ggplot(df_merged, aes_string(x = v)) +
    geom_histogram(aes(y = ..density..),
                   fill = "steelblue", alpha = 0.6) +
    geom_density(color = "darkred", linewidth = 1.1) +
    theme_minimal() +
    labs(title = paste("Distribution of", v),
         x = v, y = "Density")
  plot_list[[v]] <- p
}

# Display all in one figure (3 rows × 3 columns)
grid.arrange(grobs = plot_list, ncol = 3)


###############################################################################
# -----------------------------
#   VIX REGIME PLOT
# -----------------------------
###############################################################################

p_vix <- ggplot(df_merged, aes(date, VIX_Regime)) +
  geom_point(aes(color = VIX_Regime), size = 2, alpha = 0.8) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "VIX Regimes Over Time (Post-COVID)",
       x = "Date", y = "Regime")

p_vix





















