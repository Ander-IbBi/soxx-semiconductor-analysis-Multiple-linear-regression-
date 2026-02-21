###############################################################################
# --- SOXX Variance Comparison (Pre/Post Covid) ---
###############################################################################
library(quantmod)
library(lubridate)
library(zoo)
library(dplyr)
library(car)
library(FinTS)
library(ggplot2)  

rm(list = ls(all.names = TRUE))
###############################################################################
# 1️⃣ Download data
###############################################################################

getSymbols("SOXX", from = Sys.Date() - years(10), src = "yahoo")


###############################################################################
# 2️⃣ Convert to weekly frequency
###############################################################################

to_weekly <- function(x) apply.weekly(x, last)

soxx_w <- to_weekly(Ad(SOXX))

###############################################################################
# 3️⃣ Compute weekly log-returns
###############################################################################

soxx_ret <- diff(log(soxx_w))

###############################################################################
# 4️⃣ Build dataset and split pre/post Covid
###############################################################################

data_weekly <- na.omit(soxx_ret)
colnames(data_weekly) <- "SOXX_Return"

pre_end    <- as_date("2020-01-31")
post_start <- as_date("2020-09-01")

pre_df  <- data_weekly[index(data_weekly) <= pre_end]
post_df <- data_weekly[index(data_weekly) >= post_start]

cat("Pre period:", min(index(pre_df)), "to", max(index(pre_df)), "\n")
cat("Post period:", min(index(post_df)), "to", max(index(post_df)), "\n")

###############################################################################
# 5️⃣ Variance values
###############################################################################

var_pre  <- var(pre_df$SOXX_Return)
var_post <- var(post_df$SOXX_Return)

cat("Var(pre)  =", var_pre,  "\n")
cat("Var(post) =", var_post, "\n")


###############################################################################
# 6️⃣Diferent checks
###############################################################################
# ACF graphs for autocorrelation
par(mfrow = c(1,2))

acf(pre_df$SOXX_Return,
    main = "ACF: SOXX Weekly Log-Returns\nPre-COVID Period",
    cex.main = 1.25)

acf(post_df$SOXX_Return,
    main = "ACF: SOXX Weekly Log-Returns\nPost-COVID Period",
    cex.main = 1.25)

# Reset layout afterwards
par(mfrow = c(1,1))


# Ljung test for autocorrelation
Box.test(pre_df$SOXX_Return, lag = 10, type = "Ljung")
Box.test(post_df$SOXX_Return, lag = 10, type = "Ljung")


#ARCH test for clustering
ArchTest(pre_df$SOXX_Return)
ArchTest(post_df$SOXX_Return)


#Graph

df_plot <- data.frame(
  Date   = c(index(pre_df), index(post_df)),
  Return = c(as.numeric(pre_df$SOXX_Return),
             as.numeric(post_df$SOXX_Return)),
  Period = c(rep("Pre",  length(pre_df$SOXX_Return)),
             rep("Post", length(post_df$SOXX_Return)))
)

ggplot(df_plot, aes(x = Date, y = Return, color = Period)) +
  geom_point(alpha = 0.8) +
  labs(
    title = "SOXX Weekly Log-Returns: Pre vs Post COVID",
    x = "Date",
    y = "Log-Return"
  ) +
  scale_color_manual(values = c("Pre" = "darkgreen", "Post" = "red")) +
  theme_minimal()
###############################################################################
#7️⃣ Brown–Forsythe test (robust variance equality test)
###############################################################################

df_vol <- data.frame(
  ret = c(as.numeric(pre_df$SOXX_Return), as.numeric(post_df$SOXX_Return)),
  Period = c(
    rep("Pre",  length(pre_df$SOXX_Return)),
    rep("Post", length(post_df$SOXX_Return))
  )
)

cat("\n--- Brown–Forsythe Test for Equality of Variances ---\n")
print(leveneTest(ret ~ Period, data = df_vol, center = median))

################################################################################
# Investigate effects of MCAR 
################################################################################
mcar_test <- function(df, missing_pct = 0.20, seed=48) {
  set.seed(seed)
  # Number of rows to keep
  keep_n <- round((1 - missing_pct) * nrow(df))
  
  # Random MCAR subsample
  df_mcar <- df[sample(1:nrow(df), size = keep_n), ]
  
  print(paste0("Missingness applied: ", missing_pct * 100, "%"))
  # Brown–Forsythe (Levene) test
  result <- leveneTest(ret ~ Period, df_mcar, center = median)
  
  return(result)
}

mcar_test(df_vol, 0.10)
mcar_test(df_vol, 0.20)
mcar_test(df_vol, 0.30)
mcar_test(df_vol, 0.50)
mcar_test(df_vol, 0.60)

#Even as you randomly remove up to 60% of the data under MCAR, the Brown–Forsythe test remains strongly significant.
#This means: 
# 1. The volatility difference between pre- and post-COVID SOXX returns is real and robust.
# 2. MCAR missingness does not bias or distort your conclusion.


################################################################################
# Investigate effects of MNAR 
################################################################################
# MNAR situations for SOXX Returns may arise during high volatility, market stress,
# and crisis, and result in missing extremely positive and negative returns 


mnar_test <- function(df, missing_strength = 3) {
    
  ret <- abs(df$ret)
  
  # Scale absolute returns to 0–1
  scaled_ret <- scales::rescale(ret, to = c(0, 1))
  
  # MNAR probability increasing with return size
  prob_miss <- scaled_ret^missing_strength
  
  # Draw missingness indicators
  is_missing <- rbinom(nrow(df), size = 1, prob = prob_miss)
  
  df_mnar <- df
  df_mnar$ret[is_missing == 1] <- NA
  
  # Drop missing
  df_mnar <- na.omit(df_mnar)
  
  # Brown–Forsythe variance test
  result <- leveneTest(ret ~ Period, df_mnar, center = median)
  
  cat("\nMissing rate:", mean(is_missing), "\n")
  return(result)
}

mnar_test(df_vol, missing_strength = 0.2)
mnar_test(df_vol, missing_strength = 0.3)
mnar_test(df_vol, missing_strength = 0.5)
mnar_test(df_vol, missing_strength = 0.8)
mnar_test(df_vol, missing_strength = 1.2)
mnar_test(df_vol, missing_strength = 1.5)








