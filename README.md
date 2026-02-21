# SOXX Semiconductor ETF — Statistical Analysis of COVID-19 Structural Break and Macroeconomic Drivers

---

## Overview

This project investigates the statistical behavior of the **iShares Semiconductor ETF (SOXX)** across two dimensions:

1. **Did COVID-19 cause a structural shift in SOXX return volatility?**  
2. **Which macroeconomic and sector-specific variables significantly explain post-COVID weekly SOXX returns?**

Rather than building a prediction engine, the study is explicitly **inferential**: the goal is to identify and quantify how external economic forces impact semiconductor equity performance. The full methodology and results are documented in the accompanying project report (`Group07_Report_AMS-572.pdf`).

---

## Data

All series are collected at **weekly frequency** from two sources:

| Ticker | Description | Source |
|--------|-------------|--------|
| SOXX | iShares Semiconductor ETF | Yahoo Finance |
| VXN | CBOE Nasdaq Volatility Index | Yahoo Finance |
| REMX | Rare Earth/Strategic Metals ETF | Yahoo Finance |
| NYICDX | ICE U.S. Dollar Index | Yahoo Finance |
| CL=F | Crude Oil WTI Futures | Yahoo Finance |
| CNY=X | Chinese Yuan / USD Exchange Rate | Yahoo Finance |
| HACK | Cybersecurity ETF | Yahoo Finance |
| DGS10 | 10-Year U.S. Treasury Yield | FRED |
| T10YIE | 10-Year Breakeven Inflation Rate | FRED |

All price series are converted to **log-returns** prior to analysis. Stationarity is confirmed via ADF and KPSS tests. The VXN index is discretized into five **volatility regime categories** (Very Low / Normal / Elevated / High / Extreme) to avoid imposing a linear relationship between implied volatility and returns.

The COVID transition window (February–August 2020) is excluded from both hypotheses to avoid contamination by the acute shock period.

---

## Repository Structure

```
soxx-semiconductor-analysis/
│
├── Brown_Forsythe_Hypothesis1_Group07.R       # H1: Variance comparison (Brown-Forsythe test)
├── Multiple_Linear_Regression_Hypothesis2_Group07.R  # H2: Multiple linear regression + model selection
├── Data_exploration_and_graphs.R              # EDA, stationarity tests, rolling volatility, plots
│
└── Group07_Report_AMS-572.pdf                 # Full project report
```

---

## Hypothesis 1 — Variance Comparison: Pre vs. Post COVID

**Question:** Did the COVID-19 shock cause a statistically significant increase in the volatility of weekly SOXX returns?

### Methodological decisions

The classical **F-test for equality of variances** requires normally distributed samples — an assumption that financial return series routinely violate. A Shapiro-Wilk test on weekly SOXX log-returns confirmed non-normality (p = 1.31 × 10⁻⁶), and QQ-plots showed clear fat-tail behavior.

The **Brown-Forsythe test** was selected instead. It is a robust alternative that:
- Uses **median-based absolute deviations** rather than mean-based, making it resistant to heavy tails and skewness
- Requires only **independence of observations**, not normality or homoskedasticity
- Is well-suited to financial return series

Independence was verified via:
- **Ljung-Box test** (p = 0.36 pre-COVID, p = 0.95 post-COVID) — no serial correlation
- **ARCH LM test** (p = 0.31 pre-COVID, p = 0.82 post-COVID) — no volatility clustering at weekly frequency

### Result

```
H₀: σ²_pre = σ²_post   vs.   H₁: σ²_pre ≠ σ²_post

Brown-Forsythe F = 21.479,  p-value = 4.594 × 10⁻⁶  →  Reject H₀
```

There is overwhelming statistical evidence of a structural shift in SOXX return volatility after COVID-19.

### Robustness to missing data

The conclusion was stress-tested under two missing data mechanisms:

- **MCAR (10%–60% random deletion):** Test remained significant at all levels, confirming that the variance shift is not an artifact of sample size.
- **MNAR (selective removal of extreme returns):** Significance persisted under moderate selective deletion. It was lost only when >69% of observations were removed — an extreme and economically unrealistic scenario that would eliminate essentially all tail behavior from the data.

---

## Hypothesis 2 — Multiple Linear Regression: Drivers of Post-COVID SOXX Returns

**Question:** Which macroeconomic and sector-specific variables have a statistically significant effect on weekly SOXX returns in the post-COVID period?

### Predictor selection rationale

Variables were selected based on **economic motivation**, not mechanical screening:

- **REMX** — rare earth input costs; semiconductors depend on these materials
- **DGS10 / T10YIE** — interest rate and inflation expectations; affect equity discount rates
- **USD Index** — stronger dollar tightens conditions for multinational exporters
- **WTI Crude Oil** — energy input costs across the supply chain
- **CNY/USD** — proxy for China-related supply chain and geopolitical pressures
- **HACK** — broader technology infrastructure performance (cybersecurity sector)
- **VXN Regime** — categorical market risk sentiment variable (5 regimes)

Broad market indices (S&P 500, NASDAQ) were **deliberately excluded** to avoid multicollinearity and to isolate sector-specific drivers rather than capturing equity beta, which is economically trivial.

Quadratic terms were included to allow for nonlinear sensitivities in the full model.

### Model selection

An **exhaustive best-subset search** was conducted, selecting the model that minimizes **BIC** — penalizing unnecessary complexity while preserving explanatory power.

The BIC-optimal model is parsimonious:

```
SOXX_Return = β₀ + β₁·REMX_Return + β₂·HACK_Return + ε
```

| Term | Estimate | Std. Error | p-value |
|------|----------|------------|---------|
| Intercept | 0.00187 | 0.00190 | 0.327 |
| REMX_Return | 0.19606 | 0.03899 | 9.36 × 10⁻⁷ *** |
| HACK_Return | 0.82773 | 0.06490 | < 2 × 10⁻¹⁶ *** |

**Adjusted R² = 0.544** — comparable to the full 18-predictor specification, confirming that macroeconomic variables add little explanatory power once sector-specific drivers are included.

### Assumptions diagnostics

| Assumption | Test / Method | Result |
|---|---|---|
| Normality of residuals | Shapiro-Wilk | p = 0.403 — not rejected |
| Independence of errors | Breusch-Godfrey | p = 0.348 — not rejected |
| Linearity | Residuals vs. fitted plot | No curvature detected |
| Multicollinearity | VIF | VIF(REMX) = VIF(HACK) = 1.25 — no issue |
| Homoskedasticity | Breusch-Pagan | p = 0.016 — mild heteroskedasticity detected |

Mild heteroskedasticity is present and acknowledged. It is typical of financial return data and does not bias coefficient estimates, though it may affect standard error precision. The coefficient estimates remain reliable.

### Robustness to missing data

- **MCAR:** Coefficients for REMX and HACK remained statistically significant and directionally stable across all deletion levels (10%–60%), consistent with MCAR's theoretically unbiased nature.
- **MNAR (selective removal of extreme returns):** Introduced systematic distortions — REMX coefficient inflated ~9%, HACK deflated ~7–8%. Residual standard error decreased artificially, creating a false impression of improved fit. This highlights that **even limited MNAR missingness can meaningfully bias inference** in financial regression models.

---

## Key Findings

- COVID-19 generated a **statistically significant and persistent structural break** in SOXX return volatility (Brown-Forsythe F = 21.48, p < 10⁻⁵).
- In the post-COVID period, weekly SOXX returns are primarily explained by **rare earth metals performance (REMX)** and **cybersecurity sector performance (HACK)**, capturing input-cost exposure and technology ecosystem co-movement respectively.
- Macroeconomic variables (Treasury yields, USD, oil, inflation expectations, CNY/USD) and **VXN volatility regimes** did not add significant explanatory power once sector fundamentals were included.
- Results are robust to random missingness (MCAR) and sensitive to selective missingness (MNAR), consistent with theoretical expectations.

---

## Dependencies

All analysis is performed in **R**. Key packages used:

```r
quantmod      # Data retrieval from Yahoo Finance / FRED
tseries       # ADF, KPSS stationarity tests
FinTS         # ARCH LM test
car           # Brown-Forsythe / Levene test
leaps         # Exhaustive best-subset regression
lmtest        # Breusch-Godfrey, Breusch-Pagan tests
ggplot2       # Visualization
zoo / xts     # Time series handling
```

---

## Notes

This project was completed as a graduate course assignment in applied statistics. The models are designed for **inferential purposes** — identifying statistically significant economic relationships — and are not intended as trading signals or forecasting tools.
