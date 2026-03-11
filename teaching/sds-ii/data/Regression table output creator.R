library(modelsummary)
library(tibble)

# ---------------------------------------- Fake model creation function
make_fake_model <- function(
    term,
    estimate,
    std.error,
    response = NULL,    
    r.squared = NA,
    adj.r.squared = NA,
    nobs = NA,
    AIC = NA,
    BIC = NA,
    level = 0.95,
    model_type = NA,
    outcome = NA
) {
  
  z <- qnorm(1 - (1 - level) / 2)
  
  ti <- tibble::tibble(
    term = term,
    estimate = estimate,
    std.error = std.error,
    statistic = estimate / std.error,
    p.value = 2 * (1 - pnorm(abs(statistic))),
    conf.low = estimate - z * std.error,
    conf.high = estimate + z * std.error,
    response = response
  )
  
  gl <- tibble::tibble(
    model_type = model_type,
    outcome = outcome,
    r.squared = r.squared,
    adj.r.squared = adj.r.squared,
    #AIC = AIC,
    #BIC = BIC,
    nobs = nobs
  )
  
  mod <- list(tidy = ti, glance = gl)
  class(mod) <- "modelsummary_list"
  
  mod
}

# ========================================
# Outcome: Hours worked (hours per month)
# ========================================
M1 <- make_fake_model(
  term = c("(Intercept)", "wage ($1,000)", "hh_size (number of people)"),
  estimate = c(32.1, 0.05, 11.5),
  std.error = c(4.2, 0.029, 1.8),
  r.squared = 0.23,
  adj.r.squared = 0.22,
  nobs = 500,
  model_type = "OLS",
  outcome = "Hours worked (per month)"
)

modelsummary(
  M1,
  estimate = "{estimate}{stars}",
  statistic = "std.error",
  notes = "Standard errors in parentheses, p-value significance: + p<0.1 * p<0.05; ** p<0.01; *** p<0.001"
)

# ========================================
# Simple regression table where the test is about correct units of predictor and outcome
# Outcome: Exam score (continuous)
# Predictor: 10 hours partying
# ========================================

M1b <- make_fake_model(
  term = c("(Intercept)", "hours_partying_10 (10 hrs/month)"),
  estimate = c(68.5, -8.0),
  std.error = c(12.5, 5.2),
  r.squared = 0.06,
  nobs = 130,
  model_type = "OLS",
  outcome = "Exam score"
)

# ========================================
# A simple non-linear model where a positive non-linear effect term makes sense. U-shape.
# Outcome: Individual freetime (hours per week)
# ========================================

M1c <- make_fake_model(
  term = c("(Intercept)", "age (yrs)", "age^2"),
  estimate = c(40.0, -0.35, 0.004),
  std.error = c(4.0, 0.17, 0.001),
  r.squared = 0.12,
  adj.r.squared = 0.11,
  nobs = 400,
  model_type = "OLS",
  outcome = "Free time (hours per week)"
)

#plot the above model's predicted values to show non-linear effect
library(ggplot2)
age_seq <- seq(12, 80, by = 1)
pred_data <- data.frame(
  age = age_seq
)
pred_data$predicted_freetime <- with(
  pred_data,
  40 - 0.35 * age + 0.004 * (age^2)
)
ggplot(pred_data, aes(x = age, y = predicted_freetime)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Predicted Freetime by Age",
    x = "Age (years)",
    y = "Predicted Freetime (hours per week)"
  ) +
  theme_minimal()

# ========================================
# Outcome: Happiness score (0–100)
# ========================================
M2 <- make_fake_model(
  term = c("(Intercept)", "female", "income ($1,000)"),
  estimate = c(50.0, 5.0,
           0.5),
  std.error = c(5.0, 2.0,
           0.1),
  r.squared = 0.30,
  adj.r.squared = 0.29,
  nobs = 1000,
  model_type = "OLS",
  outcome = "Happiness score (0–100)"
)

modelsummary(
  M2,
  estimate = "{estimate} {stars}",
  statistic = "std.error",
  stars = TRUE,
  notes = "Standard errors in parentheses"
)

# ========================================
# Outcome: Volunteering hours per month
# ========================================
M30 <- make_fake_model(
  term = c("(Intercept)", "college (vs high school)", "graduate (vs high school)"),
  estimate = c(9.2, 4.6, 8.9),
  std.error = c(2.7, 1.9, 3.0),
  r.squared = 0.15,
  adj.r.squared = 0.148,
  nobs = 600,
  model_type = "OLS",
  outcome = "Volunteering (hours per month)"
)

modelsummary(
  M30,
  estimate = "{estimate} {stars}",
  statistic = "std.error",
  notes = "Standard errors in parentheses",
  fmt = 2
)

M31 <- make_fake_model(
  term = c("(Intercept)", "college (vs high school)", "graduate (vs high school)",
           "female (yes=1)"),
  estimate = c(9.1, 4.2, 7.1, 4.2),
  std.error = c(2.8, 1.6, 3.6, 3.6),
  r.squared = 0.21,
  adj.r.squared = 0.151,
  nobs = 600,
  model_type = "OLS",
  outcome = "Volunteering (hours per month)"
)

M3 <- make_fake_model(
  term = c("(Intercept)", "college (vs high school)", "graduate (vs high school)",
           "female (yes=1)", "age (yrs)"),
  estimate = c(5.0, 2.5, 4.0, 3.0, 0.12),
  std.error = c(0.7, 0.9, 1.0, 2.6, 0.03),
  r.squared = 0.35,
  adj.r.squared = 0.28,
  nobs = 541,
  model_type = "OLS",
  outcome = "Volunteering (hours per month)"
)

modelsummary(
  list("Model 1" = M30, "Model 2" = M31, "Model 3" = M3),
  estimate = "{estimate} {stars}",
  statistic = "std.error",
  notes = "Standard errors in parentheses",
  fmt = 2
)


# ========================================
# Two models to test how adding controls changes coefficients
# Outcome: Hours worked (hours per month)
# Predictors: Education (categorical), Parental unemployment (binary)
# ========================================
M2a <- make_fake_model(
  term = c("(Intercept)", "college (vs high school)", "graduate (vs high school)"),
  estimate = c(32.0, 5.0, 10.0),
  std.error = c(4.0, 1.5, 2.0),
  r.squared = 0.20,
  adj.r.squared = 0.19,
  nobs = 500,
  model_type = "OLS",
  outcome = "Hours worked (per week)"
)

M2b <- make_fake_model(
  term = c("(Intercept)", "college (vs high school)", "graduate (vs high school)",
           "parent_unemployed_in_childhood (yes=1)"),
  estimate = c(36.0, 3.0, 6.0, -4.0),
  std.error = c(4.5, 1.9, 1.8, 1.5),
  r.squared = 0.25,
  adj.r.squared = 0.24,
  nobs = 500,
  model_type = "OLS",
  outcome = "Hours worked (per week)"
)

# ========================================
# Three models to test how adding controls changes coefficients
# Outcome: Physical activity (hours per week)
# Predictors: Age, married, number of children (categorical)
# ========================================

M41 <- make_fake_model(
  term = c("(Intercept)", "age (yrs)"),
  estimate = c(10.0, -0.15),
  std.error = c(1.5, 0.05),
  r.squared = 0.10,
  adj.r.squared = 0.09,
  nobs = 600,
  model_type = "OLS",
  outcome = "Phys. act. (h/w)"
)


M42 <- make_fake_model(
  term = c("(Intercept)", "age (yrs)", "married (1 = yes)"),
  estimate = c(12.0, -0.07, -2.5),
  std.error = c(1.8, 0.045, 1.2),
  r.squared = 0.15,
  adj.r.squared = 0.14,
  nobs = 600,
  model_type = "OLS",
  outcome = "Phys. act. (h/w)"
)

M43 <- make_fake_model(
  term = c("(Intercept)", "age (yrs)", "married (1 = yes)", "0 children (ref)", "1 child",
           "2+ children"),
  estimate = c(14.0, -0.04, -1.7, 0.0, -1.5, -5.0),
  std.error = c(2.0, 0.03, 1.2, 0.0, 0.6, 1.5),
  r.squared = 0.24,
  adj.r.squared = 0.17,
  nobs = 600,
  model_type = "OLS",
  outcome = "Phys. act. (h/w)"
)

models_list <- list(
  "Model 1" = M41,
  "Model 2" = M42,
  "Model 3" = M43
)

modelsummary(
  models_list,
  estimate = "{estimate} {stars}",
  statistic = "conf.int",
  notes = "Confidence intervals in parentheses",
  fmt = 2
)



# ========================================
# Outcome: Stress score (0–30)
# ========================================
M4 <- make_fake_model(
  term = c("(Intercept)", "working_hours_10 (10 hrs/month)",
           "remote_work (yes=1)", "working_hours_10:remote_work"),
  estimate = c(18.0, 2.0, -6.0, -1.5),
  std.error = c(5.5, 0.8, 2.7, 0.6),
  r.squared = 0.32,
  adj.r.squared = 0.26,
  nobs = 850,
  model_type = "OLS",
  outcome = "Stress score"
)

modelsummary(
  M4,
  estimate = "{estimate} {stars}",
  statistic = "std.error",
  notes = "Standard errors in parentheses"
)

# ========================================
# Model to show non-linear effects
# Outcome: Number of plants owned
# ========================================

M4b <- make_fake_model(
  term = c("(Intercept)", "age (yrs)", "age^2",
           "income_centered ($1k)"),
  estimate = c(-3.0, 0.5, -0.004, 0.02),
  std.error = c(0.5, 0.1, 0.002, 0.008),
  r.squared = 0.22,
  adj.r.squared = 0.20,
  nobs = 250,
  model_type = "OLS",
  outcome = "Number of plants in one's home"
)

#plot the above model's predicted values to show non-linear effect
library(ggplot2)
age_seq <- seq(18, 80, by = 1)
pred_data <- data.frame(
  age = age_seq,
  income_centered = 0  # Centered at mean income
)
pred_data$predicted_plants <- with(
  pred_data,
  -3 + 0.5 * age - 0.004 * (age^2) + 0.02 * income_centered
)
ggplot(pred_data, aes(x = age, y = predicted_plants)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Predicted Number of Plants Owned by Age",
    x = "Age (years)",
    y = "Predicted Number of Plants"
  ) +
  theme_minimal()

modelsummary(
  M4b,
  estimate = "{estimate} {stars}",
  statistic = "std.error",
  notes = "Standard errors in parentheses"
)

# ========================================
# Outcome: Life satisfaction score (0–10)
# ========================================
M5 <- make_fake_model(
  term = c("(Intercept)", "income_centered ($1k)", 
           "leisure_hours_per_day"),
  estimate = c(5.5, 0.03, 0.42),
  std.error = c(0.5, 0.008, 0.05),
  r.squared = 0.28,
  adj.r.squared = 0.27,
  nobs = 800,
  model_type = "OLS",
  outcome = "Life satisfaction score (0–10)"
)

modelsummary(
  M5,
  estimate = "{estimate} {stars}",
  statistic = "std.error",
  notes = "Standard errors in parentheses"
)

# ========================================
# Outcome: Voted in last election (0–1 probability, LPM)
# ========================================
M6 <- make_fake_model(
  term = c("(Intercept)", "age (yrs)", "age_sq (yrs^2)", 
           "education (yrs)", "female (yes=1)"),
  estimate = c(0.20, 0.015, -0.0015, 0.02, 0.06),
  std.error = c(0.03, 0.002, 0.0014, 0.008, 0.023),
  r.squared = 0.10,
  adj.r.squared = 0.09,
  nobs = 1200,
  model_type = "OLS",
  outcome = "Voted in last election (binary)"
)

modelsummary(
  M6,
  estimate = "{estimate} {stars}",
  statistic = "std.error",
  notes = "Standard errors in parentheses"
)

# ========================================
# Outcome: Number of cars owned (count)
# ========================================
M7 <- make_fake_model(
  term = c("(Intercept)", "income ($1k)", "rural (yes=1)",
           "workers (count)", "age_head (yrs)"),
  estimate = c(0.50, 0.03, 0.80, 0.40, 0.01),
  std.error = c(0.15, 0.005, 0.10, 0.08, 0.002),
  r.squared = 0.40,
  adj.r.squared = 0.39,
  nobs = 1000,
  model_type = "OLS",
  outcome = "Number of cars owned (count)"
)

# ========================================
# Outcome: Currently smokes (log-odds)
# ========================================
M8 <- make_fake_model(
  term = c("(Intercept)", "education (yrs)", "single (yes=1)", "age (yrs)", "male (yes=1)"),
  estimate = c(-1.00, -0.15, 0.30, 0.02, 0.10),
  std.error = c(0.10, 0.02, 0.08, 0.01, 0.07),
  AIC = 350,
  BIC = 370,
  nobs = 900,
  model_type = "Logistic (log-odds)",
  outcome = "Currently smokes (binary)"
)

modelsummary(
  M8,
  estimate = "{estimate} {stars}",
  statistic = "std.error",
  stars = TRUE,
  notes = "Standard errors in parentheses"
)

# ========================================
# Outcome: Currently smokes (odds ratios)
# ========================================
M9 <- make_fake_model(
  term = c("(Intercept)", "education (yrs)", "single (yes=1)",
           "divorced_or_widowed (yes=1)", "age (yrs)", "male (yes=1)"),
  estimate = c(0.3679, 0.8607, 1.3499, 1.6487, 1.0202, 1.1052),
  std.error = c(0.0368, 0.0172, 0.1080, 0.1484, 0.0102, 0.0774),
  AIC = 350,
  BIC = 370,
  nobs = 900,
  model_type = "Logistic (odds ratios)",
  outcome = "Currently smokes (binary)"
)

# ========================================
# Outcome: College enrollment (log-odds)
# ========================================
M10 <- make_fake_model(
  term = c("(Intercept)", "parent_low_ed (no HS diploma)",
           "parent_some_college (some college)", "hs_GPA (0–4)",
           "age (yrs)"),
  estimate = c(-0.50, -0.80, -0.40, 1.20, 0.05),
  std.error = c(0.15, 0.10, 0.09, 0.20, 0.03),
  AIC = 480,
  BIC = 505,
  nobs = 700,
  model_type = "Logistic (log-odds)",
  outcome = "College enrollment (binary)"
)

# ========================================
# Outcome: College enrollment (odds ratios)
# ========================================
M11 <- make_fake_model(
  term = c("(Intercept)", "parent_low_ed (no HS diploma)",
           "parent_some_college (some college)", "hs_GPA (0–4)",
           "age (yrs)"),
  estimate = c(0.6065, 0.4493, 0.6703, 3.3201, 1.0513),
  std.error = c(0.0910, 0.0449, 0.0603, 0.6640, 0.0315),
  AIC = 480,
  BIC = 505,
  nobs = 700,
  model_type = "Logistic (odds ratios)",
  outcome = "College enrollment (binary)"
)

# ========================================
# Outcome: Volunteering participation (binary, log-odds)
# ========================================
M12 <- make_fake_model(
  term = c("(Intercept)", "age (yrs)", "age_sq (yrs^2)",
           "college (vs HS)", "graduate (vs HS)", "female (yes=1)"),
  estimate = c(-2.00, 0.05, -0.00050, 0.60, 0.90, 0.20),
  std.error = c(0.25, 0.02, 0.00020, 0.08, 0.10, 0.05),
  AIC = 410,
  BIC = 430,
  nobs = 1000,
  model_type = "Logistic (log-odds)",
  outcome = "Volunteering participation (binary)"
)

# ========================================
# Outcome: Volunteering participation (binary, odds ratios)
# ========================================
M13 <- make_fake_model(
  term = c("(Intercept)", "age (yrs)", "age_sq (yrs^2)",
           "college (vs HS)", "graduate (vs HS)", "female (yes=1)"),
  estimate = c(0.1353, 1.0513, 0.9995, 1.8221, 2.4596, 1.2214),
  std.error = c(0.0338, 0.0210, 0.00020, 0.1458, 0.2460, 0.0611),
  AIC = 410,
  BIC = 430,
  nobs = 1000,
  model_type = "Logistic (odds ratios)",
  outcome = "Volunteering participation (binary)"
)

# ========================================
# Multinomial logit: 
# =======================================

mod_mnl <- make_fake_model(
  term = rep(c("(Intercept)", "wage", "hh_size"), 2),
  estimate = c(0.8, 0.05, 0.30,   -0.4, -0.02, 0.10),
  std.error = c(0.3, 0.02, 0.12,   0.25, 0.015, 0.08),
  response = rep(c("B vs A", "C vs A"), each = 3),
  AIC = -510,
  BIC = -430,
  nobs = 500,
  model_type = "Multinomial logit",
  outcome = "Job type"
)


modelsummary(
  mod_mnl,
  shape = term + response ~ statistic,
  exponentiate = TRUE,
  statistic = "conf.int",
  fmt = 2
)

# ========================================
# Multinomial logit: Commute mode choice
# ========================================

MNL_commute <- make_fake_model(
  term = rep(c("(Intercept)", "distance_km", "income_$1k", "urban (yes=1)"), 2),
  estimate = c(
    1.2, -0.30, -0.05,  1.5,   # Public vs Car
    2.0, -0.60, -0.08,  2.2    # Bike vs Car
  ),
  std.error = c(
    0.3, 0.08, 0.02, 0.25,
    0.4, 0.10, 0.03, 0.30
  ),
  response = rep(c("Public transport vs Car", "Bicycle vs Car"), each = 4),
  nobs = 1200,
  AIC = 2100,
  BIC = 2180,
  model_type = "Multinomial logit",
  outcome = "Commute mode"
)

modelsummary(
  MNL_commute,
  shape = term + response ~ statistic,
  statistic = "conf.int",
  exponentiate = TRUE,
  fmt = 2,
  notes = "Odds ratios (95% CI). Reference category: Car."
)

# ========================================
# Multinomial logit: Vote choice
# ========================================

MNL_vote <- make_fake_model(
  term = rep(c("(Intercept)", "income_$1k", "education_yrs", "age_yrs"), 2),
  estimate = c(
    -0.8, -0.02,  0.15, -0.01,   # Left vs Abstain
    -0.6,  0.08, -0.05,  0.02    # Right vs Abstain
  ),
  std.error = c(
    0.25, 0.01, 0.03, 0.005,
    0.22, 0.02, 0.02, 0.006
  ),
  response = rep(c("Left vs Abstain", "Right vs Abstain"), each = 4),
  nobs = 1800,
  AIC = 2950,
  BIC = 3040,
  model_type = "Multinomial logit",
  outcome = "Vote choice"
)

modelsummary(
  MNL_vote,
  shape = term + response ~ statistic,
  statistic = "conf.int",
  fmt = 2,
  notes = "Log-odds relative to abstention. Reference category: Abstain."
)

# ========================================
# Conditional logit: Commute mode choice
# ========================================

CL1 <- make_fake_model(
  term = c(
    "travel_time (minutes)",
    "travel_cost (USD)",
    "bike_lane (yes=1)"
  ),
  estimate = c(
    -0.05,
    -0.18,
    1.20
  ),
  std.error = c(
    0.008,
    0.035,
    0.25
  ),
  nobs = 3000,
  AIC = 2100,
  BIC = 2180,
  model_type = "Conditional logit",
  outcome = "Commute mode choice"
)

modelsummary(
  CL1,
  estimate = "{estimate} {stars}",
  statistic = "conf.int",
  exponentiate = TRUE,
  fmt = 2,
  notes = "Odds ratios with 95% CI. Effects compare otherwise identical travel modes."
)

# ========================================
# Conditional logit: University choice
# ========================================

CL2 <- make_fake_model(
  term = c(
    "distance (km)",
    "tuition (USD 1,000s)",
    "ranking score (0-100)"
  ),
  estimate = c(
    -0.10,
    -0.22,
    0.35
  ),
  std.error = c(
    0.020,
    0.060,
    0.09
  ),
  nobs = 1800,
  AIC = 2950,
  BIC = 3040,
  model_type = "Conditional logit",
  outcome = "University choice"
)

modelsummary(
  CL2,
  estimate = "{estimate} {stars}",
  statistic = "conf.int",
  fmt = 2,
  notes = "Log-odds coefficients. Comparison is between two universities for the same student."
)

# ========================================
# Conditional logit: Partner choice
# ========================================

CL3 <- make_fake_model(
  term = c(
    "distance (km)",
    "same education (yes=1)",
    "same origin (yes=1)"
  ),
  estimate = c(
    -0.30,
    0.55,
    1.05
  ),
  std.error = c(
    0.040,
    0.14,
    0.20
  ),
  nobs = 25000,
  AIC = 41200,
  BIC = 41450,
  model_type = "Conditional logit",
  outcome = "Partner choice"
)

modelsummary(
  CL3,
  estimate = "{estimate} {stars}",
  statistic = "conf.int",
  fmt = 2,
  notes = "Log-odds coefficients."
)
