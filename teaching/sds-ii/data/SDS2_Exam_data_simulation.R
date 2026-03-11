
# Load required packages
library(tibble)
library(ggplot2)
library(purrr)
library(stringr)
library(janitor)
library(truncnorm)
library(corrr)
library(car)
library(relaimpo)
library(dplyr)
library(tidyr)
library(marginaleffects)
library(modelsummary)

simulate_crime_data <- function(
    n = 1000,
    missing_pct = list(education_years = 0.00, 
                       substance_use = 0.00, 
                       teenage_mental_health = 0.00),
    include_interactions = TRUE,
    varnames = NULL
) {
  set.seed(1234)
  
  # ---------- 1. Background ----------
  parents_income <- rtruncnorm(n, a = 0, b = 500, mean = 50, sd = 20)
  parents_income <- round(parents_income, 1)
  
  childhood_abuse_lambda <- 5 - (parents_income - 12) / 10
  childhood_abuse_lambda <- pmax(childhood_abuse_lambda, 0.2)
  childhood_abuse <- pmin(rpois(n, lambda = childhood_abuse_lambda), 5)
  
  teenage_mental_health <- rtruncnorm(
    n, a = 0, b = 100,
    mean = 80 - 15 * childhood_abuse, sd = 10
  )
  teenage_mental_health <- round(teenage_mental_health, 1)
  
  logit_substance_use <- -0.1 * (teenage_mental_health - 50) + 
    0.5 * childhood_abuse
  substance_use_prob <- 1 / (1 + exp(-logit_substance_use))
  substance_use <- rbinom(n, 1, substance_use_prob)
  
  education_years <- rtruncnorm(
    n, a = 0, b = 22,
    mean = 12 + 0.1 * parents_income, sd = 2
  )
  education_years <- round(education_years, 1)
  
  # ---------- 2. Outcome ----------
  violent_crime_score <- 80 +
    10 * substance_use +
    (-0.15) * parents_income +
    (-0.5) * education_years +
    (0.05) * (education_years ^ 2) +
    (-0.2) * teenage_mental_health +
    5 * childhood_abuse +
    rnorm(n, 0, 20)
  
  if (include_interactions) {
    violent_crime_score <- violent_crime_score +
      0.3 * substance_use * childhood_abuse 
  }
  
  violent_crime_score <- round(violent_crime_score, 1)
  
  # ---------- 3. Extra variable ----------
  econ_crime_prob <- 1 / (1 + exp(4 + -0.02 * parents_income))
  prior_econ_crime <- rbinom(n, 1, econ_crime_prob)
  
  imprisonment_prob <- 1 / (1 + exp(
    5 + -0.06 * violent_crime_score + -4.5 * prior_econ_crime
  ))
  imprisonment <- rbinom(n, 1, imprisonment_prob)
  
  # ---------- 4. Assemble ----------
  data <- tibble(
    violent_crime_score,
    education_years,
    parents_income,
    childhood_abuse,
    teenage_mental_health,
    substance_use,
    prior_econ_crime,
    imprisonment
  )
  
  # ---------- 5. Missingness ----------
  for (var in names(missing_pct)) {
    if (var %in% names(data)) {
      idx <- sample(1:n, floor(missing_pct[[var]] * n))
      data[[var]][idx] <- NA
    }
  }
  
  # ---------- 6. Rename if user provided map ----------
  if (!is.null(varnames)) {
    for (old in names(varnames)) {
      names(data)[names(data) == old] <- varnames[[old]]
    }
  }
  
  return(as.data.frame(data))
}


# Example usage
df <- simulate_crime_data(
  n = 500,
  varnames = list(
    violent_crime_score = "risk_of_poverty",
    education_years = "education_yrs",
    parents_income = "parents_income",
    childhood_abuse = "bullying_experience",
    teenage_mental_health = "attendance_rate_in_school",
    substance_use = "substance_use",
    prior_econ_crime = "parents_criminal_record",
    imprisonment = "committed_crime"
  )
)

head(df)

# Visualize distributions of all numeric variables
df %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, fill = "coral", alpha = 0.7) +
  facet_wrap(~name, scales = "free") +
  theme_minimal() +
  labs(title = "Distributions of Numeric Variables")

# Correlation matrix
cor_matrix <- df %>%
  select(where(is.numeric)) %>%
  correlate()

cor_matrix %>% shave() %>% fashion()

cor_matrix %>%
  stretch(na.rm = TRUE) %>%
  ggplot(aes(x = x, y = y, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(r, 2)), size = 3) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                       midpoint = 0, limit = c(-1, 1),
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix")

# Scatterplots with linear fit for numeric predictors
df %>%
  pivot_longer(cols = c("education_yrs", "parents_income", "substance_use")) %>%
  ggplot(aes(x = value, y = risk_of_poverty)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  facet_wrap(~name, scales = "free_x") +
  theme_minimal() +
  labs(title = "Scatterplots of Predictors vs Risk of Poverty")


# Fit full model
m_full <- lm(
  risk_of_poverty ~ education_yrs + parents_income + substance_use,
  data = df
)
summary(m_full)

# Plot predictions
plot_predictions(m_full, condition = list("education_yrs"))

# Diagnostics
par(mfrow = c(2, 1))
plot(m_full, which = 1:2)