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
library(margins)

###DAG for quarto

# ```{mermaid}
# flowchart LR
# AB[Childhood abuse] --> SC[Substance use]
# AB --> MH[Teenage Mental health]
# AB --> VI[Violent crime score]
# 
# EDU[Education] --> VI
# FI[Parents income] --> EDU
# FI --> AB
# 
# MH --> SC
# MH --> VI
# SC --> VI
# 
# VI --> IM[Imprisonment]
# 
# PO[Prior Economic crime] --> IM
# FI --> PO
# 
# classDef exp fill:#cfc;
#   classDef out fill:#ccf;
#   classDef con fill:#ddd,stroke:#333;
#   classDef coll fill:#fdd;
#   
#   class MH exp
# class VI out;
# class IM coll;
# ```

### SIMULATION

#' Simulate violent crime risk dataset
#' @param n Number of individuals
#' @param missing_pct Named list with % missing for each variable (0–1)
#' @param include_interactions Logical, whether to add interaction effects
#' @return A tibble with simulated data
simulate_crime_data <- function(n = 1000,
                                missing_pct = list(education_years = 0.00, 
                                                   substance_use = 0.00, 
                                                   teenage_mental_health = 0.00),
                                include_interactions = FALSE) {
  set.seed(1234)  # For reproducibility
  
  # 1. Generate background variables using truncated distributions
  parents_income <- rtruncnorm(n, a = 0, b = 500, mean = 50, sd = 20)
  parents_income <- round(parents_income, 1)
  
  childhood_abuse_lambda <- 5 - (parents_income - 12) / 10
  childhood_abuse_lambda <- pmax(childhood_abuse_lambda, 0.2)  # avoid zeros
  childhood_abuse <- rpois(n, lambda = childhood_abuse_lambda)
  childhood_abuse <- pmin(childhood_abuse, 5)  # cap at 5
  hist(childhood_abuse)
  
  teenage_mental_health <- rtruncnorm(n, a = 0, b = 100, mean = 80 - 15 * childhood_abuse, sd = 10)
  teenage_mental_health <- round(teenage_mental_health, 1)
  
  logit_substance_use <- -0.1 * (teenage_mental_health - 50) + 0.5 * childhood_abuse
  substance_use_prob <- 1 / (1 + exp(-logit_substance_use))
  substance_use <- rbinom(n, size = 1, prob = substance_use_prob)
  
  education_years <- rtruncnorm(n, a = 0, b = 22, mean = 12 + 0.1 * parents_income, sd = 2)
  education_years <- round(education_years, 1)
  
  # 2. Generate outcome variable
  violent_crime_score <- 80 +
    10 * substance_use +
    (-0.15) * parents_income +
    (-0.5) * education_years +
    (-0.2) * teenage_mental_health +
    5 * childhood_abuse +
    rnorm(n, 0, 20)
  
  if (include_interactions) {
    violent_crime_score <- violent_crime_score +
      0.3 * substance_use * childhood_abuse
  }
  
  violent_crime_score <- round(violent_crime_score, 1)
  
  # 3. Independent economic crime (will become spurious if conditioned on imprisonment)
  econ_crime_prob <- 1 / (1 + exp(4 + -0.02 * parents_income))
  prior_econ_crime <- rbinom(n, size = 1, prob = econ_crime_prob)
  
  # 4. Collider: imprisonment, caused by both violence and econ crime
  imprisonment_prob <- 1 / (1 + exp(5 + -0.06 * violent_crime_score + -4.5 * prior_econ_crime))
  imprisonment <- rbinom(n, size = 1, prob = imprisonment_prob)
  
  # 5. Assemble into tibble
  data <- tibble(
    violent_crime_score = violent_crime_score,
    education_years = education_years,
    parents_income = parents_income,
    childhood_abuse = childhood_abuse,
    teenage_mental_health = teenage_mental_health,
    substance_use = substance_use,
    prior_econ_crime = prior_econ_crime,
    imprisonment = imprisonment
  )
  
  # 6. Introduce missingness
  for (var in names(missing_pct)) {
    if (var %in% names(data)) {
      missing_idx <- sample(1:n, size = floor(missing_pct[[var]] * n))
      data[[var]][missing_idx] <- NA
    }
  }
  
  data <- as.data.frame(data)
  return(data)
}
  
  # ---- RUN SIMULATION AND EXPORT ----
  sim_data <- simulate_crime_data(n = 500)
  print(head(sim_data))
  
  write.csv(sim_data, "/Users/jeppelina/Documents/R/SDS II/Data/simulated_crime_data.csv", row.names = FALSE)
  
  # ---- EDA ----
  sim_data <- sim_data %>% clean_names()
  
  # Summary structure
  glimpse(sim_data)
  
  # Summary statistics
  sim_data %>%
    summarise(
      n = n(),
      crime_mean = mean(violent_crime_score, na.rm = TRUE),
      crime_sd = sd(violent_crime_score, na.rm = TRUE),
      income_mean = mean(parents_income, na.rm = TRUE),
      edu_mean = mean(education_years, na.rm = TRUE),
      teenage_mental_health_mean = mean(teenage_mental_health, na.rm = TRUE),
      substance_use_prop = mean(substance_use, na.rm = TRUE),
      prior_econ_crime_prop = mean(prior_econ_crime, na.rm = TRUE),
      imprisonment_rate = mean(imprisonment, na.rm = TRUE)
    )
  
  # Distribution plots
  sim_data %>%
    dplyr::select(violent_crime_score, education_years, childhood_abuse, parents_income, teenage_mental_health) %>%
    pivot_longer(everything()) %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins = 20, fill = "coral", alpha = 0.7) +
    facet_wrap(~name, scales = "free") +
    theme_minimal() +
    labs(title = "Distributions of Key Continuous Variables")
  
  # Correlation matrix
  cor_matrix <- sim_data %>%
    dplyr::select(where(is.numeric)) %>%
    correlate()
  
  cor_matrix %>%
    shave() %>%
    fashion()
  
  cor_matrix %>%
    stretch(na.rm = TRUE) %>%
    ggplot(aes(x = x, y = y, fill = r)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(r, 2)), size = 3) +
    scale_fill_gradient2(
      low = "red", mid = "white", high = "blue",
      midpoint = 0, limit = c(-1, 1),
      name = "Correlation"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Correlation Matrix")
  
  # Box plots for binary variables
  ggplot(sim_data, aes(x = factor(substance_use), y = violent_crime_score, fill = factor(substance_use))) +
    geom_boxplot(alpha = 0.6) +
    theme_minimal() +
    labs(title = "Violent Crime Score by Substance Use", x = "Substance Use", y = "Crime Score")
  
  ggplot(sim_data, aes(x = factor(prior_econ_crime), y = violent_crime_score, fill = factor(prior_econ_crime))) +
    geom_boxplot(alpha = 0.6) +
    theme_minimal() +
    labs(title = "Crime Score by Prior Economic Crime", x = "Prior Econ Crime", y = "Crime Score")
  
  # Linear fits for key predictors
  sim_data %>%
    pivot_longer(cols = c(education_years, parents_income, teenage_mental_health, childhood_abuse)) %>%
    ggplot(aes(x = value, y = violent_crime_score)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
    facet_wrap(~name, scales = "free_x") +
    theme_minimal() +
    labs(title = "Violent Crime Score vs Predictors (Linear Fit)")
  
  
### ANALYSIS
  
  # 1. Bivariate model — naive estimate
  m_conf1 <- lm(violent_crime_score ~ education_years, data = sim_data)
  summary(m_conf1)
  
  # Interpretation:
  # This gives the *total observed association* between education and crime.
  # It's likely biased due to confounding by background variables.
  
  # 2. Add family income — controls for socioeconomic background
  m_conf2 <- lm(violent_crime_score ~ education_years + parents_income, data = sim_data)
  summary(m_conf2)
  
  # 3. Add mental health — another confounder affecting both education and crime
  m_conf3 <- lm(violent_crime_score ~ education_years + parents_income + teenage_mental_health, data = sim_data)
  summary(m_conf3)
  
  # Optional 4. Add both mental health + child abuse  (which might be a mediator or confounder)
  m_conf4 <- lm(violent_crime_score ~ education_years + parents_income + childhood_abuse + teenage_mental_health, data = sim_data)
  summary(m_conf4)
  
  # Visual comparison of coefficients
  library(broom)
  bind_rows(
    tidy(m_conf1) %>% mutate(model = "1. No controls"),
    tidy(m_conf2) %>% mutate(model = "2. Add income"),
    tidy(m_conf3) %>% mutate(model = "3. Add mental health"),
    tidy(m_conf4) %>% mutate(model = "4. Add Abuse")
  ) %>%
    filter(term == "education_years") %>%
    ggplot(aes(x = model, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                      ymax = estimate + 1.96*std.error), width = 0.2) +
    labs(title = "Effect of Education on Crime under Different Specifications",
         y = "Coefficient for Education", x = "")
  
### Mediation analysis
  # 1. Total effect of childhood abuse
  m_total <- lm(violent_crime_score ~ childhood_abuse, data = sim_data)
  summary(m_total)
  
  # Direct effect after adjusting for mediators
  m_mediated <- lm(violent_crime_score ~ childhood_abuse + teenage_mental_health + substance_use, data = sim_data)
  summary(m_mediated)
  
  # Path a1: AB → MH
  m_ab_mh <- lm(teenage_mental_health ~ childhood_abuse, data = sim_data)
  
  # Path a2: AB → SC (directly)
  m_ab_sc <- glm(substance_use ~ childhood_abuse, family = binomial, data = sim_data)
  
  # Path a3: MH → SC
  m_mh_sc <- glm(substance_use ~ teenage_mental_health + childhood_abuse, family = binomial, data = sim_data)
  
  # Path b1: MH → VI
  m_mh_vi <- lm(violent_crime_score ~ teenage_mental_health + childhood_abuse, data = sim_data)
  
  # Path b2: SC → VI
  m_sc_vi <- lm(violent_crime_score ~ substance_use + childhood_abuse + teenage_mental_health, data = sim_data)
  
  
### Adjusting for Collider
  
  # 1. 
  m_col0 <- lm(violent_crime_score ~ prior_econ_crime, data = sim_data)
  summary(m_col0)
  
  m_col1 <- lm(violent_crime_score ~ teenage_mental_health + prior_econ_crime, data = sim_data)
  summary(m_col1)
  tidy(m_col1, conf.int = TRUE, conf.level = 0.95)
# Expectation: coefficient close to 0, since POE is independent of violence
  
  m_col2 <- lm(violent_crime_score ~ teenage_mental_health + prior_econ_crime + imprisonment, data = sim_data)
  summary(m_col2)
  tidy(m_col2, conf.int = TRUE, conf.level = 0.95)


  # Optional: visualize distributions by imprisonment
  ggplot(sim_data, aes(x = factor(imprisonment), fill = factor(prior_econ_crime))) +
    geom_bar(position = "fill") +
    labs(title = "Proportion of Prior Economic Crime by Imprisonment")
