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
library(margins)

### SIMULATION

#' @param n Number of individuals
#' @param missing_pct Named list with % missing for each variable (0–1)
#' @param include_interactions Logical, whether to add interaction effects
#' @return A tibble with simulated data
simulate_crime_data <- function(n = 1000,
                                missing_pct = list(education_years = 0.024, 
                                                   substance_use = 0.00, 
                                                   teenage_mental_health = 0.02),
                                include_interactions = FALSE) {
  set.seed(1234)  # For reproducibility
  
  # 1. Generate background variables using truncated distributions
  parents_income <- rtruncnorm(n, a = 0, b = 500, mean = 50, sd = 30)
  parents_income <- round(parents_income, 1)
  
  childhood_abuse_lambda <- 2.2 - 0.02 * parents_income
  childhood_abuse_lambda <- pmax(childhood_abuse_lambda, 0.2)  # avoid zeros
  childhood_abuse <- rpois(n, lambda = childhood_abuse_lambda)
  childhood_abuse <- pmin(childhood_abuse, 5)  # cap at 5
  hist(childhood_abuse)
  table(childhood_abuse)
  
  teenage_mental_health <- rtruncnorm(n, a = 0, b = 100, mean = 80 - 10 * childhood_abuse, sd = 18)
  teenage_mental_health <- round(teenage_mental_health, 0)
  hist(teenage_mental_health)
  
  logit_substance_use <-  6 + -0.2 * teenage_mental_health + 1.5 * childhood_abuse
  substance_use_prob <- 1 / (1 + exp(-logit_substance_use))
  substance_use <- rbinom(n, size = 1, prob = substance_use_prob)
  hist(substance_use_prob)
  table(substance_use)
  
  education_years <- rtruncnorm(n, a = 0, b = 22, mean = 12 + 0.07 * parents_income, sd = 4)
  education_years <- round(education_years, 0)
  
  logit_unemployed <- -0.6 - 0.01 * education_years - 0.015 * parents_income
  unemployed_prob <- 1 / (1 + exp(-logit_unemployed))
  unemployed <- rbinom(n, size = 1, prob = unemployed_prob)
  table(unemployed)
  
  # Bin parental income to make birth order
  income_bin <- cut(parents_income,
                    breaks = c(-Inf, 30, 60, 90, Inf),
                    labels = c("low", "midlow", "midhigh", "high"))
  
  # Define birth order probabilities per income group
  birth_probs <- list(
    low     = c(0.35, 0.3, 0.2, 0.15),    # more higher birth order
    midlow  = c(0.40, 0.35, 0.15, 0.1),
    midhigh = c(0.45, 0.35, 0.15, 0.05),
    high    = c(0.55, 0.35, 0.08, 0.02)     # mostly 1st and 2nd born
  )
  
  # Sample birth order based on income bin
  birth_order <- map_int(income_bin, function(inc) {
    sample(1:4, size = 1, prob = birth_probs[[as.character(inc)]])
  })
  
  hours_tv_weekly <- rtruncnorm(
    n,
    a = 0,
    b = 40,
    mean = 20 - 0.2 * education_years,  # negative slope
    sd = 5
  )
  hours_tv_weekly <- round(hours_tv_weekly, 1)
  
  # 2. Generate eta
  eta <- 2 +
    0.6  * substance_use +
    -0.03 * parents_income +
    -0.1  * education_years +
    -0.02 * teenage_mental_health +
    0.25  * childhood_abuse +
    0.4  * unemployed
  
  if (include_interactions) {
    eta <- eta +
      0.2 * substance_use * childhood_abuse
  }

  
  # Imprisonment
  imprisonment_prob <- 1 / (1 + exp(-eta))
  imprisonment <- rbinom(n, size = 1, prob = imprisonment_prob)
  hist(imprisonment_prob)
  table(imprisonment)
  
  # 5. Assemble into tibble
  data <- tibble(
    education_years = education_years,
    parents_income = parents_income,
    childhood_abuse = childhood_abuse,
    teenage_mental_health = teenage_mental_health,
    substance_use = substance_use,
    imprisonment = imprisonment,
    unemployed = unemployed,
    birth_order = birth_order,
    hours_tv_weekly = hours_tv_weekly
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
  sim_data <- simulate_crime_data()
  print(head(sim_data))
  
  write.csv(sim_data, "/Users/jeppelina/Documents/R/SDS II/Data/simulated_crime_logistic_data.csv", row.names = FALSE)
  
  # ---- EDA ----
  sim_data <- sim_data %>% clean_names()
  
  # Summary structure
  glimpse(sim_data)
  
  # Summary statistics
  sim_data %>%
    summarise(
      n = n(),
      income_mean = mean(parents_income, na.rm = TRUE),
      edu_mean = mean(education_years, na.rm = TRUE),
      teenage_mental_health_mean = mean(teenage_mental_health, na.rm = TRUE),
      substance_use_prop = mean(substance_use, na.rm = TRUE),
      imprisonment_rate = mean(imprisonment, na.rm = TRUE)
    )
  
  # Distribution plots
  sim_data %>%
    dplyr::select(education_years, childhood_abuse,
                  parents_income, teenage_mental_health, birth_order,
                  hours_tv_weekly, unemployed, substance_use, imprisonment) %>%
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
  

  # Bivariate Linear probability models for key predictors
  sim_data %>%
    pivot_longer(cols = c(education_years, parents_income, teenage_mental_health,
                          substance_use, unemployed, birth_order, hours_tv_weekly, childhood_abuse)) %>%
    ggplot(aes(x = value, y = imprisonment)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
    facet_wrap(~name, scales = "free_x") +
    theme_minimal() +
    labs(title = "Imprisonment vs Predictors (Linear Fit)")
  
  #####
  # ANALYSIS
  #####
  
  ##### COMPARING BIVARIATE MODELS
  m1 <- lm(imprisonment ~ parents_income, data = sim_data)
  summary(m1)
  
  m2 <- glm(imprisonment ~ parents_income, data = sim_data, family = binomial)
  summary(m2)
  
  
  # Create a sequence of parent income values
  pred_df <- tibble(parents_income = seq(min(sim_data$parents_income, na.rm = TRUE),
                                         max(sim_data$parents_income, na.rm = TRUE),
                                         length.out = 200)) 
  pred_df <- pred_df %>%
    mutate(pred_prob = predict(m2, newdata = ., type = "response")) %>%
    mutate(pred_prob_lm = predict(m1, newdata = ., type = "response"))
  
  ggplot(sim_data, aes(x = parents_income, y = imprisonment)) +
    geom_jitter(height = 0.05, alpha = 0.3) +
    geom_line(data = pred_df, aes(y = pred_prob, color = "Logistic"), linewidth = 1.2) +
    geom_line(data = pred_df, aes(y = pred_prob_lm, color = "LPM"), linewidth = 1.2) +
    labs(
      y = "Probability of imprisonment",
      x = "Parents' income",
      color = "Model"   # legend title
    ) +
    scale_color_manual(values = c("Logistic" = "steelblue", "LPM" = "coral")) +
    theme_minimal()
  
  # Fitted model predictions
  sim_data$p_hat <- predict(m2, type = "response")
  sim_data$p_hat_lm <- predict(m1, type = "response")
  
  # Classify at 0.5 cutoff
  sim_data$y_hat <- ifelse(sim_data$p_hat >= 0.5, 1, 0)
  sim_data$y_hat_lm <- ifelse(sim_data$p_hat_lm >= 0.5, 1, 0)
  
  # Error rate = proportion misclassified
  sim_data$error <- sim_data$y_hat != sim_data$imprisonment
  mean(sim_data$error)
  
  sim_data$error_lm <- sim_data$y_hat_lm != sim_data$imprisonment
  mean(sim_data$error_lm)
  
  # Error rate of null model = guess the majority for everyone
  err_null <- min(mean(sim_data$imprisonment==1), mean(sim_data$imprisonment==0))
  err_null
  
  # Confusion Matrix Logit
  confusion_matrix <- table(
    Observed = factor(sim_data$imprisonment, levels = c(0,1),
                      labels = c("No imprisonment","Imprisoned")),
    Predicted_Logistic = factor(predict(m2, type="response") > 0.5,
                       levels = c(FALSE, TRUE),
                       labels = c("Predicted No","Predicted Yes"))
  )
  
  confusion_matrix 
  
  confusion_matrix_row <- prop.table(confusion_matrix, margin = 1)  # rows sum to 1
  round(confusion_matrix_row, 3)
  
  # Confusion Matrix LPM
  confusion_matrix_lm <- table(
    Observed = factor(sim_data$imprisonment, levels = c(0,1),
                      labels = c("No imprisonment","Imprisoned")),
    Predicted_LPM = factor(predict(m1, type="response") > 0.5,
                       levels = c(FALSE, TRUE),
                       labels = c("Predicted No","Predicted Yes"))
  )
  
  confusion_matrix_lm 
  
  confusion_matrix_lm_row <- prop.table(confusion_matrix_lm, margin = 1)  # rows sum to 1
  round(confusion_matrix_lm_row, 3)
  
  
  # decile bins
  calib <- sim_data %>%
    mutate(bin = ntile(p_hat, 10)) %>%
    group_by(bin) %>%
    summarise(
      mean_p = mean(p_hat),
      obs    = mean(imprisonment),
      n      = n(),
      .groups = "drop"
    )
  
  ggplot(calib, aes(x = mean_p, y = obs)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_point(size = 2) +
    geom_line() +
    labs(x = "Mean predicted probability (bin)",
         y = "Observed event rate (bin)",
         title = "Calibration plot Logistic regression (deciles)") +
    coord_equal(xlim = c(0,1), ylim = c(0,1)) +
    theme_minimal()
  
  # decile bins lm
  calib_lm <- sim_data %>%
    mutate(bin = ntile(p_hat_lm, 10)) %>%
    group_by(bin) %>%
    summarise(
      mean_p = mean(p_hat_lm),
      obs    = mean(imprisonment),
      n      = n(),
      .groups = "drop"
    )
  
  ggplot(calib_lm, aes(x = mean_p, y = obs)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_point(size = 2) +
    geom_line() +
    labs(x = "Mean predicted probability (bin)",
         y = "Observed event rate (bin)",
         title = "Calibration plot LPM (deciles)") +
    coord_equal(xlim = c(0,1), ylim = c(0,1)) +
    theme_minimal()
  
  
  ###### MULTIVARIATE LOGISTIC
  
  m3 <- glm(imprisonment ~ parents_income + childhood_abuse, data = sim_data, family = binomial)
  summary(m3)
  mean((predict(m3, type = "response") > 0.5) != sim_data$imprisonment)
  
  # Create a grid of values for parents_income, repeated for each substance_use group
  pred_df <- expand.grid(
    parents_income = seq(min(sim_data$parents_income, na.rm = TRUE),
                         max(sim_data$parents_income, na.rm = TRUE),
                         length.out = 200),
    childhood_abuse = c(0, 1, 2, 3, 4, 5)   
  )
  
  # Predicted probabilities from logistic model
  pred_df$pred_prob <- predict(m3, newdata = pred_df, type = "response")
  
  # Plot
  ggplot(sim_data, aes(x = parents_income, y = imprisonment)) +
    geom_jitter(height = 0.05, alpha = 0.2) +
    geom_line(data = pred_df, 
              aes(y = pred_prob, color = factor(childhood_abuse)), 
              linewidth = 1.2) +
    labs(
      x = "Parents' income",
      y = "Probability of imprisonment",
      color = "Childhood abuse (Likert)"
    ) +
    scale_color_viridis_d(option = "plasma", direction = 1) +
    theme_minimal()
  
    
  #using marginaleffects
  
  plot_predictions(
    m3,
    condition = list(
      "parents_income",
      "childhood_abuse"
    )
      ) +
    theme_minimal() +
    labs(
      title = "Predicted imprisonment by parental income across childhood abuse levels",
      x = "Parents' income", y = "Probability of imprisonment"
    )
  
    #### MORE VARIABLES
    m4 <- glm(imprisonment ~ parents_income + education_years +
                childhood_abuse + unemployed + teenage_mental_health,
              data = sim_data, family = binomial)
    
    summary(m4)
    
    m42 <- glm(imprisonment ~ parents_income + education_years +
                childhood_abuse + unemployed + teenage_mental_health + substance_use,
              data = sim_data, family = binomial)
    
    summary(m42)
    mean((predict(m4, type = "response") > 0.5) != sim_data$imprisonment)
    
    # Predict two type cases
    type_cases <- tibble(
      parents_income = c(80, 20),
      education_years = c(18, 10),
      childhood_abuse = c(0, 3),
      teenage_mental_health = c(80, 20),
      substance_use = c(0, 1),
      unemployed = c(0, 1)
    )
    
    type_cases$pred_prob <- predict(m4, newdata = type_cases, type = "response")
    type_cases$pred_prob
    
    ## AME
    mfx_m4 <- margins(m4)
    summary(mfx_m4)
    
    mfx_df <- as.data.frame(summary(mfx_m4))
    
    ggplot(mfx_df, aes(x = reorder(factor, AME), y = AME)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = AME - 1.96*SE, ymax = AME + 1.96*SE), width = 0.2) +
      coord_flip() +
      labs(x = NULL, y = "Average Marginal Effect (pp)",
           title = "AMEs with 95% CI") +
      theme_minimal()
    
    ### Compare final model with LPM equivalent
    m5 <- lm(imprisonment ~ parents_income + education_years +
                childhood_abuse + unemployed + teenage_mental_health +
                substance_use,
              data = sim_data)
    
    summary(m5)
    mean((predict(m5, type = "response") > 0.5) != sim_data$imprisonment)  
  

  