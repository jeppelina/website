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


#' Simulate exam score dataset
#' @param n Number of students
#' @param missing_pct Named list with % missing for each variable (0–1)
#' @param include_interactions Logical, whether to add interaction/non-linear effects
#' @return A tibble with simulated data
simulate_exam_data <- function(n = 500, 
                               missing_pct = list(hours_studied = 0.011, gender = 0.038, previous_gpa = 0.0435),
                               include_interactions = TRUE) {
  set.seed(1234)  # For reproducibility
  

  # 1. Set field and gender first first
  field_levels <- c("social_sci", "natural_sci", "humanities")
  field <- sample(field_levels, size = n, replace = TRUE)
  
  gender <- rbinom(n, size = 1, prob = 0.5)
  
  # Let field influence latent traits
  ability <- case_when(
    field == "natural_sci" ~ rnorm(n, mean = 0.5, sd = 0.3),
    field == "social_sci"  ~ rnorm(n, mean = 0.0, sd = 1),
    field == "humanities"  ~ rnorm(n, mean = -0.3, sd = 0.6)
  )
  
  # Let women be more motivated
  motivation <- rnorm(n, mean = 0, sd = 1)
  motivation <- motivation + ifelse(gender == 1, 0.3, 0)  # Females more motivated
  
  # 2. Simulate predictors with conditional logic for realism
  parents_edu_years <- rtruncnorm(n, a = 0, b = 22, mean = 13, sd = 4)
  parents_edu_years <- round(parents_edu_years)
  
  previous_gpa <- rtruncnorm(n, a = 0, b = 100, mean = 55 + 10 * ability + 1.5 * parents_edu_years, sd = 15)  # GPA (0-100)
  previous_gpa <- round(previous_gpa)
  
  hours_studied <- rtruncnorm(n, a = 0, b = 80, mean = 15 + 10 * motivation + 0.2 * previous_gpa + 1 * parents_edu_years, sd = 5)
  hours_studied <- round(hours_studied,1)
  
  hours_slept <- rtruncnorm(n, a = 0, b = "inf", mean = 8, sd = 1.5)
  hours_slept <- round(hours_slept,1)
  

  
  field_levels <- c("social_sci", "natural_sci", "humanities")
  field <- sample(field_levels, size = n, replace = TRUE)
  
  # 3. Linear model for outcome
  exam_score <- 20 +
    0.8 * hours_studied +
    (-0.0025) * hours_studied^2 +
    0.4 * previous_gpa +
    7 * hours_slept +
    (-0.4 * hours_slept^2) +
    3 * gender +  # Let's say females = 1, males = 0
    2 * parents_edu_years +
    (-0.02 * parents_edu_years^2) +
    ifelse(field == "natural_sci", 3, 0) +
    ifelse(field == "humanities", -2, 0) +
    rnorm(n, 0, 8)  # Error term
  
  # 4. Add non-linear or interaction effects if desired
  if (include_interactions) {
    exam_score <- exam_score -
      0.03 * hours_studied * parents_edu_years     # Interaction effect
      #- 0.5 * (hours_slept - 8)^2                   # Penalty for deviating from 8h sleep
  }
  
  # Cap and floor exam scores
 #exam_score <- pmin(pmax(exam_score, 0), 100)
  exam_score <- round(exam_score)
  
  # 5. Assemble into a tibble
  data <- tibble(
    exam_score = exam_score,
    hours_studied = hours_studied,
    previous_gpa = previous_gpa,
    hours_slept = hours_slept,
    gender = gender,
    parents_edu_years = parents_edu_years,
    field = factor(field, levels = field_levels)
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

# Simulate
sim_data <- simulate_exam_data(n = 500)
print(head(sim_data))

write.csv(sim_data, "/Users/jeppelina/Documents/R/SDS II/Data/simulated_exam_data.csv", row.names = FALSE)

# Analyze data
sim_data <- sim_data %>% clean_names()

# Summary statistics
glimpse(sim_data)
skim(sim_data)

# Visualize distributions
sim_data %>%
  dplyr::select(exam_score, hours_studied, previous_gpa, hours_slept, parents_edu_years) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, fill = "coral", alpha = 0.7) +
  facet_wrap(~name, scales = "free") +
  theme_minimal() +
  labs(title = "Distributions of Key Variables")


# Correlations
cor_matrix <- sim_data %>%
  dplyr::select(where(is.numeric)) %>%
  correlate()

cor_matrix %>%
  shave(upper = FALSE) %>%
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

# Box plots
ggplot(sim_data, aes(x = field, y = exam_score, fill = field)) +
  geom_boxplot(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Exam Score by Field of Study", y = "Exam Score", x = "Field")

sim_data %>%
  mutate(gender = factor(gender, labels = c("Male", "Female"))) %>%
  ggplot(aes(x = gender, y = exam_score, fill = gender)) +
  geom_boxplot(alpha = 0.6) +
  theme_minimal() +
  labs(title = "Exam Score by Gender", y = "Exam Score", x = "")

# linear fit
g <- sim_data %>%
  pivot_longer(cols = c(hours_studied, previous_gpa, hours_slept, parents_edu_years)) %>%
  ggplot(aes(x = value, y = exam_score)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  facet_wrap(~name, scales = "free_x") +
  theme_minimal() +
  labs(title = "Exam Score vs Predictors (Linear Fit)")

g

# non-linear checks
ggplot(sim_data, aes(x = previous_gpa, y = exam_score)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  theme_minimal()

ggplot(sim_data, aes(x = hours_slept, y = exam_score)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  theme_minimal()

ggplot(sim_data, aes(x = parents_edu_years, y = exam_score)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  theme_minimal()

ggplot(sim_data, aes(x = hours_studied, y = exam_score)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  theme_minimal()


sim_data <- sim_data %>%
  mutate(hours_centered = hours_studied - mean(hours_studied, na.rm = TRUE),
         parents_edu_centered = parents_edu_years - mean(parents_edu_years, na.rm = TRUE))

# models

model <- lm(exam_score ~ hours_studied, data = sim_data)
modelb <- lm(exam_score ~ hours_centered, data = sim_data)

model2 <- lm(exam_score ~ hours_studied + parents_edu_years, data = sim_data)

model3 <- lm(exam_score ~ hours_studied + parents_edu_years + previous_gpa, data = sim_data)

model4 <- lm(exam_score ~ hours_studied * parents_edu_years + previous_gpa, data = sim_data)
model4b <- lm(exam_score ~ hours_centered * parents_edu_centered + previous_gpa, data = sim_data)

model5 <- lm(exam_score ~ hours_studied + I(hours_studied^2) + previous_gpa +
               parents_edu_years + 
               hours_studied * parents_edu_years, data = sim_data)

model6 <- lm(exam_score ~ hours_studied + previous_gpa +
               parents_edu_years + I(parents_edu_years^2) +
               hours_studied * parents_edu_years +
               gender + field, data = sim_data)


summary(model)
summary(modelb)
summary(model2)
summary(model3)
summary(model4)
summary(model4b)
summary(model5)
summary(model6)



# Visuals

table(sim_data$parents_edu_years, useNA = "always")

# Check overlap
sim_data %>%
  mutate(
    pedu_group = cut(parents_edu_years, breaks = c(0,10,15,20,25),
                     labels = c("<10","10–15","15–20",">20"))
  ) %>%
  ggplot(aes(x = pedu_group, y = hours_studied)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  theme_minimal() +
  labs(x = "Parental education group", y = "Hours studied")

# Create groups
sim_data <- sim_data %>%
  mutate(edu_group = case_when(
    parents_edu_years < 10 ~ "< 10",
    parents_edu_years < 16 ~ "10–15",
    parents_edu_years < 20 ~ "15–19",
    TRUE ~ "20+"
  ))

# Convert to factor for ordering (optional)
sim_data$edu_group <- factor(sim_data$edu_group)

table(sim_data$edu_group)
table(sim_data$edu_group, sim_data$parents_edu_years)

# Separate regressions for each group
ggplot(sim_data, aes(x = hours_studied, y = exam_score, color = edu_group)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Exam score vs. Study hours",
       color = "parental yrs of edu") +
  theme_minimal()

# Plot predictions
plot_predictions(
  model4,
  condition = list(
    "hours_studied",
    parents_edu_years = quantile(sim_data$parents_edu_years, probs = c(0.05, 0.5, 0.95), na.rm = TRUE)
  )
) +
  theme_minimal()

mfx <- slopes(
  model4,
  variables = "hours_studied",
  newdata = datagrid(
    parents_edu_years = seq(0, 22, by = 2)
  )
)
mfx


plot_slopes(
  model4,
  variables = "hours_studied",
  condition = "parents_edu_years"
) +
  theme_minimal()

# Plot predictions with centered
plot_predictions(
  model4b,
  condition = list(
    "hours_centered",
    parents_edu_centered = quantile(sim_data$parents_edu_centered, probs = c(0.05, 0.5, 0.95), na.rm = TRUE)
  )
) +
  theme_minimal()

mfx <- slopes(
  model4b,
  variables = "hours_centered",
  newdata = datagrid(
    parents_edu_centered = seq(0, 22, by = 2)
  )
)


plot_slopes(
  model4b,
  variables = "hours_centered",
  condition = "parents_edu_centered"
) +
  theme_minimal()

# Diagnostics
par(mfrow = c(2, 1))
plot(model, which = 1:2)


# Outliers
sim_data %>%
  ggplot(aes(x = "", y = exam_score)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Outliers in Exam Score")
