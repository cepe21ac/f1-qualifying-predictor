# F1 Qualifying Predictor in R
# Author: Cesare Pesci
# This script predicts second stint lap times based on driver, session, circuit, weather, and tyre condition

library(dplyr)
library(ggplot2)

# Simulate base data
set.seed(123)
n <- 300

drivers <- c("Piastri", "Norris", "Hamilton", "Leclerc", "Russell", "Antonelli", "Verstappen", "Tsunoda", "Alonso", "Stroll", "Gasly", "Doohan", "Albon", "Sainz", "Ocon", "Bearman", "Hulkenberg", "Bortoleto", "Tsunoda", "Lawson", "Hadjar")
sessions <- c("Q1", "Q2", "Q3")
circuits <- c("Australia", "China", "Monaco", "Bahrain", "Jeddah", "Miami", "Imola", "Spain", "Silverstone", "Monza", "Spain", "Canada", "Austria", "Japan", "Singapore", "Spa", "Hungary", "Netherlands", "Baku", "Austin", "Mexico", "Brazil", "Las Vegas", "Qatar", "Abu Dhabi")
conditions <- c("Dry", "Wet")
tyres <- c("New", "Used")
tyre_compounds <- c("Hard", "Medium", "Soft", "Inter", "Wet")


qual_data <- data.frame(
  driver = sample(drivers, n, replace = TRUE),
  session = sample(sessions, n, replace = TRUE),
  circuit = sample(circuits, n, replace = TRUE),
  condition = sample(conditions, n, replace = TRUE),
  tyre_compound = sample(tyre_compounds, n, replace = TRUE),
  first_tyre_used = sample(tyres, n, replace = TRUE),
  first_stint_time = rnorm(n, mean = 91.318, sd = 1)
)

# Apply scenario logic + tyre effect
qual_data <- qual_data %>%
  mutate(
    session_effect = case_when(
      # Dry condition + dry tyres
      condition == "Dry" & session == "Q1" & tyre_compound == "Hard" ~ 0.2,
      condition == "Dry" & session == "Q1" & tyre_compound == "Medium" ~ 0.35,
      condition == "Dry" & session == "Q1" & tyre_compound == "Soft" ~ 0.4,
      
      condition == "Dry" & session == "Q2" & tyre_compound == "Hard" ~ 0.15,
      condition == "Dry" & session == "Q2" & tyre_compound == "Medium" ~ 0.2,
      condition == "Dry" & session == "Q2" & tyre_compound == "Soft" ~ 0.3,
      
      condition == "Dry" & session == "Q3" & tyre_compound == "Hard" ~ 0.05,
      condition == "Dry" & session == "Q3" & tyre_compound == "Medium" ~ 0.1,
      condition == "Dry" & session == "Q3" & tyre_compound == "Soft" ~ 0.15,
      
      # Wet condition + wet tyres
      condition == "Wet" & tyre_compound == "Inter" ~ 0.25,
      condition == "Wet" & tyre_compound == "Wet" ~ 0.15,
      
      # Special case for Monaco
      circuit == "Monaco" ~ 0.6,
      
      # Fallback
      TRUE ~ 0.2
    ),
    sd_value = case_when(
      condition == "Wet" ~ 0.3,
      circuit == "Monaco" ~ 0.2,
      session == "Q1" & condition == "Dry" ~ 0.15,
      TRUE ~ 0.1
    ),
    # Boost improvement if first stint was on used tyres (and now on new tyres)
    session_effect = session_effect + ifelse(first_tyre_used == "Used", 0.15, 0),
    
    # Simulate second stint
    second_stint_time = first_stint_time - rnorm(n, mean = session_effect, sd = sd_value)
  )
qual_data <- qual_data %>%
  mutate(
    driver = factor(driver),
    session = factor(session),
    circuit = factor(circuit),
    condition = factor(condition),
    tyre_compound = factor(tyre_compound),
    first_tyre_used = factor(first_tyre_used)
  )

# Fit model with tyre factor
model <- lm(second_stint_time ~ first_stint_time + session + driver + circuit + condition + first_tyre_used, data = qual_data)

summary(model)


# Ensure factor levels match those in training data
new_data <- data.frame(
  driver = factor(c("Tsunoda", "Norris", "Russell", "Hulkenberg"), levels = levels(qual_data$driver)),
  session = factor(c("Q3", "Q3", "Q3", "Q3"), levels = levels(qual_data$session)),
  circuit = factor(c("Bahrain", "Bahrain", "Bahrain", "Bahrain"), levels = levels(qual_data$circuit)),
  condition = factor(c("Dry", "Dry", "Dry", "Dry"), levels = levels(qual_data$condition)),
  first_tyre_used = factor(c("Used", "Used", "New", "New"), levels = levels(qual_data$first_tyre_used)),
  tyre_compound = factor(c("Soft", "Soft", "Soft", "Soft"), levels = levels(qual_data$tyre_compound)),
  first_stint_time = c(91.637, 90.396, 90.364, 91.693)
)

# Now predict
new_data$predicted_second_stint <- predict(model, newdata = new_data)

# Print results
print(new_data)


qual_data$predicted <- predict(model, newdata = qual_data)

ggplot(new_data, aes(x = first_stint_time, y = predicted_second_stint, label = driver)) +
  geom_point(color = "blue", size = 3) +
  geom_text(nudge_y = 0.1, size = 4) +
  labs(
    title = "Predicted Second Stint Times",
    x = "First Stint Time (s)",
    y = "Predicted Second Stint Time (s)"
  ) 
