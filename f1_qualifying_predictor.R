# F1 Qualifying Predictor in R
# Author: Cesare Pesci
# This script predicts second stint lap times based on driver, session, circuit, weather, and tyre condition

library(dplyr)
library(ggplot2)

set.seed(123)
n <- 300

drivers <- c("Piastri", "Norris", "Hamilton", "Leclerc", "Russell", "Antonelli", "Verstappen", "Tsunoda",
             "Alonso", "Stroll", "Gasly", "Doohan", "Albon", "Sainz", "Ocon", "Bearman", "Hulkenberg", 
             "Bortoleto", "Lawson")
sessions <- c("Q1", "Q2", "Q3")
circuits <- c("Australia", "China", "Monaco", "Bahrain", "Jeddah", "Miami", "Imola", "Spain", "Silverstone", 
              "Monza", "Canada", "Austria", "Japan", "Singapore", "Spa", "Hungary", "Netherlands", "Baku", 
              "Austin", "Mexico", "Brazil", "Las Vegas", "Qatar", "Abu Dhabi")
conditions <- c("Dry", "Wet")
tyres <- c("New", "Used")

qual_data <- data.frame(
  driver = sample(drivers, n, replace = TRUE),
  session = sample(sessions, n, replace = TRUE),
  circuit = sample(circuits, n, replace = TRUE),
  condition = sample(conditions, n, replace = TRUE),
  first_tyre_used = sample(tyres, n, replace = TRUE),
  first_stint_time = rnorm(n, mean = 91, sd = 1)
)

qual_data <- qual_data %>%
  mutate(
    session_effect = case_when(
      session == "Q1" & condition == "Dry" & circuit != "Monaco" ~ 0.4,
      session == "Q2" & condition == "Dry" & circuit != "Monaco" ~ 0.3,
      session == "Q3" & condition == "Dry" & circuit != "Monaco" ~ 0.25,
      condition == "Wet" ~ 0.4,
      circuit == "Monaco" ~ 0.6,
      TRUE ~ 0.2
    ),
    sd_value = case_when(
      condition == "Wet" ~ 0.3,
      circuit == "Monaco" ~ 0.2,
      session == "Q1" & condition == "Dry" ~ 0.15,
      TRUE ~ 0.1
    ),
    session_effect = session_effect + ifelse(first_tyre_used == "Used", 0.15, 0),
    second_stint_time = first_stint_time - rnorm(n, mean = session_effect, sd = sd_value)
  ) %>%
  mutate(
    driver = factor(driver),
    session = factor(session),
    circuit = factor(circuit),
    condition = factor(condition),
    first_tyre_used = factor(first_tyre_used)
  )

model <- lm(second_stint_time ~ first_stint_time + session + driver + circuit + condition + first_tyre_used, data = qual_data)

# Example prediction
new_data <- data.frame(
  driver = factor(c("Hamilton", "Albon", "Alonso"), levels = levels(qual_data$driver)),
  session = factor(c("Q1", "Q1", "Q3"), levels = levels(qual_data$session)),
  circuit = factor(c("Monaco", "Silverstone", "Spa"), levels = levels(qual_data$circuit)),
  condition = factor(c("Dry", "Wet", "Dry"), levels = levels(qual_data$condition)),
  first_tyre_used = factor(c("New", "Used", "Used"), levels = levels(qual_data$first_tyre_used)),
  first_stint_time = c(91.8, 91.3, 91.45)
)

new_data$predicted_second_stint <- predict(model, newdata = new_data)

print(new_data)

ggplot(new_data, aes(x = first_stint_time, y = predicted_second_stint, label = driver)) +
  geom_point(color = "blue", size = 3) +
  geom_text(nudge_y = 0.1, size = 4) +
  labs(
    title = "Predicted Second Stint Times",
    x = "First Stint Time (s)",
    y = "Predicted Second Stint Time (s)"
  )
