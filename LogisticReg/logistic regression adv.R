# Load necessary packages
library(tidyverse)  # For data manipulation and plotting
theme_set(theme_bw())  # Beautifies plots

# Get the data
d <- carData::TitanicSurvival %>%
  rename(class = passengerClass)

# Cross table for quick intuition
table(d$survived, d$class)

# Run logistic regression with categorical predictor
m <- glm(survived ~ class, d, family = binomial)

# Plot predictions
library(sjPlot)
plot_model(m, type = "eff", terms = c("class"))

# Save your plot
ggsave("fancy_plot.png",
       plot = last_plot(),
       device = png,
       dpi = 999,
       width = 5, height = 3)

# Avoid using summary()
summary(m)

# Get probabilities and odds ratios
library(emmeans)
emmeans(m, pairwise ~ class, type = "response", infer = TRUE)

# Subset the data
d2 <- d %>%
  slice(1:400)

# Fit a logistic regression model
m2 <- glm(survived ~ class, d2, family = binomial)

# Get estimated marginal means and probabilities
emmeans(m2, ~ class, infer = T, type = "response")

# Reverse odds ratios if needed
emmeans(m, ~ class, type = "response") %>%
  pairs(reverse = T, infer = T)
#install.packages("gtsummary")
# Create a publication-ready table
library(gtsummary)  # For creating summary tables

fancy_table <- tbl_regression(
  m, 
  exponentiate = T, 
  add_pairwise_contrasts = T
) %>%
  # Customize the table
  add_significance_stars(
    hide_p = F, 
    hide_se = T, 
    hide_ci = F
  ) %>%
  bold_p()

# Display the table
fancy_table

# Export your table
library(flextable)  # For flexible and styled table outputs

# Save as a Word document
fancy_table %>%
  as_flex_table() %>%
  save_as_docx(path = "fancy_table.docx")

# Save as an image
fancy_table %>%
  as_flex_table() %>%
  save_as_image(path = "fancy_table.png")
