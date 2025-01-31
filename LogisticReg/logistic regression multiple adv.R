# Load necessary packages
library(tidyverse)  # For data manipulation and plotting
theme_set(theme_bw())  # Beautifies plots

# Get the data
d <- carData::TitanicSurvival %>%
  filter(!(is.na(age)))

glimpse(d)

m <- glm(survived ~ sex + age + passengerClass,
         data = d,
         family = binomial
         )
m1 <- mgcv::gam(survived ~ sex + s(age) + passengerClass, d, family = binomial)
plot(m1)

library(performance)
check_model(m1)


library(lme4)
m2 <- glmer(
  survived ~ sex + age + (1|passengerClass),
  data = d,
  family = binomial
)
check_model(m2)

library(ggeffects)
ggeffect(m2)


fancy_plot <- ggeffect(m) %>%
  plot()%>%
  sjPlot::plot_grid()

ggsave(
  "fancy_plot.png",
  plot = fancy_plot,
  device = png,
  dpi = 999,
  width = 8, height = 6
)


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
 

library(flextable)  


fancy_table %>%
  as_flex_table() %>%
  save_as_docx(path = "fancy_table2.docx")

# Save as an image
fancy_table %>%
  as_flex_table() %>%
  save_as_image(path = "fancy_table2.png")

#install.packages("equatiomatic")
library(equatiomatic)
extract_eq(m)


summary(m)


car::Anova(m)

library(randomForest)

rf <- randomForest(survived ~ sex + age + (1|passengerClass), data = d)

installed.packages("vip")
library(vip)
vip(rp)



library(ggstatsplot)


ggbarstats(
  data = d,
  x = passengerClass,
  y = survived,
  lebel = "both"
)


#install.packages("rstatix")
library(rstatix)


contingency_table <- table(d$passengerClass, d$survived)
contingency_table

pairwise_prop_test(contingency_table)


ggbarstats(
  data = d,
  x = passengerClass,
  y = survived,
  label = "both",
  facet.var = sex  # Creates separate bar plots for each sex
)

