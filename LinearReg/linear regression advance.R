#Some commonly used datasets include:
  
#  Auto: Data about different cars, including features like horsepower, weight, and fuel efficiency.
#  Carseats: Sales data for car seats.
#  College: Information about U.S. colleges.
#  Default: Data about credit card defaults.
#  Hitters: Baseball player statistics.
#  OJ: Orange juice purchase data.
#  Portfolio: Data for portfolio analysis.
#  Wage: Wage data for individuals.


#install.packages("tidyverse") # %>%
library(tidyverse)
library(ISLR) #Introduction to Statistical Learning with Applications in R (ISLR) book

glimpse(Wage)

set.seed(1)

d <- Wage %>% #output of one function as the input to the next function
  sample_n(1000) %>%
  rename(salar = wage)
d


table(d$jobclass)

m <- lm(salar~jobclass, d)

summary(m)

#install.packages("effects")
library(effects)
library(carData)

plot(allEffects(m))

plot(allEffects(m), grid = TRUE)



#multiple linear

m <- lm(salar ~ jobclass + education, d)

plot(allEffects(m))

plot(predictorEffect(predictor = "education", mod = m))
plot(predictorEffect(predictor = "jobclass", mod = m))


#multiple linearmodel with numaric predictors

m <- lm(salar ~ age + year, d)

plot(allEffects(m))
plot(allEffects(m), confint=list(style="bars"))

#install.packages("sjPlot")
library(sjPlot)
plot_model(m)


m <- lm(salar ~ age + education, d)

plot(allEffects(m))
plot(allEffects(m), confint=list(style="bars"))

#install.packages("sjPlot")
library(sjPlot)
plot_model(m)

plot_model(m, type = "int")+
  theme_classic()+
  theme(legend.position="top")+
  xlab("something difference")



m <- lm(salar ~ education * jobclass, d)

plot(allEffects(m))
plot(allEffects(m), lines = list(multiline = T))

plot(
  allEffects(m), 
  lines = list(multiline = T),
  confint=list(style="auto")
  )

#install.packages("sjPlot")
library(sjPlot)
plot_model(m)
plot_model(m, show.values = T)

#easy post hoc

#install.packages("emmeans")
library(emmeans)

emmeans(m, pairwise ~ jobclass | education, adjust = "fdr")$contrasts

#ggplot
ggplot(d, aes(age, salar))+
  geom_point()+
  geom_smooth()+
  facet_grid(jobclass~health)

#assumption

#install.packages("performance")
library(performance)
check_model(m)
y


# Example dataset
data(mtcars)

# Fit a model
model <- lm(mpg ~ wt + cyl, data = mtcars)

# Check model
library(performance)
library(insight)
library(bayestestR)
library(parameters)

check_model(m)

update.packages("performance")
install.packages(c("insight", "bayestestR", "parameters"))




# Install necessary packages
install.packages(c("performance", "ggplot2", "patchwork"))

# Load libraries
library(performance)
library(ggplot2)
library(patchwork) # For combining plots

# Example dataset
data(mtcars)

# Fit a linear regression model
model <- lm(mpg ~ wt + hp, data = mtcars)

# Assumption checks using the performance package
check_model(model)

# Custom Visualization with ggplot2
# Residual Plot
residuals_plot <- ggplot(data.frame(fitted = fitted(model), residuals = resid(model)), 
                         aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Q-Q Plot
qq_plot <- ggplot(data.frame(sample = resid(model)), aes(sample = sample)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# Combine Plots
residuals_plot + qq_plot

