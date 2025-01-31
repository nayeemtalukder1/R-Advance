#Some commonly used datasets include:

#  Auto: Data about different cars, including features like horsepower, weight, and fuel efficiency.
#  Carseats: Sales data for car seats.
#  College: Information about U.S. colleges.
#  Default: Data about credit card defaults.
#  Hitters: Baseball player statistics.
#  OJ: Orange juice purchase data.
#  Portfolio: Data for portfolio analysis.
#  Wage: Wage data for individuals.
library(tidyverse)
library(ISLR) #Introduction to Statistical Learning with Applications in R (ISLR) book

glimpse(Auto)

set.seed(1)

d <- Auto %>% #output of one function as the input to the next function
  sample_n(100)
glimpse(d)

library(ggstatsplot)
ggscatterstats(
  data = d,
  x = mpg,
  y = horsepower,
  
)

ggscatterstats(
  data = d,
  x = mpg,
  y = horsepower,
  type = "parametric"
)


ggscatterstats(
  data = d,
  x = mpg,
  y = horsepower,
  type = "nonparametric"
)

ggscatterstats(
  data = d,
  x = mpg,
  y = horsepower,
  type = "robust"
)
