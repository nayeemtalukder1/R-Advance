library(ggstatsplot)


ggbarstats(
  data = mtcars,
  x = am,
  y = cyl,
  lebel = "both"
)


install.packages("rstatix")
library(rstatix)


contingency_table <- table(mtcars$cyl, mtcars$am)
contingency_table

pairwise_prop_test(contingency_table)
