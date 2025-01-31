#Data visualization
?ChickWeight
plot(ChickWeight)

#base graphics
library(MASS)
View(UScereal)
plot(UScereal$sugars,UScereal$calories)
title("plot(Suger,calories)")

#grid graphics

#pie chart

x <- c(23,24,26,28)
labels <- c("ball","bat","stamp","weiket")

pie(x,labels, main="Pie chart", col= rainbow(length(x)))

piepercent <- round(100*x/sum(x), 1)

pie(x,labels=piepercent, main="Pie chart", col= rainbow(length(x)))
legend("topright", c("ball","bat","stamp","weiket"), cex=0.8, fill= rainbow(length(x)))
