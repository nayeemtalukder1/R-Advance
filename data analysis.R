library(MASS)
View(UScereal)

#data analysis
df <-UScereal[UScereal$vitamins == "enriched",]
df
df <- UScereal[UScereal$fat > 2,]
df
df <- UScereal[UScereal$fat > 2,c(1,3,6)]
df
df <- UScereal[UScereal$fat > 2& UScereal$vitamins == "enriched",]
df

#create a subset
df <- subset(UScereal,UScereal$fat > 2& UScereal$vitamins == "enriched",)
df
#sorting data frames
df <- UScereal[order(UScereal$sugars),]
df
#statistical analysis
##Range of income

min(UScereal$sugars)
max(UScereal$sugars)
range(UScereal$sugars)
mean(UScereal$sugars)
sd(UScereal$sugars)
var(UScereal$sugars)
mad(UScereal$sugars)

quantile(UScereal$sugars)
median(UScereal$sugars)
IQR(UScereal$sugars)

#Histogram - count of income ranges
library(ggplot2)
ggplot()+geom_histogram(data=UScereal,aes(x=sodium), bins = 30, binwidth = 100)

ggplot()+geom_histogram(data=UScereal,aes(x=sodium), fill = "blue",color="black", bins = 30, binwidth = 100)

ggplot()+geom_histogram(data=UScereal,aes(x=sodium), fill = "blue",color="black", bins = 30, binwidth = 100) + facet_grid(UScereal$vitamins)
                                        