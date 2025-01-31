data <- read.csv(file.choose())

data

install.packages("ggplot2")

ggplot2(data=data, aes(x=Height_CM, y=Weight_Kg))+ geom_point()

