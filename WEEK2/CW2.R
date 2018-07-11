library(ggplot2)

cars
ggplot(data=cars, aes(x=speed, y=dist)) + geom_point()

ggplot(data=cars, aes(x=speed, y=dist)) + geom_line()

ggplot(data=cars,aes(x=speed))+
  geom_bar(fill="lightblue",colour="black")

ggplot(data=cars,aes(x=speed))+
  geom_histogram()