# Microsoft: DAT101x Data Science Orientation

lemonade <- read.csv("Lemonade2016.csv", header = T)
lemonade

attach(lemonade)

summary(lemonade)
sum(Sales)
sum(Revenue)

hist(Sales)
hist(Revenue)
