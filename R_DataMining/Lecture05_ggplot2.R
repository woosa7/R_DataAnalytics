############################################################
#
# Data Mining 5 - ggplot2
#
############################################################

# http://docs.ggplot2.org/current/
# C2_plot.R & C5_boxplot_ggplot.R 파일 참조

library(ggplot2)
library(dplyr)

head(iris)


#-----------------------------------------------------------
# Scatter plots

# 1.
x <- ggplot(iris, aes(Sepal.Length, Sepal.Width))
x + geom_point()
x + geom_point(aes(colour = Species))
x + geom_point(aes(colour = Species, size = Petal.Width))

# 2. 
x <- 1:50
y <- sapply(x, function(x) x/(x+1))
df <- data.frame(x, y)
head(df)

ggplot(df, aes(x, y)) + geom_point()

# 3.
# colour 구분은 factor 변환 필요!!!
load("custsig.RData")

gdata <- custsig %>% select(p_trend, visits, buyCount) %>%
    group_by(p_trend) %>% summarise_each(funs(mean), visits, buyCount)

ggplot(gdata, aes(visits, buyCount)) + geom_point(aes(colour = p_trend, size = buyCount))


#-----------------------------------------------------------
# Histograms

x <- ggplot(iris, aes(Sepal.Length))
x + geom_histogram(binwidth = 0.1)
x + geom_histogram(binwidth = 0.1, aes(fill = Species))
x + geom_histogram(fill = "red", alpha = 0.3)


#-----------------------------------------------------------
# Line Charts

# 1.
head(economics)

x <- ggplot(economics)
x + geom_line(aes(x = date, y = unemploy))
x + geom_line(aes(x = date, y = unemploy), colour = "blue", size = 2)

x + geom_line(aes(x = date, y = unemploy), linetype = 2) + 
    geom_line(aes(x = date, y = pce), colour = "#CC79A7")

# 2.
head(Orange)

x <- ggplot(Orange, aes(age, circumference))
x + geom_line(aes(colour = Tree))
x + geom_line(aes(colour = Tree)) + geom_point()

# 3.
ggplot(Orange, aes(age, circumference, colour = Tree)) + geom_line() + geom_point()


#-----------------------------------------------------------
# Bar Charts

# 1.
head(mtcars)
x <- ggplot(mtcars, aes(factor(cyl)))
x + geom_bar(aes(fill = factor(cyl)), width = 0.5)
x + geom_bar(aes(fill = factor(gear)), width = 0.5)
x + geom_bar(aes(fill = factor(gear)), width = 0.5) + coord_flip()

# 2.
x <- ggplot(mtcars, aes(factor(cyl), mpg))
x + geom_bar(aes(fill = factor(cyl)), width = 0.5, stat = "identity")

# 3.
head(diamonds)
summary(diamonds)

x <- ggplot(diamonds, aes(price))
x + geom_bar(aes(fill = cut), binwidth = 3000)
x + geom_bar(aes(fill = cut), binwidth = 3000, position = "dodge")
x + geom_bar(aes(fill = cut), binwidth = 3000, position = "fill")


#-----------------------------------------------------------
# Pie Charts

x <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl)))
x + geom_bar(width = 1) + coord_polar(theta = "y")


#-----------------------------------------------------------
# Box Plots

x <- ggplot(mtcars, aes(factor(cyl), mpg))
x + geom_boxplot()
x + geom_boxplot() + geom_jitter()
x + geom_boxplot(aes(fill = factor(cyl)), outlier.colour = "red", outlier.size = 4)


#-----------------------------------------------------------
# Facets

x <- ggplot(diamonds, aes(price))
x + geom_histogram(bins = 10) + facet_wrap(~ cut)
x + geom_histogram(binwidth = 3000) + facet_wrap(~ cut)
x + geom_histogram(binwidth = 3000) + facet_grid(. ~ cut)
x + geom_histogram(binwidth = 3000) + facet_grid(color ~ cut)


#-----------------------------------------------------------
# Density

head(iris)

x = ggplot(iris, aes(Petal.Length))
x + geom_density()
x + geom_density(aes(color = Species))

