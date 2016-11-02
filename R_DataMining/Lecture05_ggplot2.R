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
x + geom_point(aes(colour = Species, size = Petal.Width))   # colour 구분은 factor 필요!!!

# 2. 
x <- 1:50
y <- sapply(x, function(x) x/(x+1))
df <- data.frame(x, y)
head(df)

ggplot(df, aes(x, y)) + geom_point()

# 3.
head(diamonds)

ggplot(diamonds, aes(x = carat, y = price)) + geom_point(aes(color = color))


#-----------------------------------------------------------
# Histograms

# 1
x <- ggplot(iris, aes(Sepal.Length))
x + geom_histogram(binwidth = 0.1)
x + geom_histogram(binwidth = 0.1, aes(fill = Species))
x + geom_histogram(fill = "red", alpha = 0.3)

# 2
ggplot(diamonds) + geom_histogram(aes(x = carat))

ggplot(diamonds) + geom_density(aes(x = carat), fill = "pink")


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

ggplot(Orange, aes(age, circumference, colour = Tree)) + geom_line() + geom_point()

# 3.
library(lubridate)

eco = economics[which(year(economics$date) >= 2000), ]   # 2000년 이후 데이터만 추출
head(eco)
summary(eco)

eco$year = factor(year(eco$date))           # year
eco$month = month(eco$date, label = T)      # month name

g = ggplot(eco, aes(x = month, y = pop/1000))
g + geom_line(aes(color = year, group = year)) + labs(title = "Population Growth", x = "Month", y = "Population")



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
# Box / Violin Plots

# 1
x <- ggplot(mtcars, aes(x = factor(cyl), y = mpg))
x + geom_boxplot()
x + geom_boxplot() + geom_jitter()
x + geom_boxplot(aes(fill = factor(cyl)), outlier.colour = "red", outlier.size = 4)

# 2
ggplot(diamonds, aes(x = 1, y = carat)) + geom_boxplot()        # x = 1 : 전체 데이터

ggplot(diamonds, aes(x = cut, y = carat)) + geom_boxplot()      # x = factor

ggplot(diamonds, aes(x = cut, y = carat)) + geom_violin()

ggplot(diamonds, aes(x = cut, y = carat)) + geom_violin() + geom_point()


#-----------------------------------------------------------
# Facets

head(diamonds)
summary(diamonds)

# 1
x <- ggplot(diamonds, aes(price))
x + geom_histogram(bins = 10) + facet_wrap(~ cut)
x + geom_histogram(binwidth = 3000) + facet_wrap(~ cut)
x + geom_histogram(binwidth = 3000) + facet_grid(. ~ cut)
x + geom_histogram(binwidth = 3000) + facet_grid(color ~ cut)

# 2
d = ggplot(diamonds, aes(x = carat, y = price)) 
d + geom_point(aes(color = color))
d + geom_point(aes(color = color)) + facet_wrap(~color)
d + geom_point(aes(color = color)) + facet_grid(cut ~ clarity)


#-----------------------------------------------------------
# Density

head(iris)

x = ggplot(iris, aes(Petal.Length))
x + geom_density()
x + geom_density(aes(color = Species))

