###############################################
#
#   DataCamp : Introduction to R
#
###############################################

#----------------------------------------------
# Vector
#----------------------------------------------

poker_vector <- c(140, -50, 20, -120, 240)
roulette_vector <- c(-24, -50, -100, 350, 10)

# Naming a vector
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
names(poker_vector) <- days
names(roulette_vector) <- days

poker_vector
roulette_vector

# Calculate
poker_vector + roulette_vector              # total_daily
sum(poker_vector) + sum(roulette_vector)    # total_week

sum(poker_vector) > sum(roulette_vector)

# Vector selection : indexing
poker_vector[3]
poker_vector[2:4]

roulette_vector[-5]
roulette_vector[-length(roulette_vector)]

poker_start <- poker_vector[c("Monday", "Tuesday", "Thursday")]
poker_start
mean(poker_start)

# Selection by comparison
selection <- poker_vector > 0
selection

poker_winning_days <- poker_vector[selection]
poker_winning_days

roulette_winning_days <- roulette_vector[roulette_vector > 0]
roulette_winning_days



#----------------------------------------------
# Matrix
#----------------------------------------------

matrix(1:9, byrow = TRUE, nrow = 3)

# Create a matrix
new_hope <- c(460.998, 314.4)
empire_strikes <- c(290.475, 247.900)
return_jedi <- c(309.306, 165.8)

box_office <- c(new_hope, empire_strikes, return_jedi)    # 그냥 c로 묶으면 vector
box_office

matrix(box_office, nrow = 3, byrow = TRUE)

# cbind / rbind
cbind(new_hope, empire_strikes, return_jedi)
rbind(new_hope, empire_strikes, return_jedi)

# Naming a matrix
star_wars_matrix <- rbind(new_hope, empire_strikes, return_jedi)
colnames(star_wars_matrix) <- c("US", "non-US")
rownames(star_wars_matrix) <- c("A New Hope", "The Empire Strikes Back", "Return of the Jedi")
star_wars_matrix

# Calculate
rowSums(star_wars_matrix)
colSums(star_wars_matrix)

# Adding a column
Total <- rowSums(star_wars_matrix)
star_wars_matrix <- cbind(star_wars_matrix, Total)
star_wars_matrix

# Selection of matrix elements
star_wars_matrix[1,]
star_wars_matrix[,2]
star_wars_matrix[1:2,2]

star_wars_matrix <- star_wars_matrix[,1:2]
star_wars_matrix

visitors <- (star_wars_matrix * 10000) / 5    # 1장당 $5
visitors



#----------------------------------------------
# Factor : categorical variables
#----------------------------------------------

# nominal category
gender <- c("Male", "Female", "Female", "Male", "Male")
factor_gender <- factor(gender)
factor_gender

# ordinal category
temperature <- c("High", "Low", "High","Low", "Medium")
factor_temper <- factor(temperature, order = TRUE, levels = c("Low", "Medium", "High"))
factor_temper

# levels
survey <- c("M", "F", "F", "M", "M")
factor_survey <- factor(survey)
factor_survey

levels(factor_survey) <- c("Female", "Male")  # Levels: F M
factor_survey

summary(survey)
summary(factor_survey)

# Ordered factors
speed <- c("fast","slow","slow","fast","insane")
factor_speed <- factor(speed, order = TRUE, levels = c("slow", "fast", "insane"))
factor_speed

factor_speed[2] > factor_speed[5]



#----------------------------------------------
# Data Frame
#----------------------------------------------

head(mtcars)
str(mtcars)

# Creating a data frame
name <- c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune")
type <- c("Terrestrial", "Terrestrial", "Terrestrial", "Terrestrial", "Gas", "Gas", "Gas", "Gas")
diameter <- c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883)
rotation <- c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67)
rings <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)

planets_df <- data.frame(name, type, diameter, rotation, rings)
head(planets_df)
str(planets_df)

# Selection of data frame elements
planets_df[4,]
planets_df[1:5, "diameter"]

planets_df[planets_df$rings,]
planets_df[planets_df$diameter < 1,]

# Sort
planets_df[order(planets_df$rotation),]



#----------------------------------------------
# List
#----------------------------------------------

# Construct list with different elements
my_vector <- 1:10 
my_matrix <- matrix(1:9, ncol = 3)
my_df <- mtcars[1:10,]

my_list <- list(my_vector, my_matrix, my_df)
names(my_list) <- c("vec", "mat", "df")
my_list

my_list2 <- list(name1 = my_vector, name2 = my_matrix, name3 = my_df)
my_list2

my_list2$year <- c(1990)
