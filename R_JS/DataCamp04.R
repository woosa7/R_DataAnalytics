###############################################
#
#   DataCamp : dplyr
#
###############################################

# R_DataMining / Lecture02.R 참조

#----------------------------------------------
# tbl - special type of data.frame
#----------------------------------------------

library(dplyr)
library(hflights)

# Houston flights data
head(hflights)
summary(hflights)
dim(hflights)

# data.frame into tbl
fdata = tbl_df(hflights)
fdata

# Changing labels
unique(fdata$UniqueCarrier)

lut <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", 
         "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", 
         "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", 
         "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")
fdata$Carrier <- lut[fdata$UniqueCarrier]
glimpse(fdata)


#----------------------------------------------
# Select
#----------------------------------------------

# starts_with("X")      : every name that starts with "X",
# ends_with("X")        : every name that ends with "X",
# contains("X")         : every name that contains "X",
# matches("X")          : every name that matches "X", where "X" can be a regular expression,
# num_range("x", 1:5)   : the variables named x01, x02, x03, x04 and x05,
# one_of(x): every name that appears in x, which should be a character vector.

names(fdata)

select(fdata, ActualElapsedTime, AirTime, ArrDelay, DepDelay)

select(fdata, Origin:Cancelled)

select(fdata, -c(5:11))

select(fdata, 1:6, -3)

select(fdata, ends_with("Delay"))

select(fdata, starts_with("Taxi"), contains("Ca"))

vars = c("Origin", "Dest", "Distance")
select(fdata, one_of(vars))


#----------------------------------------------
# Mutate
#----------------------------------------------

fdata = mutate(fdata, AverageSpeed = Distance / AirTime * 60)
select(fdata, FlightNum, TailNum, AverageSpeed)

fdata = mutate(fdata, TotalTaxi = TaxiIn + TaxiOut, 
               ActualGroundTime = ActualElapsedTime - AirTime,
               Diff = TotalTaxi - ActualGroundTime)
select(fdata, TotalTaxi, ActualGroundTime, Diff)


#----------------------------------------------
# Filter / Arrange
#----------------------------------------------

filter(fdata, Distance >= 3000)

filter(fdata, Carrier %in% c("JetBlue", "Southwest", "Delta"))

filter(fdata, (TaxiOut + TaxiIn) > AirTime)

filter(hflights, DepTime < 500 | ArrTime > 2200 )   # departed before 5am or arrived after 10pm

filter(hflights, DepDelay > 0 & ArrDelay < 0)       # departed late but arrived ahead of schedule

filter(hflights, DepDelay > 0, Cancelled == 1)      # cancelled after being delayed

jfk = filter(fdata, Dest == "JFK") %>% 
    mutate(Date = paste(Year, Month, DayofMonth, sep = "-")) %>% 
    select(Date, DepTime, ArrTime, TailNum, Dest)
jfk

arrange(jfk, TailNum)
arrange(jfk, desc(DepTime))


#----------------------------------------------
# summarise
#----------------------------------------------

summarise(fdata, min_dist = min(Distance), max_dist = max(Distance))

summarise(filter(hflights, Diverted == 1), max_div = max(Distance))

summarise(fdata,
          n_obs = nrow(fdata),
          n_carrier = n_distinct(UniqueCarrier),
          n_dest = n_distinct(Dest))

aa <- filter(fdata, UniqueCarrier == "AA")
summarise(aa, n_flights = n(),
          n_canc = sum(Cancelled),
          avg_delay = mean(ArrDelay, na.rm = TRUE))


#----------------------------------------------
# pipe : %>% 
#----------------------------------------------

hflights %>% mutate(diff = TaxiOut - TaxiIn) %>%
    filter(!is.na(diff)) %>%
    summarise(avg = mean(diff))

hflights %>% 
    mutate(RealTime = ActualElapsedTime + 100, mph = Distance / RealTime * 60) %>% 
    filter(!is.na(mph), mph < 70) %>% 
    summarise(n_less = n(), n_dest = n_distinct(Dest), 
              min_dist = min(Distance), max_dist = max(Distance))

hflights %>%
    mutate(RealTime = ActualElapsedTime + 100, mph = Distance / RealTime * 60) %>% 
    filter(mph < 105 | Cancelled == 1 | Diverted == 1) %>% 
    summarise(n_non = n(), n_dest = n_distinct(Dest),
              min_dist = min(Distance), max_dist = max(Distance))

# overnight flights
hflights %>%
    filter(!is.na(DepTime), !is.na(ArrTime), DepTime > ArrTime) %>%
    summarise(num = n())


#----------------------------------------------
# group_by
#----------------------------------------------

fdata %>% 
    group_by(Carrier) %>% 
    summarise(p_canc = mean(Cancelled == 1)*100, avg_delay = mean(ArrDelay, na.rm = T)) %>% 
    arrange(avg_delay, p_canc)

fdata %>% 
    filter(!is.na(ArrDelay), ArrDelay > 0) %>% 
    group_by(Carrier) %>% 
    summarise(avg = mean(ArrDelay)) %>% 
    mutate(rank = rank(avg)) %>% 
    arrange(rank)

# How many airplanes only flew to one destination?
fdata %>%
    group_by(TailNum) %>%
    summarise(ndest = n_distinct(Dest)) %>%
    filter(ndest == 1) %>%
    summarise(nplanes = n())

# Find the most visited destination for each carrier
fdata %>%
    group_by(Carrier, Dest) %>%
    summarise(n = n()) %>%
    mutate(rank = rank(desc(n))) %>%    # ranks per carrier
    filter(rank == 1)


#----------------------------------------------
# data.table
#----------------------------------------------

library(data.table)

hflights2 <- as.data.table(hflights)

hflights2 %>% summarise(n_carrier = n_distinct(UniqueCarrier))

# mySQL
my_db <- src_mysql(dbname = "dplyr", 
                   host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                   port = 3306, 
                   user = "student",
                   password = "datacamp")

nycflights <- tbl(my_db, "dplyr")


#----------------------------------------------
# Join
#----------------------------------------------

left_join()
right_join()
inner_join()
full_join()

semi_join(x, y, by)   # y와 match되는 x의 모든 row. x의 컬럼만 유지.
anti_join(x, y, by)   # y에 없는 x의 모든 row. x의 컬럼만 유지.

by = "key"
by = c("first", "last")


# x, y에 모두 name 컬럼이 있는데, 서로 다른 값인 경우.
x %>% left_join(y, by = "key") %>% 
    rename(artist = name.x, studio = name.y)


# mismatched key names
# x, y의 키 컬럼명이 다른 경우
x %>% left_join(y, by = c("movie" = "name")) %>% 
    rename(artist = name)


# join multi tables
library(purrr)

list(x, y, z) %>% reduce(left_join)

list(x, y, z) %>% reduce(semi_join)   # x의 row만 출력



#----------------------------------------------
# Practice - Lahman Baseball 
#----------------------------------------------

library(Lahman)

str(Master)
str(Salaries)
str(Appearances)
str(Batting)
str(HallOfFame)

players <- Master %>% 
    select(playerID, nameFirst, nameLast) %>% 
    distinct()

head(players)

#----------------------------------------------
# Find all players who do not appear in Salaries
players %>% 
    anti_join(Salaries, by = "playerID") %>%
    count()

#----------------------------------------------
# How many unsalaried players appear in Appearances?
players %>% 
    anti_join(Salaries, by = "playerID") %>% 
    semi_join(Appearances, by = "playerID") %>% 
    count()

#----------------------------------------------
# how many games each of these unsalaried players played?
players %>% 
    anti_join(Salaries, by = "playerID") %>% 
    left_join(Appearances, by = "playerID") %>% 
    group_by(playerID) %>%
    summarise(total_games = sum(G_all)) %>%
    arrange(desc(total_games))

#----------------------------------------------
# determine if the players had an at-bat (i.e. batted) in the games that they appeared in.
players %>%
    anti_join(Salaries, by = "playerID") %>% 
    left_join(Batting, by = "playerID") %>% 
    group_by(playerID, nameFirst, nameLast) %>%
    summarise(total_games = sum(AB)) %>%
    arrange(desc(total_games))

#----------------------------------------------
# Find the distinct players that appear in HallOfFame
nominated <- HallOfFame %>% distinct(playerID)

nominated_full <- nominated %>% 
    left_join(Master, by = "playerID") %>% 
    filter(inducted == "Y") %>% 
    select(playerID, nameFirst, nameLast)

head(nominated_full)


#----------------------------------------------
# Awards
# Did nominees who were inducted earn more awards than nominees who were not inducted?

# Hall of fame inductions
inducted <- HallOfFame %>% 
    filter(inducted == "Y") %>% 
    distinct(playerID)

head(inducted)

# Tally the number of awards in AwardsPlayers by playerID
nAwards <- AwardsPlayers %>% 
    group_by(playerID) %>% 
    tally()

nAwards

# players in inducted 
nAwards %>% 
    semi_join(inducted, by = "playerID") %>% 
    summarize(avg_n = mean(n, na.rm = TRUE))

# players in nominated, NOT in inducted 
nAwards %>% 
    semi_join(nominated, by = "playerID") %>%
    anti_join(inducted, by = "playerID") %>% 
    summarize(avg_n = mean(n, na.rm = TRUE))


#----------------------------------------------
# Salary
# Does the maximum salary earned by inductees tend to be 
# greater than the maximum salary earned by nominees who were not inducted?

# Find the players who are in nominated, but not inducted
notInducted <- nominated %>% 
    setdiff(inducted)

# not Inducted
Salaries %>% 
    semi_join(notInducted, by = "playerID") %>%
    group_by(playerID) %>% 
    summarize(max_salary = max(salary, na.rm = TRUE)) %>% 
    summarize(avg_salary = mean(max_salary, na.rm = TRUE))

# inducted
Salaries %>% 
    semi_join(inducted, by = "playerID") %>% 
    group_by(playerID) %>% 
    summarize(max_salary = max(salary, na.rm = TRUE)) %>% 
    summarize(avg_salary = mean(max_salary, na.rm = TRUE))


#----------------------------------------------
# Retirement
# players cannot be nominated until five years after they retire. 
# Is this reflected in our data?

nominated <- HallOfFame %>% distinct(playerID)

Appearances %>% 
    semi_join(nominated, by = "playerID") %>% 
    group_by(playerID) %>% 
    summarize(last_year = max(yearID)) %>% 
    left_join(HallOfFame, by = "playerID") %>% 
    filter(last_year >= yearID)
