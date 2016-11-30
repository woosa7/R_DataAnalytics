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


# recap
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

