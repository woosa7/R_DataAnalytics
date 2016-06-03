###########################
# KMU : Lecture
###########################

###############
# date / time
###############

thistime <- strptime("20160502 101210", format="%Y%m%d %H%M%S")
thistime
as.numeric(thistime)


difftime(thistime, Sys.time(), unit="hours")
# > difftime(thistime, Sys.time(), unit="hours")
# Time difference of -24.33783 hours

difftime(Sys.time(),thistime, unit="hours", tz="Asia/Seoul")
#difftime(Sys.time(),thistime, unit="hours", tz="America/Dawson")
# Time difference of 24.36624 hours

?difftime  #units = c("auto", "secs", "mins", "hours", "days", "weeks")
example(difftime)

utcthistime <- as.POSIXct(thistime, tz="UTC")
thistime
as.numeric(utcthistime)
as.numeric(thistime)

thistime$hour


difference <- as.numeric(utcthistime) - as.numeric(thistime)
difference/60/60 # 9hours difference

gmtthistime <- as.POSIXct(thistime, tz="GMT")
as.numeric(gmtthistime)

difference <- as.numeric(gmtthistime) - as.numeric(thistime)
difference/60/60 # same 9 hours

tokyothistime <- as.POSIXct(thistime, tz="Asia/Tokyo")
as.numeric(tokyothistime)

difference <- as.numeric(tokyothistime) - as.numeric(thistime)
difference/60/60 # 0 difference

hkthistime <- as.POSIXct(thistime, tz="Asia/Hong_kong")
as.numeric(hkthistime)

difference <- as.numeric(hkthistime) - as.numeric(thistime)
difference/60/60 # 1 hour difference





###############
# get / assign
###############

# 계산된 결과를 변수에 넣는다.
for (i in 1:10) {
  set.seed(i)
  assign(paste0("obj_",i), sample(1:10, 7, replace=F))
}

# 변수에 있는 결과값을 하나로 합친다.
objdf <- NULL
for (i in 1:10) {
  objdf <- rbind(objdf, get(paste0("obj_",i)))
}

objdf


###############
# join
###############
example(merge)
example(match)

match(c(255,11,22),c(255,11,33))
match(c(255,11,22),c(11,33, 255))

?match

match(1:10, 7:20) #[1] 0 0 0 0 0 0 1 2 3 4
match(10:1, 7:20) #[1]  4  3  2  1 NA NA NA NA NA NA
match(1:10, 7:20, nomatch=0)
max(match(1:10, 7:20, nomatch=0))
intersect <- function(x, y) y[match(x, y, nomatch = 0)]
intersect(1:10, 7:20)

intersect_nomatch_1 <- function(x, y) y[match(x, y, nomatch = 1)]
intersect_nomatch_1(1:10, 7:20)

intersect_nomatch_NA <- function(x, y) y[match(x, y, nomatch = NA)]
intersect_nomatch_NA(1:10, 7:20)

intersect_nomatch_ln <- function(x, y) y[match(x, y, nomatch = length(y))]
intersect_nomatch_ln(1:10, 7:20)

intersect_nomatch_asterisk <- function(x, y) y[match(x, y, nomatch = "*")]
intersect_nomatch_asterisk(1:10, 7:20)

df1 = data.frame(CustomerId = c(1:6), Product = c(rep("Toaster", 3), rep("Radio", 3)))
df2 = data.frame(CustomerId = c(2, 4, 6), State = c(rep("Alabama", 2), rep("Ohio", 1)))

df1
df2

# default merge is inner join
merge(x = df1, y = df2)
merge(x = df2, y = df1)

# Outer join (== full outer join) : 
merge(x = df1, y = df2, by = "CustomerId", all = TRUE)
merge(x = df2, y = df1, by = "CustomerId", all = TRUE)

# Left outer: 
merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)
merge(x = df2, y = df1, by = "CustomerId", all.x = TRUE)

# Right outer: 
merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)
merge(x = df2, y = df1, by = "CustomerId", all.y = TRUE)

# Cross join: 
merge(x = df1, y = df2, by = NULL)
merge(x = df2, y = df1, by = NULL)

# install.packages("plyr")
library(plyr)
?join

# default plyr::join is left outer join
join(df1, df2)
join(df2, df1)

# inner join
join(df1, df2, type="inner")
join(df2, df1, type="inner")

# right outer join
join(df1, df2, type="right")
join(df2, df1, type="right")

# full outer join
join(df1, df2, type="full")
join(df2, df1, type="full")


###############
# data handling
###############

dummy <- dir()
dumwhich <- which(substr(dummy, nchar(dummy)-2, nchar(dummy)) == "csv")   # csv 파일 체크
loadlist <- dummy[dumwhich]

for (i in seq_along(loadlist)) {
  assign(paste0("bikedata_",i), read.csv(loadlist[i], stringsAsFactors = F))
}

head(bikedata_1,1)
head(bikedata_2,1)
head(bikedata_3,1)

for (i in 1:3) {
  print(head(get(paste0("bikedata_",i)),1))
}

all.equal(names(bikedata_1), names(bikedata_2))
all.equal(names(bikedata_2), names(bikedata_3))
?all.equal

head(rbind(bikedata_1, bikedata_2))
head(rbind(bikedata_2, bikedata_3)) #error


# version 1
for (i in 1:3) {
  for (j in 1:3) {
    cat("bikedata_",i, " compared with ", " bikedata", j, rep("\n",2))
    print(all.equal(names(get(paste0("bikedata_",i))), names(get(paste0("bikedata_",j)))))
    cat(rep("\n",1))
  }
}

# version 2
for (i in 1:3) {
  for (j in 1:3) {
    if (i == j) {
      next
    } else {
      cat("bikedata_",i, " compared with ", " bikedata", j, rep("\n",2))
      print(all.equal(names(get(paste0("bikedata_",i))), names(get(paste0("bikedata_",j)))))
      cat(rep("\n",1))
    }
  }
}

# version 3
namediff <- 0
for (i in 1:3) {
  for (j in 1:3) {
    if (i == j) {
      next
    } else {
      cat("bikedata_",i, " compared with ", " bikedata", j, rep("\n",1))
      booldum <- all.equal(names(get(paste0("bikedata_",i))), names(get(paste0("bikedata_",j))))
      if (booldum == TRUE) {
        cat("all equal",rep("\n",2))
      } else {
        cat("minimum 1 elements are different",rep("\n",2)) 
      }
    }
  }
}

