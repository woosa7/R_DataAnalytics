
simpleAvg <- function(kor, eng, math) {
  dummy <- (kor + eng + math) / 3
  return(dummy)
}

simpleAvg(99, 88, 90)


###########################
# Function
###########################

showMoney <- function(kor, eng, math, pm, warn="다음기회에"){
  
  #testAvg <- (kor + eng + math)/3
  
  testAvg <- simpleAvg(kor, eng, math)
  
  if (testAvg >= 90) {
    pocketMoneyToBe <- pm * 1.1
  } else {
    pocketMoneyToBe <- warn
  }
  
  return(pocketMoneyToBe)
}

showMoney(90, 80, 70, 10000)

showMoney(eng=90, 87, 88, 10000)

#---------------------------------------------------------------------

showMoneyWithPrint <- function(kor, eng, math, pm){
  
  testAvg <- (kor + eng + math)/3
  
  if (testAvg >= 90) {
    pocketMoneyToBe <- pm * 1.1
  } else {
    pocketMoneyToBe <- "please, Try Hard"
  }
  
  print(kor)
  print(eng)
  print(math)
  print(pm)
  
  return(pocketMoneyToBe)
}

showMoneyWithPrint(eng = 80, 90, 70, 10000) #automatically arrange arguments and value.

showMoneyWithPrint(90, 80, 70) # one of argument is missing. error.

#---------------------------------------------------------------------
# When you are implementing a function,
# you can define default value of some arguments.

showMoneyDefault <- function(kor, eng, math, pm=10000){
  
  testAvg <- (kor + eng + math)/3
  
  if (testAvg >=90) {
    pocketMoneyToBe <- pm * 1.1
  } else {
    pocketMoneyToBe <- "please, Try Hard"
  }
  
  cat("kor :", kor, "\n")  # cat means "catalogue", "\n" means making a new line
  cat("eng :", eng, "\n")
  cat("math :", math, "\n")
  cat("pocket Money ;", pm, "\n")
  
  return(pocketMoneyToBe)
}

showMoneyDefault(90, 80, 70)

#---------------------------------------------------------------------

showMoneyWithWarning <- function(kor, eng, math, pm=10000, warn="please try hard!"){
  testAvg <- (kor + eng + math)/3
  if (testAvg >=90) {
    pocketMoneyToBe <- pm * 1.1
  } else {
    pocketMoneyToBe <- warn
  }
  return(pocketMoneyToBe)
}

showMoneyWithWarning(90, 80, 70, "this is your last chance.") # good

showMoneyWithWarning(90, 90, 90, "this is your last chance.") # error. pm 값이 없음.

showMoneyWithWarning(90, 90, 90, warn="this is your last chance.") # good

#---------------------------------------------------------------------
# R is lazy....

showMoneyWithHardWarning(90, 80, 70, "this is your last chance.",10000) # operate but semantic error


showMoneyWithHardWarning(90, 90, 90, "this is your last chance.",10000) # error


showMoneyWithHardWarning(90, 90, 90, warn="this is your last chance.",10000) # good

#---------------------------------------------------------------------

# weighted average : weight kor 20%, eng 30%, math 50% 

showAvg <- function(kor, eng, math){
  testAvg <- kor * 0.2 + eng * 0.3 + math * 0.5 # weighted average
  return(testAvg)
}

# compare the two different result.
# beware the order of arguments
# If you want to change the order of arguments when you call a function,
# you should have to tell it clearly with its argument names

showAvg(eng=90, kor=80, math=70)

showAvg(90, 80, 70) # R interprets this as, kor=90, eng=80, math=70



#########################
# Let's find out Where the objects are.
#########################

install.packages("pryr")
library(pryr)


x <- 3
address(x)
address(3) # error
address(pi)
address

mem_view <- function(x) capture.output(.Internal(inspect(x)))

inspect("a")
.Internal(inspect("a"))

mem_view("a")
mem_view(1)

mem_view("<-")
mem_view("=")
mem_view("[[")

mem_view(simpleAvg)

memoryTour <- mem_view(simpleAvg)

head(memoryTour)

head(memoryTour, 30)

length(memoryTour) # count the length of vectors

NROW(memoryTour) # same as length()
nrow(memoryTour) # this function is not applicable with vectors.

#########################
# Vector
#########################

myVector <- c(1,2,3,4,5)

mem_view(myVector)

myInteger <- 1:5

mem_view(myInteger)

## various ways of making vectors

c(1,2,3,4,5,6,7,8,9,10)
mem_view(1,2,3,4,5,6,7,8,9,10)

x <- c(1,2,3,4,5,6,7,8,9,10)
mem_view(x)

y <- 1:10
mem_view(y)

z <- c(1:10)
mem_view(z)

identical(x, y)

identical(x, z)

identical(y, z)


# sequence -----------------------------------------------------------
seq(1, 10, 2)                 # from, to, by
seq(1, 10000, by = 5)

rep(1:3, 3)

rep(1:3, times=3)

rep(1:3, times=3, each=2)

# sample -----------------------------------------------------------
sample(1:10, 10, replace=TRUE)           # 중복 허용

sample(1:10, 5, replace=FALSE)

set.seed(1)
myVector <- sample(1:10, 5, replace=TRUE)

set.seed(2)
herVector <- sample(1:10, 5, replace=TRUE)

myVector
herVector

setdiff(myVector, herVector)    # 차집합
intersect(myVector, herVector)  # 교집합
union(myVector, herVector)      # 합집합


set.seed(123)
myVector <- sample(1:50, 20, replace=TRUE)

set.seed(345)
herVector <- sample(1:50, 10, replace=TRUE)

myVector
herVector

setdiff(myVector, 1:50) # beware the order of argument and compare the results.
setdiff(1:50, myVector)
?setdiff

sampled <- rep(1:5,1)
sampled
eachSampled <- rep(1:5,1,each=2)
eachSampled

identical(sampled, eachSampled) # not identical
setequal(sampled, eachSampled)  # but two sets has equal unique elements.

seqAlongVector <- seq(1, 20, 2)
length(seqAlongVector)
seq_along(seqAlongVector)


myVector
seq_along(myVector) # making a sequence from 1 to length of input vector. very convenient!!


set.seed(12)
letterTarget <- sample(1:100000, 10000, replace = F)
letterTarget


# Vectorization

X <- 1:16
X
Y <- X + 1
Y
X + Y



