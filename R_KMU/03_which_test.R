###########################
# KMU : Lecture
###########################

x <- c(1:10)
y <- c(10:1)

# TRUE 값의 위치(Index)를 정수 또는 배열로 반환

which(x>5)
which(y>5)

which(x[x>5])   # error : 'which'에 전달되는 인자가 논리형이 아닙니다


# ------------------------------------------

set.seed(10)
x <- sample(1:50, 7, replace = F)
x

y <- which(x>20)
x[y]

x[which.max(x)]


x <- c(1,NA,2,NA,3)
which(is.na(x))
which(!is.na(x))[3]


# ------------------------------------------

myVector <- c(-5, -3, -1, 1, 3, 5)

myVector[myVector > 1]
myVector[myVector >= 3]
myVector[!(myVector <= 1)]
myVector[!myVector <= 1]

which(myVector > 1)
myVector[which(myVector > 1)]
myVector[c(FALSE,FALSE,FALSE,FALSE,TRUE,TRUE)]

c(myVector[5], myVector[6])

seq(-5,5,2)[5:6]


# ------------------------------------------

myMatrix <- matrix(1:20, nrow=4, ncol=5)
myMatrix
myMatrix[2:3, 3:4]


age <- 10:20
pay <- 300:310

cbind(age, pay)

a <- rbind(age, pay)
a
rowSums(a)
colSums(a)


# ------------------------------------------
# 다차원 array

myArray <- array(60:1, dim=c(4,5,3))
myArray

shortArray <- array(60:1, dim=c(4,5,2))
shortArray

fourDimArray <- array(1:120, dim=c(4,5,3,2))
fourDimArray


