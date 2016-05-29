####################
# vector indexing
####################

# making vectors

set.seed(30)
a <- sample(1:10)


smpl_vector <- seq(1,20,2)
smpl_vector
mode(smpl_vector)

smpl_vector[1]
smpl_vector[3]
smpl_vector[length(smpl_vector)]
smpl_vector[3:length(smpl_vector)]
smpl_vector[3:length(smpl_vector)-1]
smpl_vector[3:(length(smpl_vector)-1)]

?isTRUE
char_vector <- LETTERS
char_vector
mode(char_vector)

-1:5
1:5%%5
1:5^2


for (i in char_vector) {
  print(i)
}

for (i in char_vector) {
  cat(i)
}

for (i in char_vector) {
  cat(i, "\n")
}

bool_vector <- c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)
bool_vector
mode(bool_vector)
sum(bool_vector) # count the number of TRUEs


str_vector <- c("hello,", "me","?", "it", "looking", "is", "you", "for")
str_vector
factorial(8)

(start.time <- Sys.time())
for (i in 1:100000) {
  set.seed(i)
  dummy_str <- str_vector[sample(1:8,8,replace=F)]
  if(sum( dummy_str == str_vector[c(1,6,4,2,7,5,8,3)]) == 8) {
    print(i)
    print(dummy_str)
  }
}
Sys.time() - start.time



"me" %in% str_vector          # vector 내에 해당 값 존재여부 확인

3^2          # 제곱
3^(1/2)      # 제곱근




str_vector[c(1,6,4,2,7,5,8,3)]
mode(str_vector)



na_vector <- rep(NA, 7)
na_vector
mode(na_vector)

null_vector <- NULL
null_vector
mode(null_vector)

null_vector2 <- c()
null_vector2

# how to count vector's number of elements?
length(smpl_vector)
length(char_vector)
length(str_vector)
length(bool_vector)

# Boolean retrieval

str_vector <- c("hello,", "me","?", "it", "looking", "is", "you", "for")
bool_vector <- c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)
bool_vector
mode(bool_vector)
sum(bool_vector) # count the number of TRUEs

str_vector[bool_vector]

# application of boolean retrieval : comparing conditions

set.seed(25)
myVector <- sample(1:20, 10, replace=T)
myVector
myVector[myVector >= 10]
conditionCompare <- myVector >= 10      # T / F 벡터로 변환.
conditionCompare
myVector[conditionCompare]


# application of positive integer retrieval : which()

which(myVector >= 10)
myVector[which(myVector >= 10)]


# sigmoid function
x <- seq(-20,20,0.1)
length(x)
sigmoid <- exp(x)/(1+exp(x))
plot(x, sigmoid)

# ------------------------------------------------------------------

x <- seq(-5, 5, 0.1)
y <- exp(x)

plot(x, y)

sigmoid <- exp(x)/(1+exp(x))
plot(x, sigmoid)


z <- exp(9^(1/5)) / (1+exp(9^(1/5)))
z

# 파일리스트 중 끝에 "R" 인 파일명만 출력
getwd()
dirlist <- dir()
dirlist
nchar(dirlist)

substr(dirlist[3], nchar(dirlist[3]), nchar(dirlist[3]))  # 시작위치, 끝위치

endIndex <- substr(dirlist, nchar(dirlist), nchar(dirlist)) == "R"
endIndex
dirlist[endIndex]


str_vector <- c("1", "2", "3", "4")
str_vector2 <- c("5", "6", "7", "9")

paste(str_vector, str_vector2)
paste(str_vector, str_vector2, sep = "*")
paste(str_vector, str_vector2, collapse = " * ")



