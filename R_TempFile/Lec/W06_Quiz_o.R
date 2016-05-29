str_vector <- c("hello,", "me","?", "it", "looking", "is", "you", "for")
str_vector
factorial(8)

(start.time <- Sys.time())
output <- NULL
for (j in 1:100) {
  for (i in 1:100000) {
    dummy_str <- str_vector[sample(1:8,8,replace=F)]
    if(sum( dummy_str == str_vector[c(1,6,4,2,7,5,8,3)]) == 8) {
      cat(j, " : ", i, "\n")
      output <- c(output, i)
      break
    }
  }
}
Sys.time() - start.time
mean(output)


length(output)

#------------------------------------------------------------------------

paste(c("X","Y"), 1:10, sep="")

c("x","y")[rep(c(1,2,2,1), times=4)]

fruit <- c(5, 10, 1, 20)
names(fruit) <- c("orange", "banana", "apple", "peach")
lunch <- fruit[c("apple","orange")]
lunch

fruit

is.na(NA)
is.na(NaN)
is.na(c("NA", NA, NaN))
is.na(c(NaN, NA))

x <- c(10.4, 5.6, 3.1, 6.4, 21.7)
y <- c(x,0,x)
y








