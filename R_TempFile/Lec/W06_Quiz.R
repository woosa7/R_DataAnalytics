str_vector <- c("hello,", "me","?", "it", "looking", "is", "you", "for")
str_vector
factorial(8)

output <- NULL
outputAvg <- NULL
temp_vector <- NULL

for (k in 1:3) {
  
  for (j in 1:100) {
    
    set.seed(j)
    
    for (i in 1:100000) {
      dummy_str <- str_vector[sample(1:8,8,replace=F)]
      
      if(sum( dummy_str == temp_vector) != 8) {
        if(sum( dummy_str == str_vector[c(1,6,4,2,7,5,8,3)]) == 8) {
          cat(k, " : ", j," : ", i, "\n")
          output <- c(output, i)
          break
        }
      }
      
    }
  }

  outputAvg <- c(outputAvg, mean(output))  
}

length(output)

outputAvg

hist(outputAvg)

mean(outputAvg)
