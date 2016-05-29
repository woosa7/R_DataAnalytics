
# 1. RGB convert to number

chocolate <- c(210, 105, 30)

convertRGBtoNumber <- function(x) {
  numbers <- x/255    # RGB 색상은 0~255의 값을 가지므로
  return(numbers)
}

convertRGBtoNumber(chocolate)



# 2. myMatrix의 3열을 벡터와 매트릭스 형태 2가지로 추출

myMatrix <- matrix(1:20, nrow=4)
myMatrix

myMatrixVec <- myMatrix[,3]
myMatrixVec

myMatrixMat <- myMatrix[,3,drop=F]
myMatrixMat



