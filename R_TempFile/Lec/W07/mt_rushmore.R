# install.packages("jpeg")

library(jpeg)

# 이미지를 array로 변환
mtrushBMP <- readJPEG("W07/mount-rushmore-national-memorial.jpg")

str(mtrushBMP)
class(mtrushBMP)
dim(mtrushBMP)
range(mtrushBMP[1, ,1])
max(mtrushBMP)
min(mtrushBMP)

# row, column, 차원
mtrushBMP[100:200,100:200,1] <- 1  # Red     1 = 255 (white)
mtrushBMP[100:200,100:200,2] <- 1  # Green
mtrushBMP[100:200,100:200,3] <- 1  # Blue

# you can save touched image file by way of target keyword of writeJPEG function
writeJPEG(mtrushBMP, target="mtrushJPG.jpg")


# 50번째 행마다 흰색 가로줄 긋기
for (i in 1:dim(mtrushBMP)[1]) {
  if ( i %% 50 == 0) {
    mtrushBMP[i,,1] <- 1
    mtrushBMP[i,,2] <- 1
    mtrushBMP[i,,3] <- 1
  }
}

writeJPEG(mtrushBMP,target="mtrushJPG_x.jpg")


# 세로줄
for (y in 1:dim(mtrushBMP)[2]) {
  if ( y %% 50 == 0) {
    mtrushBMP[,y,1] <- 1
    mtrushBMP[,y,2] <- 1
    mtrushBMP[,y,3] <- 1
  }
}

writeJPEG(mtrushBMP,target="mtrushJPG_y.jpg")


#
for (x in 1:dim(mtrushBMP)[1]) {
  for (y in 1:dim(mtrushBMP)[2]) {
  if ( mtrushBMP[x,y,1] < 10 && mtrushBMP[x,y,2] < 10 && mtrushBMP[x,y,3] < 10) {
    mtrushBMP[x,y,1] <- 1
    mtrushBMP[x,y,2] <- 0
    mtrushBMP[x,y,3] <- 0
    }
  }
}

writeJPEG(mtrushBMP,target="mtrushJPG_z.jpg")

warnings()

