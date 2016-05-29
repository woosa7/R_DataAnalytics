# Quiz 1

# 1
rep(1:4, times=3, each=2)

# 2-1
set.seed(12345)
km01 <- sample(1:30000, 1000, replace=FALSE)

# 2-2
set.seed(54321)
km02 <- sample(1:30000, 1000, replace=FALSE)

# 2-3 중복 검사
intersect(km01, km02)




