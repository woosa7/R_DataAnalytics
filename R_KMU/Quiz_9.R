# 각 문서마다 20개의 토픽이 들어있는 비율. Term-Document matrix
load("data/smpl_lda_df.RData")
df <- smpl_lda_df
head(df)
dim(df)

# 1. 확률이 0 보다 큰 토픽의 갯수 리턴
fCountTopic <- function(x) {
    tPositions <- which(x>0)          # 확률이 0 이상인 컬럼의 위치
    return( length(tPositions) )      # position 갯수 리턴
}


# 2. 확률이 0 보다 큰 토픽의 토픽명 리턴
fTopicNames <- function(x) {
    tPositions <- which(x>0)                    # 확률이 0 이상인 컬럼의 위치
    tNames <- rownames(as.matrix(tPositions))   # 해당 postion의 데이터를 matrix로 변환 후 rowname(토픽명) 확인
    return( paste(tNames, collapse=" ") )       # 토픽명 여러개인 경우 합치기
}


# 각 문서별 토픽의 갯수와 해당 토픽명 표시.
df$Ntopic <- apply(df[,1:20], 1, fCountTopic)
df$Tname <- apply(df[,1:20], 1, fTopicNames)
answer <- df[21:22]
head(answer)

# write.csv(answer, file = "Quiz09.csv", row.names = T)


