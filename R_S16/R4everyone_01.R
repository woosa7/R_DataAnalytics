###############################################################
#
# R for Everyone 01
#
###############################################################

#--------------------------------------------------------------
# 문자열 추출 : stringr 패키지
#--------------------------------------------------------------

# 웹페이지에서 미국 대통령 테이블 받기

library(XML)

url = "http://www.loc.gov/rr/print/list/057_chron.html"
prData = readHTMLTable(url, which = 3, as.data.frame = T, skip.rows = 1, header = T, stringsAsFactors = F)

head(prData)
tail(prData)

prData = prData[1:64, ]   # 65 행부터 다른 정보를 가지고 있기 때문에 제거


library(stringr)   # 문자열 처리 패키지

# 0000 - 0000 형태의 문자열을 분리 --> 분리된 문자열을 matrix로 결합

yearlist = str_split(prData$YEAR, pattern = "-")
yearmatrix = data.frame(Reduce(rbind, yearlist))   # 시작년도만 있는 것도 종료년도까지 채워넣음.
names(yearmatrix) = c("start","stop")

head(yearmatrix)

prData = cbind(prData, yearmatrix)
prData$start = as.numeric(as.character(prData$start))   # numeric으로 변환
prData$stop = as.numeric(as.character(prData$stop))

head(prData)
summary(prData)


# 이름 앞 3글자로 검색
prData[str_sub(string = prData$PRESIDENT, start = 1, end = 3) == "Jam", ]

# 1로 끝나는 년도에 취임한 사람
prData[str_sub(string = prData$start, 4, 4) == 1, -3]

prData[str_detect(string = prData$PRESIDENT, pattern = "John"), -3]





#--------------------------------------------------------------
# Pair Plot
#--------------------------------------------------------------

library(ggplot2)

head(economics)
df = economics[, c(2,4,5,6)]
head(df)

GGally::ggpairs(df)   # 연속형 변수만 있는 경우


data(tips, package = "reshape2")
head(tips)

GGally::ggpairs(tips)   # 연속형, 범주형 변수 모두 있는 경우

