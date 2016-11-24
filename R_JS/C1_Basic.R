#-----------------------------------------------------------
# 기초 문법
#-----------------------------------------------------------

#-----------------------------------------------------------
# NA

a <- c(1,2,NA)
sum(a)
sum(a, na.rm = T)

# NA : Not Available. logical type.
# NaN : Not a Number 정의되지 않은 숫자. double type.

is.na(NA)                       # TRUE
is.na(NaN)                      # TRUE
is.na(c("NA", NA, NaN))         # FALSE TRUE FALSE / 처음값 때문에 NaN이 문자형으로 형변환.
is.na(c(NaN, NA))               # TRUE TRUE


#-----------------------------------------------------------
# factor : 문자 --> 숫자. 그래프 또는 통계처리에 유리.

a <- c("서울","부산","제주","제주","서울","대전","부산","서울")
fa <- factor(a)
fa
mode(fa)
plot(fa)


#-----------------------------------------------------------
# Date

Sys.Date()
substr(Sys.Date(), 1, 4)

startDate <- as.Date("2016-03-01")
endDate <- as.Date("2016-04-09")
endDate - startDate
as.numeric(endDate - startDate)

as.Date("05072016", format="%d%m%Y")

as.Date(30, "2016-05-01")
as.Date(-30, "2016-05-01")


#-----------------------------------------------------------
# 내장 함수

ceiling(1.6)
floor(1.6)
exp(2)           # e^2
factorial(5)
sqrt(4)          # root

x <- c(1:10)
max(x)
min(x)
length(x)
mean(x)          # 평균
median(x)        # 중앙값
sd(x)            # 표준편차
rev(x)


#-----------------------------------------------------------
# 집합 연산

a <- c(1,2,3)
b <- c(2,3,4)
a + b
c <- c("3", "4", "5")

union(a, c)
setdiff(a,b)
setdiff(b,a)
intersect(a,b)


#-----------------------------------------------------------
# Vector

# vector의 datatype은 모두 동일해야 한다.
# 다를 경우 자동으로 하나의 데이터타입으로 처리됨.
a <- c(1, 2, "3")         
a

names(a) <- c("x1", "x2", "x3")   # vector 각 컬럼에 이름 지정 가능
a
a["x1"]

length(a)
nrow(a)        # 행렬에만 사용되는 함수
NROW(a)

seq(1, 10)
seq(5, -5)
seq(-5, 5, 2)   # 2씩 증가

rep(1:5, 2)         # 반복
rep(1:5, 2, each=2) # 연속해서 반복

1 %in% a       # vector에 특정 문자 포함 여부

# vector indexing
a <- c(1,2,3,4,5,6)
a[c(1,3)]
a[-1:-3]
a[-length(a)]


#-----------------------------------------------------------
# 행렬 matrix

x <- matrix(c(1,2,3,4))
x <- matrix(c(1,2,3,4), nrow=2)
x <- matrix(c(1,2,3,4), nrow=2, byrow = T)
x

x[1,]      # 행 값들 가져오기
x[,1]      # 열 값들 가져오기

x <- matrix(c(1,2,3,4), nrow=2)
x <- rbind(x, c(77, 88))       # 행 추가
x <- cbind(x, c(9, 8, 7))      # 열 추가
x

colnames(x) <- c("no1", "no2", "no3")
x


#-----------------------------------------------------------
# list

member <- list(name="Kate", address="Seoul", tall=170, pay=10000)
member
member$name
member[2:3]

member$birth <- "1990-05-07"      # list에 항목 추가
member$birth <- NULL              # list 항목 삭제

length(member)                    # 항목 갯수


#-----------------------------------------------------------
# dataframe

# Create Dataframe

# 1. 직접 생성

no <- c(1:3)
name <- c("xx", "yy", "zz")
price <- c(500,100,300)
qty <- c(2,7,9)

item <- data.frame(NO=no, Name=name, Price=price, QTY=qty)
item


# 2. from Matrix

x <- c(1,"James",300, 2,"Kate",500, 3,"Risa",700, 4,"Liz",900)
data <- matrix(x, 4, byrow = T)
data

member <- data.frame(data)
names(member) <- c("NO", "NAME", "PAY")
member


# 3. from Text file

fruitData <- read.table("data/fruits.txt", header = T, sep = "")
fruitData

fruitData <- read.table("data/fruits.txt", header = T, sep = "", skip = 3)      # ignore 3 row
fruitData <- read.table("data/fruits.txt", header = T, sep = "", nrows = 3)     # return first 3 row


# 4. from csv or Excel file

cnames <- c("no", "name", "price1", "price2", "qty")
df <- read.csv("data/fruits.csv", header = F, col.names = cnames)  # 컬럼명 지정해서 import
df

library("readxl")
df <- read_excel(path = "data/fruits.xlsx", sheet = "Sheet1", col_names = TRUE)
df


# save to xml

library("XML")
library("kulife")
write.xml(df, file="fruits.xml")


#-----------------------------------------------------------
# Dataframe 다루기

df <- read_excel(path = "data/fruits.xlsx", sheet = "Sheet1", col_names = TRUE)
df

summary(df)
str(df)

df[2,2]
df$lowprice
df[c(2,3)]                    # column 1,2
df[c(2,3),]                   # row 1,2

ncol(df)
nrow(df)
names(df)
rownames(df)


# sort / order / rank

sort(df$highprice)
sort(df$highprice, decreasing = T)

order(df$highprice)           # 정렬된 위치 인덱스 리턴
rank(df$highprice)            # 정렬되지 않은 위치 인덱스 리턴


# split

split(df, df$name)            # 해당 컬럼 기준으로 분할
split(df, df$no > 2)


# merge

x <- data.frame( names=c("A", "B", "C"), address=c("Seoul", "Busan", "Tyokyo"))
y <- data.frame( names=c("A", "B", "D"), telno=c("001", "003", "888"))

merge(x, y)      # 공통적으로 있는 데이터만 merge
merge(x, y, by = "names")
merge(x, y, by = "names", all = T)


# subset : dataframe 에서 조건에 맞는 데이터를를 dataframe으로 추출

subset(df, df$qty > 5)
subset(df, highprice > 500)
subset(df, name == "apple")
subset(df, select = c(name,qty), subset = df$qty > 5)
subset(df, select = -no)

library(MASS)
str(Cars93)

subset(Cars93, select = c(Model, Type, Price), MPG.city > 30)

subset(Cars93, select = c(Manufacturer, Model, Type, Price, Make), 
       MPG.highway > median(MPG.highway) & Manufacturer == "Subaru")


# Dataframe 내용 저장.

write.table(df, "data/save_fruits.txt", quote = F, append = F)     



#-----------------------------------------------------------
# apply family
#-----------------------------------------------------------
# apply, lapply, sapply, by, tapply, aggregate

# apply     : 결과를 vector로 반환 (1 행 / 2 열)
# lapply    : 결과를 list로 반환
# sapply    : 결과를 vector 또는 matrix로 반환
#-----------------------------------------------------------

s1 <- c(91, 87, 95, 96, 89, 87, 86, 85, 92, 93)
s2 <- c(89, 86, 85, 92, 93, 91, 90, 95, 87, 89)
s3 <- c(89, 86, 78, 99, 95, 87, 89, 86, 85, 92)


# list
score <- list(korean = s1, english = s2, math = s3)
score

lapply(score, mean)   # ---> list
sapply(score, mean)   # ---> vector
sapply(score, range)

sapply(score, t.test)

extremes <- function(x) {
    c(min = min(x), max = max(x))
}

sapply(score, extremes)


# matrix
score <- c(s1, s2, s3)
dim(score) <- c(3, 10)
colnames(score) <- c("t1","t2","t3","t4","t5","t6","t7","t8","t9","t10")
rownames(score) <- c("K", "L", "M")
score

apply(score, 1, mean)
apply(score, 2, max)


# dataframe
df2 <- data.frame(score=c(s1, s2, s3))
df2$name <- c("K", "L", "M")
df2

apply(df2, 1, mean)   # error : 각 열의 데이터타입이 다름

by(df2$score, df2$name, mean)

tapply(df2$score, df2$name, mean)

aggregate(score ~ name, data = df2, mean)


#-----------------------------------------------------------
# apply examples

# 1
pioneers <- c("GAUSS:1777", "BAYES:1702", "PASCAL:1623", "PEARSON:1857")
split <- strsplit(pioneers, split = ":")
split

names <- lapply(split, function(x) { x[1] })
names

# 2
select_el <- function(x, index) {
    x[index]
}

years <- lapply(split_low, select_el, index = 2)
years



#-----------------------------------------------------------
# 입출력
#-----------------------------------------------------------

scan("data/fruits.txt", what = "")

readLines("data/fruits.txt")

read.table("data/fruits.txt")
read.table("data/fruits.txt", header = T)


#-----------------------------------------------------------
# 문자열 합치기, 나누기, Replacement
#-----------------------------------------------------------

paste("abc", "xyz")
paste("abc", "xyz", sep = ":")

substr("123456789", 3, 5)                  # 시작위치, 끝나는 위치
substr("123456789", -2, 2)

strsplit("2016-04-19", split = "-")

d <- readLines("data/alert_log.txt")
d <- gsub(" ", "_", d)                     # 특정 문자열 치환. gsub
length(d)

c <- subset(d, nchar(d) > 100)             # nchar : 문자열 길이
length(c)


#-----------------------------------------------------------
# 정규표현식
#-----------------------------------------------------------
regexpr("ORACLE", c)

# \\d		숫자
# \\D		숫자 아닌 것
# 
# \\s		공백
# \\S		공백 아닌 것
# 
# \\t		Tab
# \\n		new line (enter)
# \\.       dot
# 
# ^		    시작 글자
# $		    마지막 글자
# .*        any character
# 
# [ab]		a 또는 b
# [^ab]		a와 b 제외한 모든 문자
# [0-9]		모든 숫자
# [A-Z]		영어 대문자
# [a-z]		영어 소문자
# [A-z]		모든 영문자
# 
# i+	    i가 최소 1회 이상 나오는 경우
# i*		i가 최소 0회 이상 나오는 경우
# i?		i가 최소 0회에서 최대 1회만 나오는 경우
# i{n}		i가 연속적으로 n회 나오는 경우
# i{n1,n2}	i가 n1에서 n2회 나오는 경우
# i{n,}		i가 n회 이상 나오는 경우
# 
# [:alnum:]	문자와 숫자가 나오는 경우
# [:alpha:]	문자
# [:blank:]	공백
# [:cntrl:]	제어 문자
# [:digit:]	0 ~ 9
# [:lower:]	소문자
# [:print:]	숫자, 문자, 특수문자, 공백 모두
# [:punct:]	특수문자
# [:space:]	공백문자
# [:upper:]	대문자
# [:xdigit:]	16진수

grep("ORACLE", d)                # vector에서 특정 패턴을 찾아 index 출력
grep("ORACLE", d, value = T)     # 값 출력

grep("^Setting", d)              # ^ : 첫글자
grep("ing$", d)                  # $ : 마지막 글자
grep("[7-9]", d)


emails <- c("john.doe@ivyleague.edu", "education@world.gov", "dalai.lama@peace.org", 
            "invalid.edu", "quant@bigdatacollege.edu", "cookie.monster@sesame.tv")

hits = grep("@.*\\.edu$", emails)   # @ 다음에 .edu로 끝나는 것.
emails[hits]

gsub(pattern = "@.*\\.edu$", replacement = "@datacamp.edu", emails)











