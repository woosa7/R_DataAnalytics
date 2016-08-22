#-------------------------------------------------------------------------------------
# 비정형 데이터 분석 | Drawing Charts
#-------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------
# 제주도 여행코스
#-------------------------------------------------------------------------------------

# 1.

# 한국어 관련 작업을 할 때 필요한 사전
library(KoNLP)
library(wordcloud)

useSejongDic()            
mergeUserDic(data.frame("주상절리", "ncn"))   # 필요한 단어를 사전에 추가
mergeUserDic(data.frame("협재해변", "ncn"))
mergeUserDic(data.frame("성산일출봉", "ncn"))
mergeUserDic(data.frame("섭지코지", "ncn"))
mergeUserDic(data.frame("천지연폭포", "ncn"))
mergeUserDic(data.frame("우도", "ncn"))
mergeUserDic(data.frame("산방산", "ncn"))
mergeUserDic(data.frame("중문관광단지", "ncn"))
mergeUserDic(data.frame("잠수함", "ncn"))
mergeUserDic(data.frame("러브랜드", "ncn"))
mergeUserDic(data.frame("용두암", "ncn"))
mergeUserDic(data.frame("신비의도로", "ncn"))
mergeUserDic(data.frame("한라산", "ncn"))
mergeUserDic(data.frame("오설록", "ncn"))
mergeUserDic(data.frame("유리의성", "ncn"))
mergeUserDic(data.frame("한림공원", "ncn"))
mergeUserDic(data.frame("용머리해안", "ncn"))
mergeUserDic(data.frame("해수욕장", "ncn"))
mergeUserDic(data.frame("중문", "ncn"))
mergeUserDic(data.frame("제주민속촌", "ncn"))
mergeUserDic(data.frame("외돌개", "ncn"))
mergeUserDic(data.frame("에코랜드", "ncn"))

txt <- readLines("data/jeju_utf8.txt")  # data를 변수로 읽어오기
txt
mode(txt)                               # 변수의 데이터형식 표시

# 공백 제거
txt <- txt[txt != '']
txt <- txt[txt != ' ']
txt <- txt[txt != '  ']
txt <- txt[txt != '   ']
# 불필요한 값들 제거
# gsub("변경전 글자","변경후 글자","원본데이터")
txt <- gsub("-"," ", txt)
txt <- gsub("&"," ", txt)
txt <- gsub("=>"," ", txt)
txt <- gsub("→"," ", txt)
txt <- gsub("\\("," ", txt)
txt <- gsub("\\)"," ", txt)
txt <- gsub("/"," ", txt)
txt <- gsub(","," ", txt)


# 2.

# extractNoun : 데이터 중 명사만 골라주는 함수.
# sapply : 함수의 결과를 list 에 담는다.
nounList <- sapply(txt, extractNoun, USE.NAMES = F)

# unlist : filtering 위해 list 객체를 일반 벡터로 변환
place <- unlist(nounList)
# 두글자 이상 되는 것만 필터링. nchar() = character length
place <- Filter(function(x) {nchar(x) >= 2}, place) 
place

# 원하지 않는 내용 걸러내기.
place <- gsub("제주","", place) 
place <- gsub("통운","", place)  
place <- gsub("전국","", place)  
place <- gsub("체인","", place) 
place <- gsub("업체","", place)  
place <- gsub("질문","", place)
place <- gsub("가격","", place)  
place <- gsub("무난","", place)   
place <- gsub("여행","", place)
place <- gsub("검색","", place)
place <- gsub("코스","", place)
place <- gsub("숙소","", place)
place <- gsub("준비","", place)
place <- gsub("다운로드","", place)
place <- gsub("조회수","", place)
place <- gsub("추천수","", place)
place <- gsub("추천","", place)
place <- gsub("답변수","", place)
place <- gsub("첫째날","", place)
place <- gsub("첫쨋날","", place) 
place <- gsub("좋구요","", place)
place <- gsub("이런거","", place)
place <- gsub("둘째날","", place)
place <- gsub("셋째날","", place)
place <- gsub("세쨋날","", place)
place <- gsub("토요일","", place)
place <- gsub("일요일","", place)
place <- gsub("시간","", place)
place <- gsub("항공","", place)
place <- gsub("관광지","", place)
place <- gsub("입장료","", place)
place <- gsub("저가","", place)
place <- gsub("항공사","", place)
place <- gsub("도움","", place)
place <- gsub("대략","", place)
place <- gsub("요금","", place)
place <- gsub("\\-","", place)
place <- gsub("이용","", place)
place <- gsub("공항","", place)
place <- gsub("해안","", place)
place <- gsub("드라이브","", place)
place <- gsub("경유","", place)
place <- gsub("바다","", place)
place <- gsub("전망","", place)
place <- gsub("하루","", place)
place <- gsub("렌트카","", place)
place <- gsub("하시","", place)
place <- gsub("예약","", place)
place <- gsub("사진","", place)
place <- gsub("위치","", place)
place <- gsub("필요","", place)
place <- gsub("할인","", place)
place <- gsub("출발","", place)
place <- gsub("가능","", place)
place <- gsub("소요","", place)
place <- gsub("일정","", place)
place <- gsub("하게","", place)
place <- gsub("근처","", place)
place <- gsub("중간","", place)
place <- gsub("다양","", place)
place <- gsub("첫날","", place)
place <- gsub("도착","", place)
place <- gsub("용머","", place)
place <- gsub("바위","", place)
place <- gsub("유명","", place)
place <- gsub("정도","", place)
place <- gsub("이동","", place)
place <- gsub("무료","", place)
place <- gsub("체험","", place)
place <- gsub("둘째","", place)
place <- gsub(" ","", place)
place <- gsub("\\d+","", place)    #  모든 숫자 없애기

# nouns1 <- grep("^ORA-+", txt, value=T)     # 첫글자가 ORA- 인 데이터만 사용.
# nouns2 <- substr(nouns1,5,9)               #ORA-12345 형식에서 숫자부분만 잘라냄
# nouns3 <- gsub("[A-z]","",nouns2)          # 영어가 있는 부분을 제거하고 숫자만 남겨둠

place <- Filter(function(x) {nchar(x) >= 2}, place)
place

# 정리된 데이터를 파일로 저장한 후 다시 table 형식으로 다시 불러오기. 빈 라인들 제거됨.
write(unlist(place), "jeju_step2.txt")
place2 <- read.table("jeju_step2.txt")
wordcount <- table(place2)

head(wordcount, 10)
head(sort(wordcount, decreasing=T), 50)   # 빈도수가 많은 순으로 정렬해서 상위 30개 조회


# 3.

# -------------------------------------------
# (1) Wordcloud

# min.freq : 최소 빈도 이상 언급된 단어만 출력
# scle : range of sizes of the words

library(RColorBrewer)       # color library 로딩
palete <- brewer.pal(9,"Set1") # 글자 색깔 지정
wordcloud(names(wordcount), freq=wordcount, scale=c(5, 0.5), min.freq=5, random.order=F, random.color=T, colors=palete)


# -------------------------------------------
# (2) Pie Chart

a <- head(sort(wordcount, decreasing=T), 10)
pie(a)
pie(a, col = rainbow(10), radius = 1)

percnt <- round(a/sum(a)*100, 1)      
names(a)
labelname <- paste(names(a), percnt, "%")   # % 값 넣기
pie(a, main = "Jeju Tour Point", col = rainbow(10), cex = 0.8, labels = labelname) # cex : size of label

# Conditiional Color
colors <- c()
for (i in 1:length(a)) {
    if (a[i] >= 50) {
        colors <- c(colors, "red")
    }
    else if (a[i] >= 15) {
        colors <- c(colors, "yellow")
    }
    else {
        colors <- c(colors, "green")
    }
}

pie(a, col = colors, radius = 1)

# -------------------------------------------
# (3) Donut Chart - 위에 흰색 원을 덮음.

par(new = T)
pie(a, radius = 0.5, col = "white", labels = NA, border = NA)


# -------------------------------------------
# (4) Bar Chart --- 15_barplot_rainbow

# space : bar 사이의 간격
# ylim : y 축 값범위
# cex : size of label

bp <- barplot(a, main="Jeju Tour Point", col=rainbow(10), space=0.2, ylim=c(0,35), cex.names=1.0, las=2)
percnt <- round(a/sum(a)*100, 1)
text(x=bp, y=a*1.1, labels=paste(percnt,"%"), col="black", cex = 0.7)
text(x=bp, y=a*0.9, labels=paste(a), col="black", cex = 0.7)


# -------------------------------------------
# (5) Horizontal Bar Chart --- 15_horizontal_barplot

# xlim : x 축 값범위

bp <- barplot(a, main="Jeju Tour Point", col=rainbow(10), xlim=c(0,35), cex.names=0.7, las=1, horiz=T)
text(y=bp, x=a*1.15, labels=paste(percnt,"%"), col="black", cex = 0.7)
text(y=bp, x=a*0.9, labels=paste(a), col="black", cex = 0.7)


# -------------------------------------------
# (6) Line Chart

# xlab : x 축 labels
# ylab : y 축 labels
# type="o" : 사선 / type="s" : 직선
# lwd : 선 굵기
# h : y-value(s) for horizontal line(s).
# v : x-value(s) for vertical line(s).

plot(a, main="Jeju Tour Point", xlab="", ylab="", ylim=c(0,35), axes=FALSE, type="s", col="red", lwd=5)
axis(1, at=1:10, labels=names(a), las=2)   # x 축
axis(2, las=1)                             # y 축
abline(h=seq(0,35,5), v=seq(1,10,1), col="gray", lty=2)


# -------------------------------------------
# (7) 3D Pie Chart

# explode : 각 조각의 간격

install.packages("plotrix")
library("plotrix")

cpercnt <- round(a/sum(a)*100, 1)
clabels <- paste(names(a), "\n", "(", cpercnt, ")")
pie3D(a, main="제주도 관광지", col=rainbow(10), cex=0.7, labels=clabels, explode=0.05)


# Plot 이미지로 저장하기

# windows(800, 600, pointsize = 12)   # 별도의 윈도우 열기
# pie(a)                              # 차트 그리기
# savePlot("jeju.png", type="png")    # 결과물을 그림으로 저장
# dev.off()                           # 윈도우 닫기



#-------------------------------------------------------------------------------------
# 비정형 자료 분석 - Propose
#-------------------------------------------------------------------------------------

# chr (문장) --> 단어 list --> chr (단어) --> txt --> table list --> wordcount (numeric)

txt <- readLines("data/propose_utf8.txt", encoding = "UTF-8")
txt

txt <- txt[txt != '']
txt <- txt[txt != ' ']
txt <- txt[txt != '  ']

nounList <- sapply(txt, extractNoun, USE.NAMES = F)

tempText <- unlist(nounList)
tempText <- gsub("프로포즈","", tempText)
tempText <- gsub("선물","", tempText)
tempText <- gsub("추천","", tempText)
tempText <- gsub("답변","", tempText)
tempText <- gsub("조회","", tempText)
tempText <- gsub("생각","", tempText)
tempText <- gsub("생활","", tempText)
tempText <- gsub("이벤트","", tempText)
tempText <- gsub("준비","", tempText)
tempText <- gsub("\\d+","", tempText)    #  모든 숫자 없애기
tempText <- Filter(function(x) {nchar(x) >= 2}, tempText)


write(unlist(tempText), "propose2.txt")
pro_table <- read.table("propose2.txt")
wordcount <- table(pro_table)

head(sort(wordcount, decreasing=T), 30)

library(RColorBrewer)
palete <- brewer.pal(9,"Set1")

wordcloud(names(wordcount), freq=wordcount, scale=c(5, 0.5), min.freq=5, random.order=F, random.color=T, colors=palete)

