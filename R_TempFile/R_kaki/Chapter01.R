#-------------------------------------------------------------------------------------
# 제주도 여행코스 word cloud

# 1.

install.packages("KoNLP")     # 한국어 관련 작업을 할 때 필요한 사전
install.packages("wordcloud")

library(KoNLP)                # package load
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

txt <- readLines("jeju.txt")        # data를 변수로 읽어오기
mode(txt)                           # 변수의 데이터형식 표시

txt <- txt[txt != '']               # 불필요한 값들 제거
txt <- txt[txt != ' ']
txt <- txt[txt != '  ']
txt <- txt[txt != '   ']
txt <- gsub("-"," ", txt)
txt <- gsub("&"," ", txt)
txt <- gsub("=>"," ", txt)
txt <- gsub("→"," ", txt)
txt <- gsub("\\("," ", txt)
txt <- gsub("\\)"," ", txt)


# 2.

# extractNoun : 데이터 중 명사만 골라주는 함수.
# sapply : 함수의 결과를 list 에 담는다.
nounList <- sapply(txt, extractNoun, USE.NAMES = F)

tempText <- unlist(nounList)                            # unlist : filtering 위해 일반 텍스트로 저장

place <- Filter(function(x) {nchar(x) >= 2}, tempText)  # 두글자 이상 되는 것만 필터링. nchar() = character length

# 원하지 않는 내용 걸러내기.
# gsub("변경전 글자","변경후 글자","원본데이터")
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
place <- gsub("리","", place)
place <- gsub("바위","", place)
place <- gsub("유명","", place)
place <- gsub("정도","", place)
place <- gsub("이동","", place)
place <- gsub("무료","", place)
place <- gsub("용머","", place)
place <- gsub("체험","", place)
place <- gsub("둘째","", place)
place <- gsub(" ","", place)
place <- gsub("\\d+","", place)    #  모든 숫자 없애기

place <- Filter(function(x) {nchar(x) >= 2}, place)

# 정리된 데이터를 파일로 저장한 후 다시 table 형식으로 다시 불러오기
write(unlist(place), "jeju_step2.txt")
place2 <- read.table("jeju_step2.txt")
wordcount <- table(place2)

head(place2, 10)
head(wordcount, 10)

head(sort(wordcount, decreasing=T), 30)   # 빈도수가 많은 순으로 정렬해서 상위 30개 조회


# 3.

# (1) Wordcloud
# min.freq : 최소 빈도 이상 언급된 단어만 출력
library(RColorBrewer)       # color library 로딩
palete <- brewer.pal(9,"Set1") # 글자 색깔 지정
wordcloud(names(wordcount), freq=wordcount, scale=c(5, 0.5), min.freq=5, random.order=F, random.color=T, colors=palete)

# (2) Pie Chart
a <- head(sort(wordcount, decreasing=T), 10)
pie(a)
pie(a, col = rainbow(10), radius = 1)

percnt <- round(a/sum(a)*100, 1)      
names(a)
labelname <- paste(names(a), percnt, "%")   # % 값 넣기
pie(a, main = "제주도 관광지", col = rainbow(10), cex = 0.8, labels = labelname)

# (3) Donut Chart - 위에 흰색 원을 덮음.
par(new = T)
pie(a, radius = 0.5, col = "white", labels = NA, border = NA)

# (4) Bar Chart
bp <- barplot(a, main="제주도 관광지", col=rainbow(10), space=0.8, ylim=c(0,35), cex.names=0.7, las=2)
percnt <- round(a/sum(a)*100, 1)
text(x=bp, y=a*1.1, labels=paste(percnt,"%"), col="black", cex = 0.7)
text(x=bp, y=a*0.9, labels=paste(a), col="black", cex = 0.7)

# (5) Horizontal Bar Chart
bp <- barplot(a, main="제주도 관광지", col=rainbow(10), xlim=c(0,35), cex.names=0.7, las=1, horiz=T)
text(y=bp, x=a*1.15, labels=paste(percnt,"%"), col="black", cex = 0.7)
text(y=bp, x=a*0.9, labels=paste(a), col="black", cex = 0.7)


#savePlot("jeju.png", type="png")   # 결과물을 그림으로 저장



#-------------------------------------------------------------------------------------
# Propose

# chr (문장) --> 단어 list --> chr (단어) --> txt --> table list --> wordcount (numeric)

txt <- readLines("propose.txt")
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
tempText <- Filter(function(x) {nchar(x) >= 2}, tempText)

#tempText <- grep("^ORA+", tempText, value = T)  # 첫글자가 ORA 인 데이터만 사용.

write(unlist(tempText), "propose2.txt")
pro_table <- read.table("propose2.txt")
wordcount <- table(pro_table)

head(sort(wordcount, decreasing=T), 30)

library(RColorBrewer)
palete <- brewer.pal(9,"Set1")

wordcloud(names(wordcount), freq=wordcount, scale=c(5, 0.5), min.freq=5, random.order=F, random.color=T, colors=palete)

#-------------------------------------------------------------------------------------








