#------------------------------------------------------------
# wordcloud test
#------------------------------------------------------------

library(KoNLP)
library(wordcloud)

# 분석용 데이터 read

txt <- readLines("data/hong.txt") # txt 라는 변수에 한 줄 씩 읽어 들입니다.
txt

# 데이터 중에서 명사만 골라낸 후 nouns 변수에 할당

txt <- gsub("저","",txt)  # 제거할 글자 지정
txt <- gsub("수","",txt)
txt <- gsub("들","",txt)
txt <- gsub("것","",txt)

nouns <- sapply(txt, extractNoun, USE.NAMES=F)

# 추출된 명사를 상위 30 개만 출력해서 확인

head(unlist(nouns), 30)

# 파일에 저장

write(unlist(nouns),"hong_2.txt") 

# 파일을 다시 table 형식으로 변환

rev <- read.table("hong_2.txt")

# text 형태로 결과 확인

nrow(rev)
wordcount <- table(rev)
head(sort(wordcount, decreasing=T),30)

# Word Cloud 형태로 출력

library(RColorBrewer)          # 컬러 라이브러리
palete <- brewer.pal(9,"Set1") # 글자 색깔 지정

wordcloud(names(wordcount),freq=wordcount,scale=c(5,0.5),rot.per=0.25,min.freq=1,
          random.order=F,random.color=T,colors=palete)

