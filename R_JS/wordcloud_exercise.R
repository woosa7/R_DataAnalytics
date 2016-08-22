#------------------------------------------------------------
# wordcloud test
#------------------------------------------------------------

setwd("/Volumes/MacHDD/workspace/R_Study/R_JS")

library(KoNLP)  # 설치된 패키지를 Loading 합니다.
library(wordcloud)

#Step 3. 분석용 데이터를 변수로 읽어 들입니다.

txt <- readLines("data/hong.txt") # txt 라는 변수에 한 줄 씩 읽어 들입니다.
txt

#Step 4. 데이터 중에서 명사만 골라낸 후 nouns 변수에 할당합니다.

txt <- gsub("저","",txt)  # 제거할 글자를 지정합니다
txt <- gsub("수","",txt)
txt <- gsub("들","",txt)
txt <- gsub("것","",txt)

nouns <- sapply(txt,extractNoun,USE.NAMES=F)

#Step 5. 추출된 명사를 상위 30 개만 출력해서 확인합니다.

head(unlist(nouns), 30)

#Step 6. 파일에 저장해 둡니다. 

write(unlist(nouns),"hong_2.txt") 

#Step 7. 수정 완료된 파일을 다시 table 형식으로 변환해서 변수에 불러들입니다.

rev <- read.table("hong_2.txt")

#Step 8. 화면에 그래픽으로 출력하기 전에 text 형태로 결과를 확인해 봅니다

nrow(rev) # rev 변수에 몇건의 데이터가 있는지 확인해 봅니다
wordcount <- table(rev)
head(sort(wordcount, decreasing=T),30)

#Step 9. Word Cloud 형태로 그래픽으로 출력합니다

library(RColorBrewer) # 화면에 출력할 컬러를 사용할 라이브러리를 Loading 합니다.
palete <- brewer.pal(9,"Set1") # 글자 색깔을 지정합니다.

wordcloud(names(wordcount),freq=wordcount,scale=c(5,0.5),rot.per=0.25,min.freq=1,
          random.order=F,random.color=T,colors=palete)

