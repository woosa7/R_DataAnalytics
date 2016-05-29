#---------------------------------------------------------------
# Graph : Chapter2_1 참조
#---------------------------------------------------------------

#---------------------------------------------------------------
# Graph Options

#   main=""			    제목
#   sub=""			    부제목
#   xlab="", ylab=""    x, y 축 제목 문자열
#   xlim= , ylim=       x, y 축 한계값
#   ann=F			    x, y 축 제목 표시하지 않음
#   axes=F			    x, y 축 표시하지 않음
#   tmag=2              제목 등에 사용되는 문자의 확대배율
# 
#   axis(...)		    사용자지정 x, y 축
#   title               메인, x, y 축 제목 지정
# 
#   <그래프 타입>
#   type = "p"		    점 (기본값)
#   type = "l"		    꺾은 선
#   type = "b"		    점과 선
#   type = "c"		    "b"에서 점 생략
#   type = "o"		    점과 선 중첩
#   type = "h"		    각 점에서 x축까지 수직선 그래프
#   type = "s"		    왼쪽 값을 기초로 계단 모양으로 연결
#   type = "S"		    오른쪽 값을 기초로 계단 모양으로 연결
#   type = "n"		    축만 그리고 그래프는 그리지 않음
#   
#   <선>
#   lty=0, lty="blank" 
#   lty=1, lty="solid"
#   lty=2, lty="dashed"
#   lty=3, lty="dotted"
#   lty=4, lty="dotdash"
#   lty=5, lty="longdash"
#   lty=6, lty="twodash"
#   
#   <색, 기호>
#   col=1, col="blue"	색상 1 ~ 8
#   pch=0, pch="문자"	점 모양 지정
#   bg="blue"           그래프 배경색
#   lwd="숫자"		    선 굵기
#   cex="숫자"		    점 또는 문자 굵기
#   
#   par(mfrow = c(행수, 열수))        : 한 페이지에 여러 그래프 배치
#   par(new=T)                        : 그래프 중첩
#
#   lines                             : 그래프 위에 새로운 그래프 그리기
#   
#   legend                            : 범례


apple <- c(260, 400, 250, 200, 310)
peach <- c(180, 200, 210, 190, 170)
orange <- c(210, 250, 260, 330, 300)

windows(800, 600, pointsize = 10)   # 별도의 윈도우 열기
dev.off()                           # 윈도우 닫기

#---------------------------------------------------------------
# plot
#---------------------------------------------------------------
# plot(y data, option)
# plot(x data, y data, option)

x <- c(1:20)
y <- sample(1:20)
plot(x, y, xlim=c(0,20), ylim=c(0,25), main="sample", type="o", lty=2, col=2)


# 사용자지정 x, y 축

# 1.
plot(apple, type="o", col="red", ylim=c(0,400))


# 2.
plot(apple, type="o", col="red", ylim=c(0,400), axes = F, ann=F)
axis(1, at=1:5, labels = c("Mon","Tue","Wed","Thu","Fri"))
axis(2, ylim=c(0,400))
title(main="Fruits", col.main="red", font.main=4)
title(xlab="Day", col.lab="black")
title(ylab="Qty", col.lab="blue")

# 3. from # 2.
par(new=T)
lines(peach, type="o", pch=21, col="green", lty=2)
lines(orange, type="o", pch=22, col="blue", lty=2)

legend(4, 130, c("apple","peach","orange"), col=c("red","green","blue"), cex=0.8, pch=21, lty=1:3)


# 4.
# multi graph on 1 window
par(mfrow = c(1,3))
plot(apple, type="o", col="red", ylim=c(0,400))
plot(peach, type="o", col="red", ylim=c(0,400))
plot(orange, type="o", col="red", ylim=c(0,400))


#---------------------------------------------------------------
# barplot
#---------------------------------------------------------------

# angle, density, col   : 막대를 칠하는 선의 각도, 밀도, 색상
# lengent               : 범례
# names                 : 각 막대의 라벨
# width                 : 각 막대의 상대적인 폭
# space                 : 각 막대 사이의 간격
# beside                : True 일 경우 각 값마다 막대를 그린다.
# horiz                 : True 일 경우 가로막대 출력


# 요일별 apple 판매량

# density
barplot(apple, main="Apple", xlab="Day", ylab="Qty", border="blue",
        names.arg = c("Mon","Tue","Wed","Thu","Fri"),
        density = c(10,30,50,70,90))


# condition color 특정 조건을 만족하는 경우에 색깔 표시
colors <- c()
for (i in 1:length(apple)) {
  if (apple[i] >= 300) {
    colors <- c(colors, "red")
  }
  else {
    colors <- c(colors, "grey")
  }
}

barplot(apple, main="Apple", xlab="Day", ylab="Qty", border="blue", col=colors,
        names.arg = c("Mon","Tue","Wed","Thu","Fri"))


# 과일별/요일별 판매랑

fruits10 <- cbind(apple, peach, orange)
fruits10

barplot(as.matrix(fruits10), main="Fruits", ylab="Qty", ylim = c(0,450), 
        beside = T, col=rainbow(5))
legend(10, 450, c("Mon","Tue","Wed","Thu","Fri"), cex=0.8, fill=rainbow(5))



# 요일별/과일별 판매량
# multi-section / 1 bar

fruits11 <- t(fruits10)
colnames(fruits11) <- c(1,2,3,4,5)
fruits11

barplot(fruits11, main="Fruits", ylab="Qty", ylim = c(0,1100), col=rainbow(3), space=0.1,
        cex=0.8, cex.axis=0.8, las=1, names.arg=c("Mon","Tue","Wed","Thu","Fri"))
legend(4, 1100, rownames(fruits11), cex=0.8, fill=rainbow(3))



#---------------------------------------------------------------
# Pie Chart
#---------------------------------------------------------------

# angle, density, col : 각도, 밀도, 색상
# labels          : 각 부분의 이름 지정
# radius          : 원형의 크기
# clockwise       : 회전 방향. 기본 = 반시계방향
# init.angle      : 시작되는 지점의 각도


apple <- c(260, 400, 250, 200, 310)
pie(apple)

# 1.
pie(apple, init.angle = 90, col=rainbow(length(apple)), radius = 1,
    labels = c("Mon","Tue","Wed","Thu","Fri"))

# 2. 비율
f_ratio <- round(apple/sum(apple)*100, 1)
f_labels <- paste(f_ratio, "%", sep = "")

pie(apple, main="Apple", init.angle = 90, col=rainbow(length(apple)), radius = 0.8,
    cex=0.8, labels=f_labels)
legend(1,1, c("Mon","Tue","Wed","Thu","Fri"), cex=0.8, fill=rainbow(length(apple)))

# 3.
f_ratio <- round(apple/sum(apple)*100, 1)
f_days <- c("Mon","Tue","Wed","Thu","Fri")
f_labels <- paste(f_days, "\n", f_ratio, "%")
pie(apple, main="Apple", init.angle = 90, col=rainbow(length(apple)), radius = 1,
    cex=1.0, labels=f_labels)



#---------------------------------------------------------------
# 3D Pie Chart
#---------------------------------------------------------------

# explode : 각 조각의 간격

library("plotrix")

f_ratio <- round(apple/sum(apple)*100, 1)
f_days <- c("Mon","Tue","Wed","Thu","Fri")
f_labels <- paste(f_days, "\n", f_ratio, "%")

pie3D(apple, main="Apple", col=rainbow(length(apple)), cex=0.8, labels=f_labels, explode=0.05)



#---------------------------------------------------------------
# boxplot
#---------------------------------------------------------------

# col         : 박스 색상
# names       : 막대 라벨
# range       : 박스 끝에서 수염까지의 길이. default = 1.5
# width       : 박스 폭
# notch       : True일 경우 상자의 허리부분 가늘게 표시
# horizontal  : 가로 상자


boxplot(apple, peach, orange,
        col=c("red","yellow","green"),
        names=c("apple","peach","orange"),
        horizontal = T)


#---------------------------------------------------------------
# treemap
#---------------------------------------------------------------

# vSize   : 그룹핑 기준
# index   : 화면 표시 기준

library(treemap)

setwd("c:/R_Study/R_JS")

data <- read.csv("data/all_student_score.csv")
data

treemap(data, vSize="score", index="score")
treemap(data, vSize="team", index="team")

treemap(data, vSize="team", index=c("score","name"))
treemap(data, vSize="score", index=c("team","score"))



#---------------------------------------------------------------
# stars
#---------------------------------------------------------------

# draw.segments = T : 부채꼴로 색상 표시
# full              : T 원형 / F 반원형


data <- read.csv("data/all_subject_score.csv")
data
nrow(data)

# 1번 이름 컬럼을 rowname으로 설정하고 삭제함
row.names(data) <- data$name
rownames(data)
data <- data[2:7]

# star chart

# 1.
stars(data, flip.labels = F, draw.segments = F, frame.plot = T, full = T,
      main = "Score Diagram of subject/student")

label <- names(data)
value <- table(label)
pie(value, labels = label, radius = 0.2, cex = 0.6, col = NA)


# 2.
stars(data, flip.labels = T, draw.segments = T, frame.plot = T, full = T,
      main = "Score Diagram of subject/student")

label <- names(data)
value <- table(label)
color <- c("black","red","green","blue","cyan","violet")
pie(value, labels = label, radius = 0.1, cex = 0.6, col = color)


# 3.
stars(data, flip.labels = T, draw.segments = T, frame.plot = T, full = F,
      main = "Score Diagram of subject/student")


# star --> bar
data2 <- t(data)
data2

label <- rownames(data2)
label

barplot(as.matrix(data2), main = "Test", ylab = "score", beside = T, 
        col = rainbow(5), ylim = c(0,150))
abline(h=60, col = "red", lwd=2)
legend(50,140, label, cex=0.8, fill = rainbow(5))



#---------------------------------------------------------------
# ggplot2
#---------------------------------------------------------------

# ggplot(data, aes(x=x axis data, y=y axis data)) + geom 함수

library(ggplot2)

data <- read.table("data/korean_score.txt", header=T, sep="")
data

# geom_point()

ggplot(data, aes(x=이름, y=점수)) + geom_point()


# geom_bar()
# -- stat : 주어진 데이터에서 geom에 필요한 데이터를 생성.

ggplot(data, aes(x=이름, y=점수)) + geom_bar(stat = "identity")

gpbar <- ggplot(data, aes(x=이름, y=점수)) + geom_bar(stat = "identity", fill="cyan", colour="red")

gpbar + theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1, color="blue", size=10))


# geom_bar + geom_text

library(plyr)  # split-apply-combine paradigm !!!

data <- read.csv("data/student_score_3.csv", header=T, sep=",")
data

transData <- arrange(data, 이름, 과목)   # plyr : sort
transData <- ddply(transData, "이름", transform, 합계=cumsum(점수)) # plyr : 누적합계
transData <- ddply(transData, "이름", transform, label=cumsum(점수)-점수*0.5) # 라벨 위치
transData

ggplot(transData, aes(x=이름, y=점수, fill=과목)) + 
    geom_bar(stat="identity") + 
    geom_text(aes(y=label, label=paste(점수,"점")), color="black", size=4) + 
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1, color="blue", size=10))


# geom_segment

data <- read.csv("data/all_subject_score.csv")
kdata <- data[, 1:2]
kdata

# theme_bw() : 배경 흑백
# panel.grid.major.y = element_line : 가로 선 그리기

ggplot(data[,1:2], aes(x=Korean, y=reorder(name,Korean))) + geom_point(size=5) +
  theme_bw() +                                      
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color="red", linetype="dashed"))


ggplot(data[,1:2], aes(x=Korean, y=reorder(name,Korean))) + 
  geom_segment(aes(yend=name, xend=0, color="red")) +
  geom_point(size=5) +
  theme_bw() +                                      
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank())


# geom_line

library(plyr)

data <- read.csv("data/student_subject _score_3.csv")
data2 <- arrange(data, 이름, 과목)   # plyr : sort
data2

ggplot(data2, aes(x=과목, y=점수, color=이름, group=이름)) + geom_line() 

ggplot(data2, aes(x=과목, y=점수, color=이름, group=이름, fill=이름)) + geom_line() + 
  geom_point(size=3, shape=22)



