#----------------------------------------------------------------------------
# 구간 (범위) 데이터 분석 - boxplot
#----------------------------------------------------------------------------

require(ggplot2)

#----------------------------------------------------------------------------
# boxplot

tempratureData <- read.csv("data/Trend_Temperature_Seoul.csv", header=T)
str(tempratureData)
head(tempratureData)

# ggplot > boxplot
# aes(x축, y축)

ggplot(tempratureData, aes(factor(Month), MeanTemp)) + geom_boxplot()
savePlot("20_temperature_boxplot.png", type="png")

ggplot(tempratureData, aes(factor(Month), MeanTemp)) + geom_point()
savePlot("20_temperature_ggplot_point.png", type="png")



#----------------------------------------------------------------------------
# boxplot

# geom_jitter : 데이터의 분포를 점으로 표시

sales <- read.csv("data/emp_monthly_score.csv", header=T)
head(sales)

# 1. box만
ggplot(sales,aes(factor(name),score)) + geom_boxplot()
savePlot("21_boxplot.png", type="png")

# 2. 분포위치
ggplot(sales,aes(factor(name),score)) + geom_boxplot() + geom_jitter()
savePlot("21_boxplot_point.png", type="png")

# 3. 색상넣기
ggplot(sales,aes(factor(name),score)) + geom_boxplot(aes(fill=(name))) + geom_jitter()
savePlot("21_boxplot_color.png", type="png")



#----------------------------------------------------------------------------
# boxplot

total <- read.csv("data/all_student_score.csv", header=T)
head(total)

ggplot(total,aes(factor(team),score)) + geom_boxplot(aes(fill=(team)))

ggplot(total,aes(factor(team),score)) + geom_boxplot(aes(fill=(team))) + geom_jitter()



#----------------------------------------------------------------------------
# dash board : polygon
#----------------------------------------------------------------------------
# example(polygon)

sales <- read.csv("data/emp_monthly_score_1.csv", header=T, stringsAsFactors=FALSE, sep=",")
sales

# function
# x : 표현할 값, angle1 : 좌측 게이지 각도, angle3 : 우측 게이지 각도, title : Dashboard 제목

# matrix(c(-1,0,0,1,1,0), 3, 2, byrow=T)
#       [,1] [,2]
# [1,]   -1    0
# [2,]    0    1
# [3,]    1    0

dash_t <- function(x, angle1, angle3, title) {
    i <- matrix(c(-1,0,0,1,1,0), 3, 2, byrow=T)
    plot(i, xlab="", ylab="", axes=F, type="n")
    x.cir <- cos(seq(0,180,1)*pi/180)
    y.cir <- sin(seq(0,180,1)*pi/180)
    
    #첫번째 polygon
    cir <- rbind(cbind(x.cir[(181-angle1):181], y.cir[(181-angle1):181]) ,
                 cbind(0.8*x.cir[181:(181-angle1)], 0.8*y.cir[181:(181-angle1)]),
                 col="green")
    polygon(cir, col="green", border="white")
    
    #두번째 polygon
    cir <- rbind(cbind(x.cir[(angle3+1):(181-angle1)], y.cir[(angle3+1):(181-angle1)]) ,
                 cbind(0.8*x.cir[(181-angle1):(angle3+1)], 0.8*y.cir[(181-angle1):(angle3+1)]),
                 col="blue")
    polygon(cir,col="blue", border="white")
    
    #세번째 polygon
    cir <- rbind(cbind(x.cir[1:(angle3+1)], y.cir[1:(angle3+1)]) ,
                 cbind(0.8*x.cir[(angle3+1):1], 0.8*y.cir[(angle3+1):1]),
                 col="red")
    polygon(cir,col="red", border="white")   
    
    for (i in 0:36) segments(0,0,cos(i*pi/36),sin(i*pi/36),col="white")
    
    arrows(0,0.1,0.75*cos(pi-x*pi),0.75*sin(pi-x*pi),lwd=3,length=0.2)  # 화살표
    text(0,0.1,"o",cex=3)  # 가운데 동그라미
    title(title)
}

empNames <- sales$name

windows(height=4, width=6)
# par(mfrow=) : 한 윈도우에 여러 개의 plot 그리기
# oma : 각 plot간의 좌,우,상,하 간격.
par(mfrow=c(3,3), oma=c(3,3,3,3), mar=c(1,1,1,2))

for (i in 1:nrow(sales)) {
    dash_t(sales[i,5], 60, 30, empNames[i])
    text(0,0.5, paste(sales[i,5]*100,"%"), cex=1.5)
}
savePlot("22_dashboard.png", type="png")



#----------------------------------------------------------------------------
# barplot / achievement
#----------------------------------------------------------------------------
# 전체 높이 1 기준 달성률 표시 차트

a <- read.csv("data/emp_monthly_score_1.csv", header=T, stringsAsFactors=FALSE, sep=",")

# 달성률:미달성률 (0.9:0.1)
x <- matrix(c(a[1,5], 1-a[1,5],
              a[2,5], 1-a[2,5], 
              a[3,5], 1-a[3,5],
              a[4,5], 1-a[4,5],
              a[5,5], 1-a[5,5],
              a[6,5], 1-a[6,5],
              a[7,5], 1-a[7,5]), 2, 7)

empNames <- a$name
bp <- barplot(x, names=empNames, col=c("blue","red"), main="achievement rate (Jan)")
abline(h=seq(0.05,0.95,0.05), col="white", lwd=2) # 0.5 단위 구분선
text(x=bp, y=a$ratio*0.95, labels =paste(a$ratio*100,"%"), col = "yellow", cex = 1.2)

savePlot("23_bar_achievement.png", type="png")

