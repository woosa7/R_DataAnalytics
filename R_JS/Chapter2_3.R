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
# dash board
#----------------------------------------------------------------------------

sales <- read.csv("data/emp_monthly_score_1.csv", header=T, stringsAsFactors=FALSE, sep=",")
sales

# function
# x : 표현할 값, angle1 : 좌측 게이지 각도, angle3 : 우측 게이지 각도, title : Dashboard 제목

dash_t <- function(x, angle1, angle3, title) {
    i <- matrix(c(-1,0,0,1,1,0), 3, 2, byrow=T)
    plot(i, xlab="", ylab="", axes=F, type="n")
    x.cir <- cos(seq(0,180,1)*pi/180)
    y.cir <- sin(seq(0,180,1)*pi/180)
    
    #첫번째 polygon
    cir <- rbind(cbind(x.cir[(181-q.1):181], y.cir[(181-q.1):181]) ,
                 cbind(0.8*x.cir[181:(181-q.1)], 0.8*y.cir[181:(181-q.1)]),
                 col="green")
    polygon(cir, col="green", border="white")
    
    #두번째 polygon
    cir <- rbind(cbind(x.cir[(q.3+1):(181-q.1)], y.cir[(q.3+1):(181-q.1)]) ,
                 cbind(0.8*x.cir[(181-q.1):(q.3+1)], 0.8*y.cir[(181-q.1):(q.3+1)]),
                 col="blue")
    polygon(cir,col="blue", border="white")
    
    #세번째 polygon
    cir <- rbind(cbind(x.cir[1:(q.3+1)], y.cir[1:(q.3+1)]) ,
                 cbind(0.8*x.cir[(q.3+1):1], 0.8*y.cir[(q.3+1):1]),
                 col="red")
    polygon(cir,col="red", border="white")   
    
    for (i in 0:36) segments(0,0,cos(i*pi/36),sin(i*pi/36),col="white")
    
    arrows(0,0.1,0.75*cos(pi-x*pi),0.75*sin(pi-x*pi),lwd=3,length=0.2)
    text(0,0.1,"o",cex=3)
    title(title)
}

emps <- sales$name

windows(height=4, width=6)
par(mfrow=c(3,3), oma=c(3,3,3,3), mar=c(1,1,1,2))   # oma 값은 각 좌,우,상,하 간격.

dash_t(sales[1,5],60,30, emps[1])
text(0,0.5, paste(sales[1,5]*100,"%"), cex=1.5)

dash_t(sales[2,5],60,30, emps[2])
text(0,0.5,paste(sales[2,5]*100,"%"),cex=1.5)

dash_t(sales[3,5],60,30, emps[3])
text(0,0.5,paste(sales[3,5]*100,"%"),cex=1.5)

dash_t(sales[4,5],60,30, emps[4])
text(0,0.5,paste(sales[4,5]*100,"%"),cex=1.5)

dash_t(sales[5,5],60,30, emps[5])
text(0,0.5,paste(sales[5,5]*100,"%"),cex=1.5)

dash_t(sales[6,5],60,30, emps[6])
text(0,0.5,paste(sales[6,5]*100,"%"),cex=1.5)

dash_t(sales[7,5],60,30, emps[7])
text(0,0.5,paste(sales[7,5]*100,"%"),cex=1.5)


#----------------------------------------------------------------------------

a <- read.csv("data/emp_monthly_score_1.csv", header=T, stringsAsFactors=FALSE, sep=",")

# 아래 그래프 작업의 핵심은 전체를 1 로 보고 달성율을 1-달성율로 계산한것입니다.
# 그래서 달성한 부분은 초록색으로 표시하고 1 - 달성율 부분은 파란색으로 표시했습니다.
# 아주 많이 사용되는 방법이므로 꼭 이해해야 합니다!

x <- matrix(c(a[1,5],1-a[1,5],
              a[2,5],1-a[2,5], 
              a[3,5],1-a[3,5],
              a[4,5],1-a[4,5],
              a[5,5],1-a[5,5],
              a[6,5],1-a[6,5],
              a[7,5],1-a[7,5]),2,7)

bp <- barplot(x,names=c(a[1,2],a[2,2],a[3,2],a[4,2],a[5,2],a[6,2],a[7,2]),col=3:5, main="1월 사원별 성과 달성률")
abline(h=seq(0.05,0.95,0.05),col="white",lwd=2)

text(x=bp, y =a$ratio*0.95, labels =paste(a$ratio*100,"%"), col = "red", cex = 1.2)




