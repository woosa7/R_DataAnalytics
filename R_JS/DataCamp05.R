#########################################################
#
#   DataCamp : Open Course
#   Data Analysis and Statistical Inference
#
#########################################################

#--------------------------------------------------------
# Behavioral Risk Factor Surveillance System (BRFSS)
#--------------------------------------------------------

load(url("http://assets.datacamp.com/course/dasi/cdc.Rdata"))

str(cdc)
summary(cdc)

# relative frequency
table(cdc$genhlth) / nrow(cdc)

gender_smokers = table(cdc$gender, cdc$smoke100)
mosaicplot(gender_smokers)

mosaicplot(table(cdc$genhlth, cdc$smoke100))

bmi = cdc$weight / (cdc$height ^ 2) * 703
boxplot(bmi~cdc$genhlth)

hist(bmi, breaks=50)

plot(cdc$weight, cdc$wtdesire)


#--------------------------------------------------------
# Hot Hand Phenomenon
# 이전에 던진 슛이 성공한 경우 다음 슛 역시 성공할 것이라고 믿는 현상.
#--------------------------------------------------------

load(url("http://assets.datacamp.com/course/dasi/kobe.RData"))
head(kobe)

table(kobe$basket)
58/nrow(kobe)

# 슈팅 성공 연속 횟수
kobe_streak <- calc_streak(kobe$basket)
kobe_streak
table(kobe_streak)

# 이전 슛 성공여부는 다음 슛 성공여부와 상관없다 : 독립적
# 이전 슛 성공은 다음 슛 성공 확률을 높인다 : 비독립적

# 슛 성공 확률
P(shot1=H) = 0.45
# hot hand일 경우 첫 슛이 성공했을 때 두번째 슛 성공 확률
P(shot2=H | shot1=H) > 0.45

# simulation - independent shooter
outcomes <- c("H", "M")
sim_basket <- sample(outcomes, size = 133, replace = TRUE, prob = c(0.45, 0.55))
sim_streak <- calc_streak(sim_basket)

barplot(table(kobe_streak))
barplot(table(sim_streak))
# 두 테이블의 분산이 비슷하므로 Kobe는 hot hands라고 할 수 없다.


#----------------------------------------------
# Sampling distributions - point estimate
#----------------------------------------------

# 아이오와 주 에임스 시 부동산 데이터
load(url("http://assets.datacamp.com/course/dasi/ames.RData"))
head(ames)
dim(ames)
names(ames)


area <- ames$Gr.Liv.Area   # ground living area

summary(area)
hist(area)

# sample size가 증가할수록 그 분포의 중심이 실제 모집단 평균에 대해 신뢰성이 높아진다.
# 샘플 분포의 가변성이 줄어듬.

sample_means10 <- rep(NA, 5000)
sample_means50 <- rep(NA, 5000)
sample_means100 <- rep(NA, 5000)

for (i in 1:5000) {
    samp <- sample(area, 10)
    sample_means10[i] <- mean(samp)
    samp <- sample(area, 50)
    sample_means50[i] <- mean(samp)
    samp <- sample(area, 100)
    sample_means100[i] <- mean(samp)
}

xlimits <- range(sample_means10)

par(mfrow = c(3, 1))
hist(sample_means10, breaks = 20, xlim = xlimits)
hist(sample_means50, breaks = 20, xlim = xlimits)
hist(sample_means100, breaks = 20, xlim = xlimits)


#----------------------------------------------
# confidence intervals
#----------------------------------------------

samp_mean <- rep(NA, 50)
samp_sd <- rep(NA, 50)
n <- 60

for (i in 1:50) {
    samp <- sample(area, n) 
    samp_mean[i] <- mean(samp)
    samp_sd[i] <- sd(samp)
}

# 95% confidence intervals
se <- samp_sd/sqrt(n)
lower <- samp_mean - 1.96 * se
upper <- samp_mean + 1.96 * se

library(ggplot2)

confint.dat = data.frame(samp_mean, lower, upper, i = 1:length(samp_mean))
confint.dat$excl0 = ifelse(confint.dat$upper < mean(area) | 
                               mean(area) < confint.dat$lower, 1, 0)
confint.dat$excl0 = factor(confint.dat$excl0)

ggplot(data = confint.dat, aes(x = i, y = samp_mean, ymin = lower, ymax = upper, colour = excl0)) + 
    geom_pointrange() + 
    scale_x_continuous(breaks = NULL) + geom_hline(yintercept = mean(area)) + 
    scale_colour_manual(values = c("black", "red")) + guides(colour = FALSE) + 
    ylab("Sample mean") + xlab("") + coord_flip()


