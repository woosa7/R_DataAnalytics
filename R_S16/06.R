#------------------------------------------
# 분산분석 ANOVA

movie <- read.csv("movie_MBA.csv", header = T)
summary(movie)
levels(movie$rating)

out <- lm(log(total_seen)~rating, movie)
summary(out)

install.packages("multcomp")
library(multcomp)


dunnet <- glht(out, linfct = mcp(rating="Dunnett"))
summary(dunnet)

tukey <- glht(out, linfct = mcp(rating="Tukey"))
summary(tukey)


#-------------------
movie$rating2 <- movie$rating
levels(movie$rating2)

levels(movie$rating2) <- c(2,2,1,3) #12세 15세 합치기
summary(movie$rating2)

out2 <- lm(log(total_seen)~rating2, movie)
summary(out2)

movie$rating2 <- relevel(movie$rating2, ref="1")
out2 <- lm(log(total_seen)~rating2, movie)
summary(out2)


#-------------------------
# 공분산분석

df <- read.csv("anorexia.csv")
str(df)

boxplot(Prewt~Treat, df)

out <- lm(Postwt-Prewt ~ Treat, df)
summary(out)

# 이전 몸무게가 동일한 사람들을 기준으로...
out <- lm(Postwt-Prewt ~ Prewt+ Treat, df)
summary(out)
anova(out)


#---------------------------
# exercise : forbes

df <- read.csv("Forbes500.csv")
str(df)

boxplot(Sales~sector, df)
boxplot(log(Sales)~sector, df)

model <- lm(log(Sales)~sector, df)
summary(model)


model <- lm(log(Sales)~log(Assets)+sector, df)
summary(model)
dunnet <- glht(model, linfct = mcp(sector="Dunnett"))
summary(dunnet)


df$sector <- factor(df$sector)
df$sector <- relevel(df$sector, ref = "HiTech")

model2 <- lm(log(Sales)~Assets+sector, df)
summary(model2)
dunnet2 <- glht(model2, linfct = mcp(sector="Dunnett"))
summary(dunnet2)


