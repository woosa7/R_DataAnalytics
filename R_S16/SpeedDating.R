############################################################
#
# 다변량통계분석 - Speed Dating Data
#
############################################################
library(dplyr)

oData = read.csv("SpeedDatingData.csv", header = T, stringsAsFactors = F)
head(oData)

# Attractive, Sincere, Intelligent, Fun, Ambitious, Has shared interests/hobbies

# before dating 1_1
# the day after 1_2

# iid = wave + id + gender : 1명의 참가자
# 각 참가자별로 1 row. 6가지 항목의 값은 개인별로 동일.
# 결측치 제거
mainData = select(oData, iid, gender, attr1_1, sinc1_1, intel1_1, fun1_1, amb1_1, shar1_1, attr1_2, sinc1_2, intel1_2, fun1_2, amb1_2, shar1_2) %>% 
    distinct(iid, gender, attr1_1, sinc1_1, intel1_1, fun1_1, amb1_1, shar1_1, attr1_2, sinc1_2, intel1_2, fun1_2, amb1_2, shar1_2) %>% 
    filter(!is.na(attr1_1), !is.na(attr1_2), !is.na(shar1_1))

mainData$gender = factor(mainData$gender)
rownames(mainData) = mainData$iid
    
head(mainData, 20)
summary(mainData)



write.csv(mainData, "SpeedDatingMainData.csv", row.names = F)


#-----------------------------------------------------------
# 데이터 탐색
#-----------------------------------------------------------

library(psych)

# before
female_1 = mainData[mainData$gender==0, ][,3:8]
male_1 = mainData[mainData$gender==1, ][,3:8]

head(female_1)

pairs.panels(female_1)
pairs.panels(male_1)

# after
female_2 = mainData[mainData$gender==0, ][,9:14]
male_2 = mainData[mainData$gender==1, ][,9:14]

pairs.panels(female_2)
pairs.panels(male_2)

# heatmap
heatmap(as.matrix(female_1), scale = "column", Colv = NA)
heatmap(as.matrix(male_1), scale = "column", Colv = NA)


#-----------------------------------------------------------
# 주성분분석
#-----------------------------------------------------------

pca_f_1 <- prcomp(female_1, scale = T)
summary(pca_f_1)
pca_f_1
biplot(pca_f_1)

pca_f_2 <- prcomp(female_2, scale = T)
summary(pca_f_2)
pca_f_2
biplot(pca_f_2)





