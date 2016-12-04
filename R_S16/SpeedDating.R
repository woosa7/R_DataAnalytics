############################################################
#
# 다변량통계분석 - Speed Dating Data
#
############################################################
library(dplyr)

oData = read.csv("data/SpeedDatingData.csv", header = T, stringsAsFactors = F)
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



write.csv(mainData, "data/SpeedDatingMainData.csv", row.names = F)


#-----------------------------------------------------------
# 데이터 탐색
#-----------------------------------------------------------

library(psych)

# before
all_1 = mainData[,c(2,3:8)]
female_1 = mainData[mainData$gender==0, ][,3:8]
male_1 = mainData[mainData$gender==1, ][,3:8]

pairs.panels(all_1[,-1])
pairs.panels(female_1)
pairs.panels(male_1)

# after
all_2 = mainData[,c(2,9:14)]
female_2 = mainData[mainData$gender==0, ][,9:14]
male_2 = mainData[mainData$gender==1, ][,9:14]

pairs.panels(all_2[,-1])
pairs.panels(female_2)
pairs.panels(male_2)

# heatmap
heatmap(as.matrix(all_1[,-1]), scale = "column", Colv = NA)
heatmap(as.matrix(all_2[,-1]), scale = "column", Colv = NA)



#-----------------------------------------------------------
# 주성분분석
#-----------------------------------------------------------
library(ggfortify)

pca_all_1 = prcomp(all_1[,-1], scale = T)
summary(pca_all_1)
pca_all_1

autoplot(pca_all_1, data = all_1, colour = 'gender', shape = F, label = T, loadings = T, 
         label.size = 7, loadings.label.size = 5, main = "PCA / Before Dating",
         loadings.label = T, loadings.colour = "blue", loadings.label.colour = "blue")


pca_all_2 = prcomp(all_2[,-1], scale = T)
summary(pca_all_2)
pca_all_2

autoplot(pca_all_2, data = all_2, colour = 'gender', shape = F, label = T, loadings = T, 
         label.size = 7, loadings.label.size = 5, main = "PCA / After Dating",
         loadings.label = T, loadings.colour = "blue", loadings.label.colour = "blue")
