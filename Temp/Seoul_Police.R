library(dplyr)
library(reshape2)

# ----------------------------------------------------------------------
# cdata = read.csv("seoul_police.csv", stringsAsFactors = F)
# cdata$year = as.character(cdata$year)
# head(cdata)
# summary(cdata)
# 
# crime = cdata %>% 
#     group_by(gu, year) %>% 
#     summarise_each(funs(sum), m_occur, m_arrest, rob_occur, rob_arrest, rap_occur, rap_arrest, 
#                    thf_occur, thf_arrest, v_occur, v_arrest)
#     
# write.csv(crime, "crime.csv", row.names = FALSE)
# ----------------------------------------------------------------------

cdata = read.csv("crime_origin.csv", stringsAsFactors = F)
cdata$year = as.character(cdata$year)
head(cdata)
summary(cdata)

crime01 = cdata %>% mutate(ctype = 'murder')    %>% select(gu, year, ctype, occur = m_occur, arrest = m_arrest)
crime02 = cdata %>% mutate(ctype = 'robbery')   %>% select(gu, year, ctype, occur = rob_occur, arrest = rob_arrest)
crime03 = cdata %>% mutate(ctype = 'rape')      %>% select(gu, year, ctype, occur = rap_occur, arrest = rap_arrest)
crime04 = cdata %>% mutate(ctype = 'theft')     %>% select(gu, year, ctype, occur = thf_occur, arrest = thf_arrest)
crime05 = cdata %>% mutate(ctype = 'violence')  %>% select(gu, year, ctype, occur = v_occur, arrest = v_arrest)

crime = rbind(crime01, crime02, crime03, crime04, crime05)
crime = crime %>% mutate(ratio = ifelse(arrest / occur > 1, 1, arrest / occur))

summary(crime)

write.csv(crime, "seoulcrime.csv", row.names = FALSE)

# ----------------------------------------------------------------------
