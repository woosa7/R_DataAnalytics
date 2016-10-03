library(dplyr)
library(lubridate)
library(ggplot2)

cs <- read.delim("HDS_Customers.tab", stringsAsFactors = F)
tr <- read.delim("HDS_Transactions_MG.tab", stringsAsFactors = F)
card <- read.delim("HDS_Cards.tab", stringsAsFactors = F)
job <- read.delim("HDS_Jobs.tab", stringsAsFactors = F)

head(tr)
summary(tr)

sort(tr$net_amt[tr$net_amt>0], decreasing = T)

head(cs)

tr %>% group_by(inst_mon) %>% summarise(n = n())
dim(tr)
