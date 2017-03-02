library(dplyr)

df = read.csv("US_Election2012.csv", header = T, stringsAsFactors = F)
colnames(df) = c('state','county', 'fips', 'vote1', 'Obama', 'vote2', 'Romney')
df$vote1 = as.numeric(df$vote1)
df$vote2 = as.numeric(df$vote2)
df$Obama = as.numeric(df$Obama)
df$Romney = as.numeric(df$Romney)

head(df)
summary(df)

df_elect = df %>% filter(fips > 0) %>% 
    mutate(total = vote1 + vote2,
           margin = ifelse(Obama >= Romney, Obama - Romney, Romney - Obama),
           turnout = 100 - Obama - Romney,
           winner = ifelse(Obama >= Romney, 'Obama', 'Romney')) %>% 
    select(state, county, Obama, Romney, winner, total, margin, turnout)

head(df_elect)

write.csv(df_elect, "election2012.csv", row.names = F, fileEncoding = 'utf-8')
