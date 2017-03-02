library(dplyr)
library(reshape)

data = read.csv("WDI_Data.csv", stringsAsFactors = F)
dev_data = data %>% filter(X1960 > 0)

head(dev_data)
nrow(dev_data)

mdata = melt(dev_data, id = c("CountryName", "CountryCode", "IndicatorName", "IndicatorCode"))

# Total Population, Urban population (% of total)
target = c("SP.POP.TOTL", "SP.URB.TOTL.IN.ZS")

mdata2 = mdata %>% filter(value > 0) %>% 
    filter(IndicatorCode %in% target) %>% 
    mutate(Year = substr(as.character(variable),2,5),
           TotalPop = ifelse(IndicatorCode == "SP.POP.TOTL", value, 0),
           UrbanPopRatio = ifelse(IndicatorCode == "SP.URB.TOTL.IN.ZS", value, 0)) %>% 
    select(CountryName, CountryCode, Year, TotalPop, UrbanPopRatio) %>% 
    group_by(CountryName, CountryCode, Year) %>% 
    summarise(TotalPop = sum(TotalPop), UrbanPopRatio = sum(UrbanPopRatio))

head(mdata2)
nrow(mdata2)

write.csv(mdata2, "ind_pop_data.csv", row.names = F, fileEncoding = "UTF-8")

