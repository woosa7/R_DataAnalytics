
setwd("c:/R_Study/R_KMU")
setwd("/Volumes/MacHDD/workspace/R_Study/R_JS")

windows(800, 600, pointsize = 12)   # 별도의 윈도우 열기
savePlot("aa.png", type="png")    # 결과물을 그림으로 저장
dev.off()                           # 윈도우 닫기

install.packages("stringr")

