
tick = read.csv("exampledata.csv", stringsAsFactors = F)
tick

plot(tick$Tickets ~ tick$Employees)
plot(tick$Tickets ~ tick$Contract)

out <- lm(Tickets ~ Employees + Contract, data = tick)
summary(out)

predict(out, data.frame("Employees" = c(750), "Contract" = c(13000)), 
        interval = "confidence")


-468 + (0.067 * 5132) + (1349 * 33.4) - (14.4 x midwest) - (9.3 x southeast) + (16.5 x west)

-468 + (0.067 * 5132) + (1349 * 33.4 / 100) + (16.5 * 1)

-468 + (0.067 * 5889) + (1349 * 31.8 / 100)
