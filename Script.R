setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library("tidyverse")
library("data.table")
library("stargazer")
library("here")
library("vtable")
library("ivpack")
library("ggplot2")

#Import the data
dt <- read.table(file="Voting.csv", sep = ",", header=TRUE)
dt <- as.data.table(dt)

#Create average temperature columns
dt[, "ave_temp":= 0.5*(dt[, "maxTempC"]+dt[, "minTempC"])]
dt[, "ave_temp1015":= 0.5*(dt[, "maxTempC1015"]+dt[, "minTempC1015"])]

#Run first model
model1 <- lm(turnout ~ prcpMm+prcpMm1015+ave_temp+ave_temp1015, data=dt)

#Run second model (IV)
model2 <- ivreg(shareRep ~ turnout+prcpMm1015+ave_temp1015+prcpMm| prcpMm+prcpMm1015+ave_temp+ave_temp1015, data=dt)

#Plot first result
turnout <- dt[,"turnout"]
ave_temp <- dt[,"ave_temp"]
#Convert to vectors for ease of plotting
turnout_v <- turnout$turnout
ave_temp_v <- ave_temp$ave_temp
plot(ave_temp_v, turnout_v, main="Effect of Temperature on Turnout", xlab= "Temperature (Celsius)", ylab="
     Turnout (%)")
abline(model1$coefficients[1],model1$coefficients[4], col="blue")

#Output tables in latex
stargazer(model1,type="latex")
stargazer(model2,type="latex")
