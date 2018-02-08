library(ggplot2)
data = read.table("https://www.math.ntnu.no/emner/TMA4268/2018v/data/SYSBPreg3uid.txt")
dim(data)
colnames(data)
modelA=lm(-1/sqrt(SYSBP) ~ .,data = data)
summary(modelA)

modelB=lm(SYSBP ~ .,data = data)
summary(modelB)

# residuls vs fitted
ggplot(modelA, aes(.fitted, .resid)) + geom_point(pch = 21) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE, col = "red", size = 0.5, method = "loess") +
  labs(x = "Fitted values", y = "Residuals", title = "Fitted values vs. residuals", subtitle = deparse(modelA$call))
# qq-plot of residuals
ggplot(modelA, aes(sample = .stdresid)) +
  stat_qq(pch = 19) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  labs(x = "Theoretical quantiles", y = "Standardized residuals", title = "Normal Q-Q", subtitle = deparse(modelA$call))
# normality test
library(nortest)
ad.test(rstudent(modelA))

# residuls vs fitted
ggplot(modelB, aes(.fitted, .resid)) + geom_point(pch = 21) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE, col = "red", size = 0.5, method = "loess") +
  labs(x = "Fitted values", y = "Residuals", title = "Fitted values vs. residuals", subtitle = deparse(modelB$call))
# qq-plot of residuals
ggplot(modelB, aes(sample = .stdresid)) +
  stat_qq(pch = 19) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  labs(x = "Theoretical quantiles", y = "Standardized residuals", title = "Normal Q-Q", subtitle = deparse(modelB$call))
# normality test
library(nortest)
ad.test(rstudent(modelB))

names(data)
new=data.frame(SEX=1,AGE=56,CURSMOKE=1,BMI=89/1.75^2,TOTCHOL=200,BPMEDS=0)

#Best guess for 1/sqrt(SYSBP):
beta = modelA$"coefficients"
x = new
best_guess = beta["(Intercept)"]+beta["SEX"]*x["SEX"]+beta["AGE"]*x["AGE"]+beta["CURSMOKE"]*x["CURSMOKE"]+beta["BMI"]*x["BMI"]+beta["TOTCHOL"]*x["TOTCHOL"]+beta["BPMEDS"]*x["BPMEDS"]
best_guess
#betab = modelB$"coefficients"
#best_guessb = betab["(Intercept)"]+betab["SEX"]*x["SEX"]+betab["AGE"]*x["AGE"]+betab["CURSMOKE"]*x["CURSMOKE"]+betab["BMI"]*x["BMI"]+betab["TOTCHOL"]*x["TOTCHOL"]+betab["BPMEDS"]*x["BPMEDS"]
#best_guessb

predict(modelA,newdata=new,interval ="prediction",type="response",level=0.90)
"Limits of prediction interval for SYSBP:"
lower = 1/(predict(modelA,newdata=new,interval ="prediction",type="response",level=0.90)[2]^2)
lower
upper = 1/(predict(modelA,newdata=new,interval ="prediction",type="response",level=0.90)[3]^2)
upper

