library(UsingR)

data("diamond")

fit<-lm(diamond$price ~ diamond$carat)
e<-resid(fit)
yhat<-predict(fit)
nd<-data.frame(residuals=e,diamond)
ggplot(data = nd,aes(x=carat,y=residuals))+geom_point()

#Estimated Variance of residuals
sigma<-sqrt(sum(e)^2/(n-2))

#Estimated Variance of b0
ssx<-sum((diamond$carat-mean(diamond$carat))^2)
seBeta0 <- sqrt(1/n + mean(diamond$carat)^2 /ssx )*sigma
seBeta1 <- sigma/sqrt(ssx)

#Tstats
tBeta0 <-beta0

#1
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit<-lm(y~x)
summary(fit)

#2
summary(lm(y ~ x))$sigma

#3
rawCars<-mtcars
fitCars<-lm(mpg ~ wt,data =rawCars )
summary(fitCars)

fit <- lm(mpg ~ I(wt - mean(wt)), data = rawCars)
confint(fit)

newData=data.frame(wt=mean(rawCars$wt))
predict(fitCars,newData,interval="confidence")

#4
?mtcars

#5
newData=data.frame(wt=3)
predict(fitCars,newData,interval="prediction")

#6
fit <- lm(mpg ~ wt, data = mtcars)
confint(fit)[2, ] * 2

#8
