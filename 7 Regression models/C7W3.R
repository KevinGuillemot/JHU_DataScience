# ########################################################################################
# C7W3 multiple regression
# 
# ########################################################################################

# Clear Environment
rm(list = ls())
#Close all graphs
graphics.off()

# ########################################################################################
# Libraries
# ########################################################################################

library(ggplot2)
library(GGally)
library(datasets)



###########################################################################################
# Data
###########################################################################################

#Get built in R dataset
data(swiss)



###########################################################################################
# Data cleaning
###########################################################################################



###########################################################################################
# EDA
###########################################################################################

ggpairs(swiss,lower=list(continuous="smooth"))


###########################################################################################
# Modeling
###########################################################################################

#Feritility vs Agriculture
summary(lm(Fertility~Agriculture,data=swiss))$coefficients
#Agriculture is 0.19

#Feritility vs all the rest
summary(lm(Fertility~.,data=swiss))$coefficients
#Agraciluture is -0.1721
#It changed sign compared to previous model, because other variables are now held constant
#Previous model was incomplete
#If include a variable that is combination of others then it gives back NA

#Residuals
plot(lm(Fertility~.,data=swiss))



#Residuals

#Create data
n<-100
x<-c(10,rnorm(n))
y<-c(10,rnorm(n))
ggplot(data=data.frame(x,y),aes(x,y))+
  geom_point()+
  geom_smooth(method = "lm")
#One outlier suggesting a non existant linear relationship in the data
fit<-lm(y~x)
summary(fit)

#dfbetas: change in the coeff when ith is removed
dfbetas(fit)
ggplot(data=data.frame(dfbetas(fit)),aes(y=x,x=seq(1,nrow(dfbetas(fit)))))+geom_point()
# Outlier is cleary flagged

#Hatvalues: measure of leverage normalized on [0:1]
hatvalues(fit)
ggplot(data=data.frame(x=hatvalues(fit)),aes(y=x,x=seq(1,length(hatvalues(fit)))))+geom_point()
# Outlier flagged