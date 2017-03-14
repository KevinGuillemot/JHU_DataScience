# ########################################################################################
# EDA - Exploratory data analysis
# 
# ########################################################################################


# ########################################################################################
# Libraries
# ########################################################################################

#http://www.r-graph-gallery.com/

###########################################################################################
# Data
###########################################################################################

#Get built in R dataset
data("mtcars")

###########################################################################################
# Data cleaning
###########################################################################################

#Data structure
str(mtcars)

#Preview
head(mtcars)

#Check for NA
sum(is.na(mtcars))

#Tranform numerical to factors if necessary
myMtcars<-transform(mtcars,cyl=factor(cyl))

###########################################################################################
# EDA
###########################################################################################

#Close all graphs
graphics.off()

###########################################################################################
# 1 Dimension

#Distribution summary
summary(myMtcars)

#Boxplot
boxplot(myMtcars$mpg,main="mpg",ylab="mpg")
abline(h=25)

#Histogram
hist(myMtcars$mpg, breaks = 10,main="mpg",xlab="mpg")
abline(v=median(myMtcars$mpg),col="red")
rug(myMtcars$mpg)

#Bar plot
barplot(table(myMtcars$cyl),main="Numer of cars for each cylinder",xlab="cyl",ylab="Observations")

###########################################################################################
# >1 Dimensions

#Boxplot
boxplot(mpg ~ cyl, data = myMtcars,main="mpg/cyl",xlab="cyl",ylab="mpg")

#Histograms
nrows=length(unique(myMtcars$cyl))
ncols=1
par(mfrow=c(nrows,ncols))
for (cyl in unique(myMtcars$cyl)){
  hist(myMtcars$mpg[myMtcars$cyl==cyl],main = paste("mpg distribution for",cyl,"cylinders"),xlab="mpg")
  rug(myMtcars$mpg[myMtcars$cyl==cyl])
}

#Scatterplots
with(myMtcars, plot(as.numeric(cyl),mpg,col=carb,main="mpg/cyl with carb"))
legend("topright",legend=levels(factor(myMtcars$carb)),text.col=levels(factor(myMtcars$carb)))



nrows=1
ncols=length(unique(myMtcars$cyl))
par(mfrow=c(nrows,ncols))
for (cyli in unique(myMtcars$cyl)){
  with(subset(myMtcars,cyl==cyli),plot(carb,mpg,main=cyli))
}

