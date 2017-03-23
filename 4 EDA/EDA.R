# ########################################################################################
# EDA - Exploratory data analysis
# 
# ########################################################################################


# ########################################################################################
# Libraries
# ########################################################################################

#http://www.r-graph-gallery.com/
library(lattice)
library(ggplot2)
library(datasets)

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

#Plot to file
#pdf(file="myplot.pdf")
#plot instrctuins
#dev.off()

###########################################################################################
# Base package

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


#Boxplot 2D
boxplot(mpg ~ cyl, data = myMtcars,main="mpg/cyl",xlab="cyl",ylab="mpg")

#Histograms 2D
nrows=length(unique(myMtcars$cyl))
ncols=1
par(mfrow=c(nrows,ncols))
for (cyl in unique(myMtcars$cyl)){
  hist(myMtcars$mpg[myMtcars$cyl==cyl],main = paste("mpg distribution for",cyl,"cylinders"),xlab="mpg")
  rug(myMtcars$mpg[myMtcars$cyl==cyl])
}

#Scatterplots 2D
with(myMtcars, plot(as.numeric(cyl),mpg,col=carb,main="mpg/cyl with carb"))
legend("topright",legend=levels(factor(myMtcars$carb)),text.col=levels(factor(myMtcars$carb)))

par(mfrow=c(1,2),mar=c(4,4,2,1),oma=c(0,0,2,0))
with(myMtcars,{
  plot(as.numeric(cyl),mpg,main="mpg/cyl")
  plot(carb,mpg,main="mpg/carb")
  mtext("mpg for cyl and carb",outer = TRUE)
})

nrows=1
ncols=length(levels(myMtcars$cyl))
par(mfrow=c(nrows,ncols))
for (cyli in levels(myMtcars$cyl)){
  with(subset(myMtcars,cyl==cyli),plot(carb,mpg,main=cyli))
}

#smooth scatter plot, for a lot of data
smoothScatter(x,y)


###########################################################################################
# Lattice

# Scatterplot
xyplot(Ozone ~ Wind,data=airquality)

#Multiple scatter
airquality2 <- transform(airquality,Month=factor(Month))
xyplot(Ozone ~ Wind | Month,data=airquality)

xyplot(Ozone ~ Wind | Month,data=airquality,panel = function(Wind,Ozone,...){
  panel.xyplot(Wind,Ozone,...)
  panel.abline(h=median(Ozone),lty=2)
})


###########################################################################################
# ggplot2

str(mpg)

#Histogram
qplot(hwy,data=mpg,fill=drv)
qplot(hwy,data=mpg,geom="density",color=drv)
#Facets determine the pannels, pannels split data using factors along rows or columns
qplot(hwy,data=mpg,facets=.~drv)
qplot(hwy,data=mpg,facets=drv~.)

#Scatterplot
qplot(displ,hwy,data=mpg,color=drv)
qplot(displ,hwy,data=mpg,shape=drv)
qplot(displ,hwy,data=mpg,facets=.~drv)
qplot(displ,hwy,data=mpg,geom = c("point","smooth"))
qplot(displ,hwy,data=mpg,color=drv)+geom_smooth(method="lm")
qplot(displ,hwy,data=mpg,facets=.~drv)+geom_smooth(method="lm")

#ggplot function
#Define data
g<-ggplot(mpg, aes(displ,hwy))
g+geom_point()
g+geom_point()+theme_bw()
g+geom_point()+labs(title="hwy")+labs(x="displ",y="hwy")
g+geom_point(color="steelblue",size=2,alpha=0.5)
g+geom_point()+geom_smooth()
g+geom_point()+geom_smooth(method = "lm")
g+geom_point(aes(color=drv))+geom_smooth(method = "lm")
g+geom_point(aes(color=drv))+geom_smooth(aes(color=drv),method = "lm")
g+geom_point()+facet_grid(.~drv)+geom_smooth(method = "lm")
#Use coord cart otherwise will just subset what fits into the ylim
g+geom_point()+coord_cartesian(ylim = c(-3,3))

#If one of the independant variables is continous we can't directly use in graph or it would produce an infinity
#Calculate decides of the data
cutpoints<- quantile(mpg$cty, seq(0,1,length=4),na.rm = TRUE)

#Cut the data and retrieve factors
mpg$ctyFactor<-cut(mpg$cty,cutpoints)

#See levels
levels(mpg$ctyFactor)

library(ggplot2movies)
g <- ggplot(movies, aes(votes, rating))
print(g)


qplot(Wind, Ozone, data = airquality, facets = . ~ factor(Month))

airquality2 = transform(airquality, Month = factor(Month))
qplot(Wind, Ozone, data = airquality2, facets = . ~ Month)


#Hierachical clustering
dataFrame<- data.frame(x=x,y=y)
distxy<-dist(dataFrame)
hClustering <- hclust(distxy)
plot(hClustering)

#Heatmap
#Runs hierarchical cluster on rows and columns of a table
dataFrame<- data.frame(x=x,y=y)
dataMatrix<-as.matrix(dataFrame)[nrows, ncols]
heatmap(dataMatrix)

#Kmeans
dataFrame<- data.frame(x=x,y=y)
kmeansObj<-kmeans(dataFrame, centers = 3)
kmeansObj$cluster
kmeansObj$centers
plot(x,y,col=kmeansObj$cluster,pch=19)
points(kmeansObj$centers,col=1:3,pch=3)

#Imputing
library(impute)
dataMatrix2<-impute.knn(dataMatrix2)$data

