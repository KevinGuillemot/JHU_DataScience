library(datasets)
data(iris)
?iris

mean(iris$Sepal.Length[iris$Species=="virginica"])

colMeans(iris)
apply(iris, 2, mean)
apply(iris[, 1:4], 1, mean)
rowMeans(iris[, 1:4])
apply(iris, 1, mean)
apply(iris[, 1:4], 2, mean)

data(mtcars)
?mtcars
str(mtcars)
tapply(mtcars$mpg,mtcars$cyl,mean)
s<-split(mtcars,mtcars$cyl)
lapply(s,function(col) mean(col$mpg))
a<-tapply(mtcars$hp,mtcars$cyl,mean)