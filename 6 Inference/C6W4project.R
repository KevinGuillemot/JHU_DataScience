#https://github.com/alex23lemm/Statistical-inference-project

# Part 1

#Exponential distribution
lambda<-0.2
popMean<-1/lambda
popSd<-1/lambda
n<-40
nbSimulations<-1000
expSamples<-rexp(nbSimulations, lambda)
sampleMeans<-replicate(nbSimulations,mean(rexp(n, lambda)))
meanSampleMeans<-mean(sampleMeans)
sdSampleMeans<-sd(sampleMeans)

ggplot(data.frame(means=sampleMeans),aes(means))+
  geom_density() +
  geom_vline(xintercept = popMean,colour="blue")+
  geom_vline(xintercept = meanSampleMeans,colour="red")+ 
  labs(title = "Distribution of sample means")+
  annotate("text",x=c(popMean-1,meanSampleMeans+0.5),y=0,label=c("Population mean","Mean of samples"),hjust=0, colour=c("blue","red"))

#Show the sample mean and compare it to the theoretical mean of the distribution
popMean
meanSampleMeans
# very close, sample mean is an unbiased estimator of population mean

#Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
popSd
sdSampleMeans
popSd/sqrt(n)

#Show that the distribution of sample means is approximately normal
y<- quantile(sampleMeans,c(0.25,0.75))
x<- qnorm(c(0.25, 0.75))
slope<-diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
ggplot(data.frame(means=sampleMeans),aes(sample=means))+
  stat_qq()+
  geom_abline(slope=slope,intercept=int)
# almost normal for mean of 40 samples

#Compared to 1000 generated exp random variables
y<- quantile(expSamples,c(0.25,0.75))
x<- qnorm(c(0.25, 0.75))
slope<-diff(y)/diff(x)
int <- y[1L] - slope * x[1L]
ggplot(data.frame(s=expSamples),aes(sample=s))+
  stat_qq()+
  geom_abline(slope=slope,intercept=int)




#Part 2
library(datasets)
rawData<-ToothGrowth

str(rawData)
#Len desnity. more or less unimodal, no scaling required
ggplot(rawData,aes(len))+
  geom_density()

# Factors are in same proportion in the dataset. looks properly randomized
prop.table(table(select(rawData,supp,dose)))

#OJ seems slightly more potent than VC
ggplot(rawData,aes(x=supp,y=len))+
  geom_boxplot()

#Behavior seems different depending supp, effects will be analysed separately
ggplot(rawData,aes(x=dose,y=len))+
  geom_point(aes(colour = supp))

#increase with dose. Might be a max threshold with OJ
ggplot(rawData,aes(x=factor(dose),y=len))+
  geom_boxplot()+
  facet_grid(. ~ supp)

#tooth groth by supp
t.test(formula=rawData$len~rawData$supp)
#No statistical significance. pvalue 6% , confidence interval includes 0

t.test(formula=rawData$len~rawData$dose)