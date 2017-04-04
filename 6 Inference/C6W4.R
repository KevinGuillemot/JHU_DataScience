#1
subjects<-data.frame(baseline=c(140,138,150,148,135),weektwo=c(132,135,151,146,130))
t.test(x=subjects$baseline,y=subjects$weektwo,alternative = "two.sided",paired=TRUE)

#2
n<-9
sampleMean<-1100
sampleSd<-30
sampleMean+c(-1,1)*qt(p = 0.025,df = 8)*sampleSd/sqrt(n)

#3
binom.test(x=3, n=4, alt="greater")

#4
poisson.test(x=10,T = 1787,r=0.01,alternative ="less" )

#5
n<-18
nPlac<-9
nTreat<-9
meanPlac<-1
meanTreat<- -3
sdPlac<-1.8
sdTreat<-1.5
sigma<-
t.test(x=)

#7
n<-100
mu_zero<-0
sigma_zero<-0.04
mu_a<-0.01
alpha<-0.05
Decision<-mu_zero+qt(p = 1-alpha,df = n-1)*sigma_zero/sqrt(n)
pt(q = Decision,ncp = mu_a,df = n-1,lower.tail = FALSE)
power.t.test(n=n, delta=mu_a, sd=sigma_zero , sig.level=alpha, type="one.sample", alt="one.sided")$power

#8
power.t.test(delta = 0.01,sd = 0.04,sig.level = 0.05,power = 0.9,type="one.sample", alt="one.sided" )


