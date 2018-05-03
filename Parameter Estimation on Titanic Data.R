mydata <- read.csv('C:/Users/Jordan/Desktop/R Multimodal/titanic.csv',header = TRUE)
mydata <- mydata[-c(1,4,9,11,12)] #remove disinteresting elements
mydata <- mydata[!rowSums(is.na(mydata)) > 0,] #remove all values of n/a in age column

library("coda")


mydata$Survived <- as.vector(mydata$Survived)
mydata$Pclass <- as.vector(mydata$Pclass)
mydata$Sex <- as.numeric(mydata$Sex)
mydata$Sex <- as.vector(mydata$Sex)
mydata$Age <- as.vector(mydata$Age)
mydata$SibSp <- as.vector(mydata$SibSp)
mydata$Parch <- as.vector(mydata$Parch)
mydata$Fare <- as.vector(mydata$Fare)

mydata$Survived <- scale(mydata$Survived)
mydata$Pclass <- as.vector(scale(mydata$Pclass))
mydata$Sex <- as.numeric(mydata$Sex)
mydata$Sex <- as.vector(scale(mydata$Sex))
mydata$Age <- as.vector(scale(mydata$Age))
mydata$SibSp <- as.vector(scale(mydata$SibSp))
mydata$Parch <- as.vector(scale(mydata$Parch))
mydata$Fare <- as.vector(scale(mydata$Fare))




y <- mydata$Survived #-1 = dead, 1 = survived
x1 <- mydata$Pclass #-1 = 1,1 = 3
x2 <- mydata$Sex #female 1, male = 2
x3 <- mydata$Age
x4 <- mydata$SibSp
x5 <- mydata$Parch
x6 <- mydata$Fare

likelihood = function(param){
  a = param[1]
  b = param[2]
  c = param[3]
  d = param[4]
  e = param[5]
  f = param[6]
  #sd = param[3]
  
  pred = a*x1 + b*x2 + c*x3 + d*x4 + e*x5 + f*x6
  singlelikelihoods = dnorm(y, mean = pred, sd = 0.01,log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)
}

prior = function(param){
  a = param[1]
  b = param[2]
  c = param[3]
  d = param[4]
  e = param[5]
  f = param[6]
  #sd = param[3]
  aprior = dnorm(a,mean = -1, sd = .1, log = T)
  bprior = dnorm(b,mean = -1.5, sd = .1, log = T)
  cprior = dnorm(c,mean = -0.7, sd = .1, log = T)
  dprior = dnorm(d,mean = 0, sd = .1, log = T)
  eprior = dnorm(e,mean = 1, sd = .1, log = T)
  fprior = dnorm(f,mean = 1, sd = .1, log = T)
  #sdprior = dunif(sd, min=0, max=30, log = T)
  #print(aprior)
  return(aprior+bprior+cprior+dprior+eprior+fprior)#+sdprior)
}

prior = function(param){
  a = param[1]
  b = param[2]
  c = param[3]
  d = param[4]
  e = param[5]
  f = param[6]
  #sd = param[3]
  aprior = dnorm(a,mean = 0, sd = .1, log = T)
  bprior = dnorm(b,mean = 0, sd = .1, log = T)
  cprior = dnorm(c,mean = 0, sd = .1, log = T)
  dprior = dnorm(d,mean = 0, sd = .1, log = T)
  eprior = dnorm(e,mean = 0, sd = .1, log = T)
  fprior = dnorm(f,mean = 0, sd = .1, log = T)
  #sdprior = dunif(sd, min=0, max=30, log = T)
  #print(aprior)
  return(aprior+bprior+cprior+dprior+eprior+fprior)#+sdprior)
}

proposalfunctionone = function(){
  return(rnorm(1,mean = 0, sd= 0.03))
}

run_metropolis_MCMC_chained = function(startvalue, iterations){
  progressBar <- txtProgressBar(min = 0,max = (iterations),style = 3)
  chain = array(dim = c(iterations+1,6))#3))
  chain[1,] = startvalue
  j <- 1
  for (i in 1:(iterations)){
    for(j in 1:6){
      proposalstep <- proposalfunctionone()
      proposal <- chain[i,]
      proposal[j] <- proposal[j] + proposalstep
      probab = exp(likelihood(proposal) - likelihood(chain[i,]) + prior(proposal) - prior(chain[i,]))
      if (runif(1) < probab){
        chain[i+1,j] = proposal[j]
      }else{
        chain[i+1,j] = chain[i,j]
      }
    }
    setTxtProgressBar(progressBar,(i))
  }
  burntin <- chain[1000:iterations,1:6]
  return(mcmc(burntin))
}

chains <- list()
i<-0
j<- 1
for(i in seq(-10,10,by = 4)){
startvalue = c(i,i,i,i,i,i)
chains[[j]] = run_metropolis_MCMC_chained(startvalue, 240000)
print(j)
j <- j + 1 
}

library("coda")
varnames(chains[[1]]) <- c("x1","x2","x3","x4","x5","x6")
plot(chains[[1]],cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,trace = FALSE)


summary(chains[[1]])
gelman.diag(chains,multivariate = FALSE)

par(mfrow = c(1,1),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

plot(dnorm(seq(-10,10),mean = -1,sd = 5, log = T),xlim = c(0,20),ylim = c(-5,-2),xlab = "x",ylab = "Log(Prior(x))",main = "Prior Distribution, x1",type = 'l')
par(new = TRUE)
plot(dnorm(seq(-10,10),mean = -1.5,sd = 5, log = T),xlim = c(0,20),ylim = c(-5,-2),xlab = "x",ylab = "Log(Prior(x))",main = "Prior Distribution, x2",type = 'l')
par(new = TRUE)
plot(dnorm(seq(-10,10),mean = -0.7,sd = 5, log = T),xlim = c(0,20),xlab = "x",ylim = c(-5,-2),ylab = "Log(Prior(x))",main = "Prior Distribution, x3",type = 'l')
par(new = TRUE)
plot(dnorm(seq(-10,10),mean = 0,sd = 5, log = T),xlim = c(0,20),xlab = "x",ylab = "Log(Prior(x))",ylim = c(-5,-2),main = "Prior Distribution, x4",type = 'l')
par(new = TRUE)
plot(dnorm(seq(-10,10),mean = 1,sd = 5, log = T),xlim = c(0,20),xlab = "x",ylim = c(-5,-2),ylab = "Log(Prior(x))",main = "Prior Distribution, x5",type = 'l')
par(new = TRUE)
plot(dnorm(seq(-10,10),mean = 1,sd = 5, log = T),xlim = c(0,20),xlab = "x",ylab = "Log(Prior(x))",ylim = c(-5,-2),main = "Prior Distribution, x6",type = 'l')

test <- function(){
  return(
 dnorm(seq(-10,10),mean = -1,sd = 5, log = T)+
dnorm(seq(-10,10),mean = -1.5,sd = 5, log = T)+
  dnorm(seq(-10,10),mean = -0.7,sd = 5, log = T)+
  dnorm(seq(-10,10),mean = 0,sd = 5, log = T)+
  dnorm(seq(-10,10),mean = 1,sd = 5, log = T)+
  dnorm(seq(-10,10),mean = 1,sd = 5, log = T))

  }

plot(test(),xlim = c(0,20),xlab = "x",ylab = "Log(Prior(x))",main = "Total Prior Distribution",type = 'l')



#plot(using$ï..Age,using$TotalWorkingYears,xlim = c(18,60),ylim = c(0,50),type = 'p',pch = 20,xlab = "Age",ylab = "Years Spent Working",main = "Graph Showing IBM Employees Age vs Years Spent Working (Anywhere)")
#abline(-18,1,col = "2")

#lm(using$TotalWorkingYears~using$ï..Age)
#abline(lm(using$TotalWorkingYears~using$ï..Age),col = "4")
#par(new = TRUE)

#plot(test,xlim = c(18,60),ylim = c(0,50),col = "3")


#test <- function(x){
#  return(0.5537*x - 9.0838)
#}
