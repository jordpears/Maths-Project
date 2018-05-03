library(tidyverse)
library(pracma)
library(utils)
library(coda)


f <- function(x) { #function to turn into probability density
  ifelse (x>-10 & x<20,0.2*dnorm(x,2,0.5)+0.1*dnorm(x,-3,0.8)+0.1*dnorm(x,5,1)+0.2*dnorm(x,7,0.3)+0.2*dnorm(x,9,2)+0.2*dnorm(x,5,2),0)
}

bottom <- 0 #lower limit
top <- 2*pi+0.1 #upper limit

integral <- quad(f,bottom,top) #determine normalising constant 

probDensity <- function(x) { #generate probability density function by dividing each value by the normalising constant
  f(x)/integral
}

verifyProbDensity <- quad(probDensity,bottom,top) #should = 1.

par(mfrow = c(1,1))

plot(probDensity,-10,20,main = "Multi modal Probability Distribution",xlab = "x",ylab = "P(x)")

totalIn <<- 0

countSwitchProb <- function(oldVal,newVal){
  if(oldVal < barrierLeft){
    if(newVal > barrierRight){
      totalIn <<- totalIn + 1
    }
  }
  else {
    if(newVal < barrierLeft){
      totalIn <<- totalIn + 1
    }
  }
}

stepSize <- function(x,number=1){ 
  x = rnorm(number,x,0.20) 
}

MetropolisHastings <- function(oldPoint,iterations,burnIn){
  acceptedPoints = 0
  chain <- vector("numeric",length = iterations)
  progressBar <- txtProgressBar(min = 0,max = iterations,style = 3)
  for(i in 1:iterations){
    chain[i] = oldPoint
    proposedPoint = stepSize(oldPoint)
    acceptanceRatio = probDensity(proposedPoint)/probDensity(oldPoint)
    if (runif(1,0,1) < acceptanceRatio){
      countSwitchProb(oldPoint,proposedPoint)
      oldPoint = proposedPoint
      accpetedPoints =+ 1
    }
    else{
      oldPoint = oldPoint
    }
    setTxtProgressBar(progressBar,i)
  }
  chain <- tail(chain,-burnIn)
  close(progressBar)
  cat(((length(unique(chain)))*1.0/iterations)*100,"% acceptance rate\n")
  return(chain)
}

chainLength = 100000

chain <- MetropolisHastings(6,chainLength,1000)


whys <- lapply(chain,function(x) probDensity(x))
plot(probDensity,-10,20,main = "Plot of Chain Path",ylab = "Probability of Glucose Level",xlab = "Glucose Level Proportional to Baseline")
par(new = TRUE)
test <- hist(chain,yaxt='n',xlim = c(-10,20), ann=FALSE,breaks=seq(-10,20,l=400),ylab = "",xlab = "")

mcmcchain <- mcmc(chain)
gelman.diag(mcmcmchain)

mcmcchain <- mcmc(chains)

chains <- list()
j <- 1
par(mfrow = c(1,1))
chainLength = 1000000
plot(probDensity,-10,20,main = "Target Distribution",ylab = "Probability of Glucose Level",
     xlab = "Glucose Level Proportional to Baseline",type = 'l',
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
for (i in c(-3,2,7)){
  chains[[j]] <- MetropolisHastings(i,chainLength,100)
  par(new = TRUE)
  test <- hist(chains[[j]],yaxt='n',xlim = c(-10,20), ann=FALSE,breaks=seq(-10,20,l=400),ylab = "",xlab = "",xaxt = "n",yaxt = "n")
  par(new = TRUE)
  j <- j + 1
}
`           `

test <- hist(chains[[1]],yaxt='n',xlim = c(-10,20), ann=FALSE,breaks=seq(-10,20,l=400),ylab = "",xlab = "",plot = FALSE)$density
test1 <- as.numeric(lapply(seq(-10,20,length.out = 400),function(x) probDensity(x)))
testfun <- vector()
for(i in c(0:length(test))){testfun[i] <- abs(test[i]-test1[i])} 

barplot(testfun,yaxt = 'n',type = 'h',xaxt = 'n')
par(new = TRUE)
plot(test1,ylim = c(0,0.6),type = 'l',yaxt = 'n')
par(new = TRUE)
plot(test,ylim = c(0,0.6),type = 'l')
