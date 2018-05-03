library(tidyverse)
library(pracma)
library(utils)
library(coda)

barrierLeft <- pi
gap <- 0.4
barrierRight <- barrierLeft + gap

f <- function(x) { #function to turn into probability density
  ifelse (x <= barrierLeft & x > 0,abs(sin(x)),
          ifelse(x >= barrierRight & x <= barrierRight+pi,abs(sin(x-gap)),0))
}

bottom <- 0 #lower limit
top <- 2*pi+0.1 #upper limit

integral <- quad(f,bottom,top) #determine normalising constant 

probDensity <- function(x) { #generate probability density function by dividing each value by the normalising constant
  f(x)/integral
}

verifyProbDensity <- quad(probDensity,bottom,top) #should = 1.

par(mfrow = c(1,1))

plot(probDensity,0,barrierRight+pi,main = "Biodal Probability Distribution",xlab = "x",ylab = "P(x)")

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

stepSize <- function(x,stepSD,number=1){ #change name
  x = rnorm(number,x,stepSD) #set in quantum system
}

MetropolisHastings <- function(oldPoint,iterations,burnIn,stepSD){
  acceptedPoints = 0
  chain <- vector("numeric",length = iterations)
  progressBar <- txtProgressBar(min = 0,max = iterations,style = 3)
  for(i in 1:iterations){
    chain[i] = oldPoint
    proposedPoint = stepSize(oldPoint,stepSD)
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
  return(mcmc(chain))
}
chains <- list()
j <- 1

chainLength = 1000000
par(mfrow = c(1,1))
for (i in c(7.0)){
  totalIn <<- 0
  chains[[j]] <- MetropolisHastings(6,chainLength,1000,i)
  tunellingProbability <- totalIn/chainLength
  print(tunellingProbability)
  plot(probDensity,0,barrierRight+pi,main = paste("Plot of Final Distribution with Jumping S.D. = ",i),ylab = "P(x)",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  par(new = TRUE)
  hist(chains[[j]],yaxt='n',xlim = c(0,barrierRight+pi), ann=FALSE,breaks=seq(0,barrierRight+pi,l=400),ylab = "",xlab = "",xaxt = "n")
  j <- j + 1
}

gelman.diag(chains)

test <- hist(chains[[2]],yaxt='n',xlim = c(0,barrierRight+pi), ann=FALSE,breaks=seq(0,barrierRight+pi,l=100),ylab = "",xlab = "",plot = FALSE)$density
test1 <- as.numeric(lapply(seq(0,barrierRight+pi,length.out = 99),function(x) probDensity(x)))

test <- as.numeric(lapply(test,function(x) x/length(test)))
test1 <- as.numeric(lapply(test,function(x) x/sum(test1)))
    
testfun <- vector()
for(i in c(0:length(test))){testfun[i] <- (abs(test[i])-abs(test1[i]))*3} 

barplot(testfun,ylim = c(0,0.25),type = 'h',cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
par(new = TRUE)
plot(test1,ylim = c(0,0.25),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     type = 'l',yaxt = 'n',ylab = "P(x) with Error",xlab = "x",main = "Relative Error From the Discrete Chain Method & Target Distribution",xaxt = "n")
par(new = TRUE)
plot(test,ylim = c(0,0.25),type = 'l')
  