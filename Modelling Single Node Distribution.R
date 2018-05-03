library(tidyverse)
library(pracma)
library(utils)
library(coda)

f <- function(x) { #function to turn into probability density
  ifelse (x >= 0 & x<=4,1,0)
}

bottom <- 0 #lower limit
top <- 2*pi+0.1 #upper limit

integral <- quad(f,bottom,top) #determine normalising constant 

probDensity <- function(x) { #generate probability density function by dividing each value by the normalising constant
  f(x)/integral
}

verifyProbDensity <- quad(probDensity,bottom,top) #should = 1.

par(mfrow = c(1,2))

stepSize <- function(x){ #change name
  x = rnorm(1,x,0.08) #set in quantum system
}

x   <- seq(-0.4,0.4,length=1000)
y   <- dnorm(x,mean=0, sd=0.08)
plot(x,y, type="l", lwd=1,main = "Transition Probability with S.D = 0.08 and Mean = 0",ylab = "Relative Frequency",xlab = "Step Size")

MetropolisHastings <- function(oldPoint,iterations,burnIn){
  acceptedPoints = 0
  chain <- vector("numeric",length = iterations)
  progressBar <- txtProgressBar(min = 0,max = iterations,style = 3)
  for(i in 1:iterations){
    chain[i] = oldPoint
    proposedPoint = stepSize(oldPoint)
    acceptanceRatio = probDensity(proposedPoint)/probDensity(oldPoint)
    if (runif(1,0,1) < acceptanceRatio){
      oldPoint = proposedPoint
      accpetedPoints =+ 1
    }
    else{
      oldPoint = oldPoint
    }
    setTxtProgressBar(progressBar,i)
  }
  if(burnIn > 0 ){
    chain <- tail(chain,-burnIn)
  }
  close(progressBar)
  cat(((length(unique(chain)))*1.0/iterations)*100,"% acceptance rate\n")
  return(chain)
}

par(mfrow = c(1,2),cex=1.5)

chain <- MetropolisHastings(1,100000,0)

hist(chain,ylim = c(0,0.25),xlim = c(-1,5),yaxt = 'n',main = "Random Walk Chain Histogram",xlab = "x",ylab = "P(x)")
par(new = TRUE)
plot(probDensity,-1,5, ann=FALSE,ylab = "",xaxt='n',xlab = "",ylim = c(0,0.28))

plot(x = chain,y = linspace(-1,5,length(chain)),type = 'l',yaxt='n', ann=FALSE,ylab = "",xlim = c(-1,5))
par(new = TRUE)
plot(probDensity,-1,5,main = "Plot of Chain Path",ylab = "")

h = hist(chain,plot = FALSE) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*5
plot(h,freq=FALSE,ylim = c(0,0.28),xlim = c(-1,5),yaxt = 'n',main = "Random Walk Chain Histogram",xlab = "x",ylab = "P(x)")
