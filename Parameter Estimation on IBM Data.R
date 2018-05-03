mydata <- read.csv('C:/Users/Jordan/Desktop/R Multimodal/ibmdata.csv',header = TRUE)
mydata <- mydata[-c(3,5,7,8,9,10,15,16,19,22,23,25,27,21,30,33,34,35)] #remove disinteresting elements

library("coda")

using <- mydata[c(15,1)]
y <- using$TotalWorkingYears
x <- using$ï..Age

likelihood = function(param){
  a = param[1]
  b = param[2]
  #sd = param[3]
  
  pred = a*x + b
  singlelikelihoods = dnorm(y, mean = pred, sd = 1.0, log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)
}

prior = function(param){
  a = param[1]
  b = param[2]
  #sd = param[3]
  aprior = dnorm(a, sd = 1, log = T)
  bprior = dnorm(b, sd = 1, log = T)
  #sdprior = dunif(sd, min=0, max=30, log = T)
  return(aprior+bprior)#+sdprior)
}

proposalfunction = function(param){
  return(rnorm(2,mean = param, sd= c(0.01,0.01)))
}

run_metropolis_MCMC = function(startvalue, iterations){
  progressBar <- txtProgressBar(min = 0,max = iterations,style = 3)
  chain = array(dim = c(iterations+1,2))#3))
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])
    
    probab = exp(likelihood(proposal)+ prior(proposal) - likelihood(chain[i,])- prior(chain[i,]))
    #print(probab)
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
    setTxtProgressBar(progressBar,i)
  }
  chain <- tail(chain,iterations-2000)
  return(mcmc(chain))
}

startvalue = c(0.5,-10)#,0)
chain = run_metropolis_MCMC(startvalue, 100000)

plot(chain)
summary(chain)



plot(using$ï..Age,using$TotalWorkingYears,xlim = c(18,60),ylim = c(0,50),type = 'p',pch = 20,xlab = "Age",ylab = "Years Spent Working",main = "Graph Showing IBM Employees Age vs Years Spent Working (Anywhere)",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
abline(-18,1,col = "2")

lm(using$TotalWorkingYears~using$ï..Age)
abline(lm(using$TotalWorkingYears~using$ï..Age),lty = 1,lwd = 2)
par(new = TRUE)

plot(test,xlim = c(18,60),ylim = c(0,50),xaxt="n",yaxt = "n",ylab="",xlab = "",lwd = 2,lty =2)


test <- function(x){
  return(0.5547*x - 10.0092)
}
