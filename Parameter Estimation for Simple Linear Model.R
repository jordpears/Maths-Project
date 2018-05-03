trueA = 4.0
trueB = 2.0
trueSd = 8.0
sampleSize = 100

library("coda")

x = -((sampleSize)/2):((sampleSize-1)/2)
y =  trueA * x  + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd)

likelihood = function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  
  pred = a*x + b
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)
}

prior = function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  aprior = dnorm(a,sd = 1.0, log = T)
  bprior = dnorm(b, sd = 1.0, log = T)
  sdprior = dnorm(sd, sd=1.0, log = T)
  return(aprior+bprior+sdprior)
}

proposalfunction = function(param){
  return(rnorm(3,mean = param, sd= c(0.01,0.01,0.01)))
}

MetropolisHastings = function(startvalue, iterations){
  chain = array(dim = c(iterations+1,3))
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])
    
    probab = exp(likelihood(proposal)+ prior(proposal) - likelihood(chain[i,])- prior(chain[i,]))
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  chain<-tail(chain,iterations-20000)
  return(mcmc(chain))
}

startvalue = c(3.8,1.2,8.3)
chain <- MetropolisHastings(startvalue, 100000)
summary(chain)
par(cex=1.5)
varnames(chain) <- c("Alpha","Beta","Gamma")
plot(chain,plot)
