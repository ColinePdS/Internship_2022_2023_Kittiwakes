load('anV11.RData')
library(nimble)

myCode <- nimbleCode({
  for(i in 1:N){ 
    for (j in 1:T){ 
      arrival[i,j] ~ dbin(p[i,j], 1) 
      p[i,j]  <- phi(cste+
                       b1j*j+
                       b2j*j*j+
                       Year[Yr[i]]  
                     
      )
      
    }
  }   
  for (k in 1:(A)){ 
    
    Year[k]~dnorm(0,sdYr)
    
  } 
  isdYr~dgamma(1,1)
  sdYr<- 1/isdYr
  
  cste  ~ dnorm(0.001, 0.001)
  b1j  ~ dnorm(0.001, 0.001)
  b2j ~dnorm(0.001, 0.001)
})

myConstants <- list( N = N, T=T, A =A, Yr=Yr)
myData      <- list( arrival = hdv_f2000_f)
myInits     <- list( cste = 0,
                     b1j = 0,
                     b2j = 0,
                     isdYr = runif(1,0,1),
                     Year = rnorm(A,0,1)
)
n.iter <- 50000
n.burnin <- 10000
nc <- 2
nt <- 1


myOut <- nimbleMCMC(code = myCode, 
                    constants = myConstants, 
                    data = myData, 
                    inits = myInits,
                    monitors = c("cste", "b1j", "b2j", "sdYr"),
                    niter = n.iter,
                    nburnin = n.burnin, 
                    nchains = nc,
                    thin = nt,
                    summary = TRUE
)





save(myOut, file = "AnV14.RData")


