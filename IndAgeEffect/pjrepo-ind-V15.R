load(file = "IdAV6.RData")



library(nimble)


myCode <- nimbleCode({
  for(i in 1:length(tori)){ 
    for (j in 1:T){
      arrival[i,j] ~ dbin(p[i,j], 1) 
      p[i,j]  <- phi( (cste)+
                       bj1*j+
                       bj2*j*j+
                       eps[tori[i]]+
                       eta[age[i]]*g[age[i]]
                    )
       }
  }     
  
  cste  ~ dnorm(0.001, 0.001)
  bj1  ~ dnorm(0.001, 0.001)
  bj2  ~ dnorm(0.001, 0.001)
  
      
  
  for(k in 1:I) {
    eps[k] ~ dnorm(0,tau)
  }
  for (l in 2: MaxAge){
    eta[l] ~dnorm(0.001, 0.001)
  }
  
  
  invTau ~ dgamma(1, 1)
  tau <- 1/invTau
  
  
  
})

myConstants <- list( T=T, I = I, tori = ct_code[,2], MaxAge = MaxAge, age = age_2000_f, g = g)
myData      <- list( arrival = hdv_f2000_f)
myInits     <- list( cste = 0,
                     bj1 = 0,
                     bj2 = 0,
                     eps = rep(0,I),
                     eta = rep(0,MaxAge),
                     invTau = runif(1,0,1)
                     
                     )

n.iter <- 60000
n.burnin <- 48000
nc <- 2
nt <- 3

myOut <- nimbleMCMC(code = myCode, 
                    constants = myConstants, 
                    data = myData, 
                    inits = myInits,
                    monitors = c( "bj1","bj2", "cste", "tau", "eta"),
                    niter = n.iter,
                    nburnin = n.burnin, 
                    nchains = nc,
                    thin = nt,
                   summary = TRUE
                   )



save (myOut, file = "IndV16.Rdata")

