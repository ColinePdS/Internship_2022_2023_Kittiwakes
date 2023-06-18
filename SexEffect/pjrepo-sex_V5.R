
load(file = "sxV6.RData")
library(nimble)

myCode <- nimbleCode({
  for(i in 1:N){ 
    
    for (j in 1:T){ 
      arrival[i,j] ~ dbin(p[i,j], 1) 
      p[i,j]  <- phi ( b1[1]+b[1]*j+b[2]*j*j+b1[2]*sexe[i,1])
      }
  }     

  b[1]  ~ dnorm(0.001, 0.001)
  b[2]  ~ dnorm(0.001, 0.001)
 b1[1]  ~ dnorm(0.001, 0.001)
 b1[2]  ~ dnorm(0.001, 0.001)

})

myConstants <- list( N = N, T=T )
myData      <- list( arrival = hdv_f2000_f, sexe = sx_f)
myInits     <- list( b1 = rep(0, 2),
                     b = rep(0, 2))

n.iter <- 50000
n.burnin <- 10000
nc <- 2
nt <- 1


myOut <- nimbleMCMC(code = myCode, 
                    constants = myConstants, 
                    data = myData, 
                    inits = myInits,
                    monitors = c("b1", "b"),
                    niter = n.iter,
                    nburnin = n.burnin, 
                    nchains = nc,
                    thin = nt,
                    summary = TRUE
                   )




 save (myOut, file = "SxV7.Rdata")

