
##############################################################

simData = function(N = 600, B1=0){
  
  require(lavaan)
  
  estimated_B=NA; SE=NA; p=NA
  
  tryCatch({
    
    group = sample(0:1,N,replace=T)
    X = rnorm(N,0,1)
    Y = X*0.1 + X*group*B1 + rnorm(N,0,1)
    
    df = data.frame(
      x1 = 0.8*X + rnorm(N,0,1),
      x2 = 0.3*X + rnorm(N,0,1),
      x3 = 0.5*X + rnorm(N,0,1),
      x4 = 1.2*X + rnorm(N,0,1),
      x5 = 0.6*X + rnorm(N,0,1),
      y1 = 0.5*Y + rnorm(N,0,1),
      y2 = 0.9*Y + rnorm(N,0,1),
      y3 = 1.5*Y + rnorm(N,0,1),
      y4 = 0.4*Y + rnorm(N,0,1),
      y5 = 0.2*Y + rnorm(N,0,1),
      group
    )
    
    model = '
  X =~ x1 + x2 + x3 + x4 + x5
  Y =~ y1 + y2 + y3 + y4 + y5

  Y ~ c(b1, b2)*X

  diff := b1 - b2
  '
    fit = sem(model,df,group="group",std.lv=T)
    
    if(inspect(fit, "converged")){
      coefs = summary(fit)$pe
      
      estimated_B = coefs[coefs$lhs=="diff","est"]
      SE = coefs[coefs$lhs=="diff","se"]
      p = coefs[coefs$lhs=="diff","pvalue"]
    }
  },error=function(e){})
  return(list(estimated_B=estimated_B,SE=SE,p=p))
}

##############################################################


library(parallel)
inputs = 1:5000

Ns = c(272)

for(n in Ns){
  cl = makeCluster(1) 
  Start = Sys.time()
  clusterExport(cl, varlist = c("simData","n"))
  results = parLapply(cl, inputs, function(x) simData(N=n))
  assign(paste0("results",n),results)
  save.image(file="PowerSimExample2Results.RData")
  x = unlist(results)
  power = mean(x[names(x)=="p"]<0.05)
  print(paste("N:",n,"- power:",round(power,3)))
  End = Sys.time()
  print(difftime(End,Start,units="hours"))
  stopCluster(cl)
}


