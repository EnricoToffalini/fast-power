
##############################################################

simData = function(N = 600, B1 = 0){
  
  require(glmmTMB)
  
  estimated_B=NA; SE=NA; p=NA
  
  tryCatch({
    N = round(N/2)*2
    group = factor(rep(c("A", "B"), each = N/2))
    sigmaBase = 1
    sigma = ifelse(group == "A", sigmaBase, sigmaBase*exp(B1))
    x = rnorm(N,0,1)
    y = 1 + 2 * x + rnorm(N, sd = sigma)
    df = data.frame(y, x, group)
    fit = glmmTMB(y ~ x, dispformula =~ group, data=df)
    su = summary(fit)
    estimated_B = su$coefficients$disp["groupB","Estimate"]
    SE = su$coefficients$disp["groupB","Std. Error"]
    p = su$coefficients$disp["groupB","Pr(>|z|)"]
    
  },error=function(e){})
  
  return(list(estimated_B=estimated_B,SE=SE,p=p))
  
}

##############################################################


library(parallel)
inputs = 1:5000

Ns = c(618)

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


