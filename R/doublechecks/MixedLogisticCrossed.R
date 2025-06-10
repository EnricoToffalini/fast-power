
##############################################################

simData = function(N = 600, B1=0, package="lme4"){
  
  if(package=="lme4") require(lme4)
  if(package=="glmmTMB") require(glmmTMB)
  if(!package %in% c("lme4","glmmTMB")) stop("unsupported package")
  
  estimated_B=NA; SE=NA; p=NA
  
  tryCatch({
    
    # general parameters
    N = round(N/3)*3
    k = 8
    blocks = 6
    N_Class = round(N/15)
    
    # prepare random intercepts
    rInts = rnorm(n=N, 0, 0.8)
    
    # set variability of subjects
    tauSubj = 0.5
    tauSubjSlope= 0.05
    rInt = rep(rInts*tauSubj, each=k*blocks)
    rSlope = rep(rnorm(N,0,tauSubjSlope), each=k*blocks)
    id = rep(1:N, each=k*blocks)
    Class = rep(rep(1:ceiling(N / N_Class), each = N_Class), each = k * blocks)[1:length(id)]
    
    # set variability of items
    item = c(replicate(N,sample(k*blocks)))
    tauItem = 0.2
    rItem = rnorm(k*blocks,0,tauItem)[item]
    
    # factors
    Block = rep(0:(blocks-1),each=k,times=N)
    Group = rep(c("CG","TG"),each=(N/2)*k*blocks)
    GroupDummy = as.numeric(as.factor(Group))-1
        
    # generate linear component
    yLinear = 
      -1 + 
      0*Block + 
      0*GroupDummy + 
      B1*Block*GroupDummy +
      rInt +
      rSlope*Block +
      rItem
    
    # generate observed responses
    acc = rbinom(N*k*blocks, 1, prob=plogis(yLinear))
    
    # prepare dataset
    df = data.frame(id, item, Group, Block, acc, Class)

    if(package=="lme4"){
      fit = glmer(acc ~ Group * Block + (1+Block||id) + (1|item), data=df, family = binomial(link="logit"))
      su = summary(fit)
      estimated_B = su$coefficients["GroupTG:Block","Estimate"]
      SE = su$coefficients["GroupTG:Block","Std. Error"]
      p = su$coefficients["GroupTG:Block","Pr(>|z|)"]
    }else if(package=="glmmTMB"){
      fit = glmmTMB(acc ~ Group * Block + (1+Block||id) + (1|item), data=df, family = binomial(link="logit"))
      su = summary(fit)
      estimated_B = su$coefficients$cond["GroupTG:Block","Estimate"]
      SE = su$coefficients$cond["GroupTG:Block","Std. Error"]
      p = su$coefficients$cond["GroupTG:Block","Pr(>|z|)"]
    }
  },error=function(e){})
  return(list(estimated_B=estimated_B,SE=SE,p=p))
}

##############################################################

library(parallel)
inputs = 1:5000

Ns = c(769)

for(n in Ns){
  cl = makeCluster(detectCores() - 1) 
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



