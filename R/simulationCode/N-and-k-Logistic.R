
simDataGroup = function(N = 500, k = 10, tau = 0.8, B1 = 0){
  require(lme4)
  estimated_B=NA; SE=NA; p=NA
  
  tryCatch({
    
    # prepare random effects
    rInts = rnorm(n=N, 0, tau)
    rInt = rep(rInts, each=k)
    id = rep(1:N, each=k)
    
    # prepare fixed effects
    group = rep(rbinom(N,1,0.5), each=k)
    
    # generate linear component
    yLinear = 
      0 + 
      B1*group + 
      rInt
    
    # generate observed responses
    response = rbinom(N*k, 1, prob=plogis(yLinear))
    trial = rep(1:k, times=N)
    
    # prepare dataset
    df = data.frame(id, group, trial, response)
    
    fit = glmer(response ~ group + (1|id), data=df, family = binomial(link="logit"))
    su = summary(fit)
    estimated_B = su$coefficients["group","Estimate"]
    SE = su$coefficients["group","Std. Error"]
    p = su$coefficients["group","Pr(>|z|)"]
    
  },error=function(e){})
  
  return(list(estimated_B=estimated_B,SE=SE,p=p))
}

##########################################################

simDataStimulusType = function(N = 500, k = 10, tau = 0.8, B1 = 0){
  require(lme4)
  estimated_B=NA; SE=NA; p=NA
  
  tryCatch({
    
    # prepare random effects
    rInts = rnorm(n=N, 0, tau)
    rInt = rep(rInts, each=k)
    id = rep(1:N, each=k)
    
    # prepare fixed effects
    type = rbinom(N*k,1,0.5)
    
    # generate linear component
    yLinear = 
      0 + 
      B1*type + 
      rInt
    
    # generate observed responses
    response = rbinom(N*k, 1, prob=plogis(yLinear))
    trial = rep(1:k, times=N)
    
    # prepare dataset
    df = data.frame(id, type, trial, response)
    
    fit = glmer(response ~ type + (1|id), data=df, family = binomial(link="logit"))
    su = summary(fit)
    estimated_B = su$coefficients["type","Estimate"]
    SE = su$coefficients["type","Std. Error"]
    p = su$coefficients["type","Pr(>|z|)"]
    
  },error=function(e){})
  
  return(list(estimated_B=estimated_B,SE=SE,p=p))
}

##########################################################

simDataStimulusTypeSlope = function(N = 500, k = 10, tau = 0.8, tauSlope = 0.5, B1 = 0){
  require(lme4)
  estimated_B=NA; SE=NA; p=NA
  
  tryCatch({
    
    # prepare random effects
    rInts = rnorm(n=N, 0, tau)
    rInt = rep(rInts, each=k)
    rSlopes = rnorm(n=N, 0, tauSlope)
    rSlo = rep(rSlopes, each=k)
    id = rep(1:N, each=k)
    
    # prepare fixed effects
    type = rbinom(N*k,1,0.5)
    
    # generate linear component
    yLinear = 
      0 + 
      B1*type + 
      rInt +
      rSlo*type
    
    # generate observed responses
    response = rbinom(N*k, 1, prob=plogis(yLinear))
    trial = rep(1:k, times=N)
    
    # prepare dataset
    df = data.frame(id, type, trial, response)
    
    fit = glmer(response ~ type + (type|id), data=df, family = binomial(link="logit"))
    su = summary(fit)
    estimated_B = su$coefficients["type","Estimate"]
    SE = su$coefficients["type","Std. Error"]
    p = su$coefficients["type","Pr(>|z|)"]
    
  },error=function(e){})
  
  return(list(estimated_B=estimated_B,SE=SE,p=p))
}

