
######################################

library(dplyr)
library(mgcv)
library(effects)
library(ggplot2)
library(toffee) # devtools::install_github("enricotoffalini/toffee")

simData = function(N=NA,hypothesis=1,B1=0.2){
  x = rnorm(N,0,1)
  y = rnorm(N,0,1) + B1*x + 1
  fit = lm(y~x)
  su = summary(fit)
  B = su$coefficients["x","Estimate"]
  SE = su$coefficients["x","Std. Error"]
  p = su$coefficients["x","Pr(>|t|)"]
  return(list(B=B,SE=SE,p=p))
}

######################################

N = round(exp(seq(log(50),log(500),length.out=20)))
x = lapply(N,function(n) simData(N=n))
dfx = bind_rows(x)
dfx$N = N
dfx$z = dfx$B / dfx$SE
plot(log(dfx$N),log(dfx$SE))


dfx$logSE = log(dfx$SE)
dfx$logN = log(dfx$N)
fit = lm(logSE ~ logN, data=dfx)
(target_logSE = log(0.1 / 2.8016)) # B / critical z for 80% power


eff =  data.frame(effect("logN",xlevels=list(logN=log(20:1500)),fit))
target_logN = eff$logN[which.min(abs(eff$fit-target_logSE))]
target_logN_lower = eff$logN[which.min(abs(eff$lower-target_logSE))]
target_logN_upper = eff$logN[which.min(abs(eff$upper-target_logSE))]
exp(target_logN_lower)
exp(target_logN)
exp(target_logN_upper)


ggplot(eff,aes(x=logN,y=fit))+
  geom_line(size=1)+
  geom_ribbon(aes(ymin=lower,ymax=upper),alpha=.2)+
  geom_point(data=dfx,aes(x=log(N),y=log(SE)),color="blue",size=3.5,alpha=.5)+
  geom_hline(yintercept=target_logSE,color="red",linetype=2,size=1)+
  xlab("log N")+
  ylab("log SE")+
  theme(text=element_text(size=20))

exp(target_logN)

######################################
