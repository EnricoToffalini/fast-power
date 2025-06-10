
##############################################################

library(ggplot2)
library(future.apply)
source("simulationCode/N-and-k-Logistic.R")

##############################################################

simData = simDataStimulusType

# varying over N 
Ns = round(exp(runif(1000,log(20),log(200)))); Ns
ks = c(5,40); ks
combs = data.frame(expand.grid(N=Ns,k=ks),SE=NA); combs

plan(multisession(workers=7))
combs$SE = future_sapply(1:nrow(combs), function(i) {
  simData(combs$N[i], combs$k[i])$SE
})
combs$logN = log(combs$N)
combs$logk= log(combs$k)
combs$logSE = log(combs$SE)
combs$groupN = as.factor(combs$N)
combs$groupk = as.factor(combs$k)
combsN = combs

(ggN = ggplot(combsN, aes(x = logN, y = logSE, group = groupk, color=groupk)) + 
    geom_point(size=4, alpha=.3)+
    geom_smooth(size=2, se=F)+
    labs(color="k")+
    ylab("log(StdErr)") + xlab("log(N)") +
    theme(text=element_text(size=25)))


# varying over k 
Ns = c(20,60); Ns
ks = round(exp(runif(1000,log(5),log(200)))); ks
combs = data.frame(expand.grid(N=Ns,k=ks),SE=NA); combs

plan(multisession(workers=7))
combs$SE = future_sapply(1:nrow(combs), function(i) {
  simData(combs$N[i], combs$k[i])$SE
})
combs$logN = log(combs$N)
combs$logk= log(combs$k)
combs$logSE = log(combs$SE)
combs$groupN = as.factor(combs$N)
combs$groupk = as.factor(combs$k)
combsK = combs

(ggK = ggplot(combsK, aes(x = logk, y = logSE, group = groupN, color=groupN)) + 
    geom_point(size=4, alpha=.3)+
    geom_smooth(size=2, se=F)+
    labs(color="N")+
    ylab("log(StdErr)") + xlab("log(k)") +
    theme(text=element_text(size=25)))

save.image(paste0("simulations_N-k-Type.RData"))

##############################################################

