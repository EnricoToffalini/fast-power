
library(glmmTMB)
library(lavaan)
library(effects)
library(ggplot2)
library(toffee) # devtools::install_github("enricotoffalini/toffee")
source("StdErr algorithm function.R")

set.seed(0)

## DIFFERENT SIGMA

source("simulationCode/DifferentSigma.R")
B = 0.16
target_se = toffee::SE4power(B=B, power=0.8)
Start = Sys.time()
x = StdErr_power(simData=simData, target_SE = target_se,
                 warm_up = 30, max_niter = 30)
End = Sys.time()
timetakenDS = difftime(End,Start,units="mins")
x$N
x$plot
xDS = x
save.image("simulations.RData")

## MIXED LOGISTIC CROSSED
source("simulationCode/MixedLogisticCrossed.R")
B = 0.05
target_se = toffee::SE4power(B=B, power=0.8)
Start = Sys.time()
x = StdErr_power(simData=simData, target_SE = target_se,
                 warm_up = 30, max_niter = 30)
End = Sys.time()
timetakenMLC = difftime(End,Start,units="mins")
x$N
x$plot
xMLC = x
save.image("simulations.RData")

## SEM MULTIGROUP 1
source("simulationCode/SEMmultigroup.R")
B = 0.25
target_se = toffee::SE4power(B=B, power=0.8)
Start = Sys.time()
x = StdErr_power(simData=simData, target_SE = target_se,
                 warm_up = 30, max_niter = 30)
End = Sys.time()
timetakenSM1 = difftime(End,Start,units="mins")
x$N
x$plot
xSM1 = x
save.image("simulations.RData")


