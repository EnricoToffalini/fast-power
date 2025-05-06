
######################################

StdErr_power = function(simData = NA, target_SE = NA, B = 0, 
                        warm_up = 30, max_niter = 30, 
                        starting_N = 200, alpha = 0.05, 
                        include_warmup = TRUE){
  
  require(ggplot2)
  require(effects)
  
  results = data.frame(
    N = c(starting_N,rep(NA,(warm_up+max_niter)-1)),
    SE = NA,
    iterType = c(rep("warm_up",warm_up),rep("iter",max_niter)),
    iter = 1:(warm_up+max_niter)
  )
  for(i in 1:(nrow(results)-1)){
    results$SE[i] = simData(N=results$N[i],B1=B)$SE
    if(results$iterType[i]=="warm_up"){
      if(results$SE[i]>target_SE | is.na(results$SE[i])) results$N[i+1] = round(results$N[i]*runif(1,1.1,1.5))
      if(results$SE[i]<=target_SE & !is.na(results$SE[i])) results$N[i+1] = round(results$N[i]*runif(1,0.5,0.75))
    }else{
      fitSE = lm(log(SE)~log(N),data=results)
      ci = confint(fitSE)
      target_logN = (log(target_SE)-fitSE$coefficients[1])/fitSE$coefficients[2]
      effSE = data.frame(effect("log(N)",xlevels=list(N=c(exp(as.numeric(target_logN)),0)),fitSE))
      results$N[i+1] = round(exp(target_logN)* runif(1,0.9,1.1)) 
    }
    results$SE[i+1] = simData(N=results$N[i+1],B1=B)$SE
    print(paste("iteration:",i+1,"- N:",results$N[i+1],"- SE:",round(results$SE[i+1],4)))
    
  }
  if(include_warmup){
    results_final = results
  }else{
    results_final = results[results$iterType=="iter",]
  }
  fit = lm(log(SE) ~ log(N), data=results_final)
  eff = data.frame(effect("log(N)",xlevels=list(N=seq(min(results_final$N,na.rm=T),max(results_final$N,na.rm=T))),fit))
  N = eff$N[which.min(abs(eff$fit-log(target_SE)))]
  N_lower = eff$N[which.min(abs(eff$lower-log(target_SE)))]
  N_upper = eff$N[which.min(abs(eff$upper-log(target_SE)))]
  
  plot = ggplot(eff,aes(x=N,y=exp(fit)))+
    geom_line(size=1)+
    geom_ribbon(aes(ymin=exp(lower),ymax=exp(upper)),alpha=.2)+
    geom_point(data=results_final,aes(x=N,y=SE,
                                     color=iter),size=3.5,alpha=.5)+
    geom_hline(yintercept=target_SE,color="#EE5500",linetype=2,size=1)+
    scale_color_gradient(low="black",high="#22DDFF")+
    xlab("Sample size (total N)")+
    ylab("Std.Err.")+
    theme(text=element_text(size=20))
  
  return(list(N = N, N_ci = c(N_lower, N_upper),
              results = results_final, plot = plot))
}

######################################
