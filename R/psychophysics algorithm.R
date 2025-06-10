
##################################

rm(list=ls())
library(pwr)
library(ggplot2)
library(psyphy)

##################################

df = data.frame(n=3:4000,
                power=NA,
                pp=NA)
df$power = round(pwr.t.test(n=df$n,d=0.10)$power,5)

fit = glm(cbind(power*1e5,1e5-power*1e5) ~ sqrt(n), 
          data=df, 
          family=binomial(link=mafc.probit(20)))
df$pp = as.numeric(predict(fit,newdata=list(n=df$n),type="response"))

ggplot(df)+
  geom_line(aes(x=n,y=power),color="black",size=1)+
  geom_line(aes(x=n,y=pp),color="red",size=1,linetype=2)+
  scale_y_continuous(limits=c(0,1))+
  theme(text=element_text(size=20))

##################################


