######################### res_bind.r #########################

## Load packages
library(dplyr)
library(tidyr)

## Preparation
fl=dir("results")
k=1000

## Results combination
# 1. Uncertainty for both
pf0=NULL
for(i in which(substr(fl,1,5)=="fig_1")){
  load(paste0("results/",fl[i]))
  m=as.numeric(substr(fl[i],6,7))
  pf0=rbind(pf0,pf%>%mutate(Sample=Sample+(m-1)*k))
}
save(pf0,file="results/fig_1.Rdata")
# Output
load("results/fig_1.Rdata")
pf0%>%gather(Var,Value,-Sample,-Year)%>%
  group_by(Year,Var)%>%
  summarise(Mean=mean(Value),SD=sd(Value),
            Median=median(Value),CV=SD/Mean,
            Q25=quantile(Value,0.25),
            Q75=quantile(Value,0.75),
            Q05=quantile(Value,0.05),
            Q95=quantile(Value,0.95))%>%
  write.csv("fig_1.csv",row.names=F)

# 2. Uncertainty for only activity data
pf0=NULL
for(i in which(substr(fl,1,5)=="fig_2")){
  load(paste0("results/",fl[i]))
  m=as.numeric(substr(fl[i],6,7))
  pf0=rbind(pf0,pf%>%mutate(Sample=Sample+(m-1)*k))
}
save(pf0,file="results/fig_2.Rdata")
# Output
load("results/fig_2.Rdata")
pf0%>%gather(Var,Value,-Sample,-Year)%>%
  group_by(Year,Var)%>%
  summarise(Mean=mean(Value),SD=sd(Value),
            Median=median(Value),CV=SD/Mean,
            Q25=quantile(Value,0.25),
            Q75=quantile(Value,0.75),
            Q05=quantile(Value,0.05),
            Q95=quantile(Value,0.95))%>%
  write.csv("fig_2.csv",row.names=F)

# 3. Uncertainty for only parameters
pf0=NULL
for(i in which(substr(fl,1,5)=="fig_3")){
  load(paste0("results/",fl[i]))
  m=as.numeric(substr(fl[i],6,7))
  pf0=rbind(pf0,pf%>%mutate(Sample=Sample+(m-1)*k))
}
save(pf0,file="results/fig_3.Rdata")
# Output
load("results/fig_3.Rdata")
pf0%>%gather(Var,Value,-Sample,-Year)%>%
  group_by(Year,Var)%>%
  summarise(Mean=mean(Value),SD=sd(Value),
            Median=median(Value),CV=SD/Mean,
            Q25=quantile(Value,0.25),
            Q75=quantile(Value,0.75),
            Q05=quantile(Value,0.05),
            Q95=quantile(Value,0.95))%>%
  write.csv("fig_3.csv",row.names=F)

# 4. Uncertainty for 102 flows
pf0=NULL
for(i in which(substr(fl,1,2)=="cf")){
  load(paste0("results/",fl[i]))
  m=as.numeric(substr(fl[i],6,7))
  pf0=rbind(pf0,pf%>%mutate(Sample=Sample+(m-1)*k))
}
save(pf0,file="results/cf.Rdata")
# Output
load("results/cf.Rdata")
pf0%>%gather(Var,Value,-Sample,-Year)%>%
  group_by(Year,Var)%>%
  summarise(Mean=mean(Value),SD=sd(Value),
            Median=median(Value),CV=SD/Mean,
            Q25=quantile(Value,0.25),
            Q75=quantile(Value,0.75),
            Q05=quantile(Value,0.05),
            Q95=quantile(Value,0.95))%>%
  write.csv("cf.csv",row.names=F)
# Combination of 102 P flow results
p1=read.csv("cflow.csv")
p2=read.csv("cf.csv")
names(p1)[1]="Year"
p1%>%gather(Var,Value,-Year)%>%full_join(p2)%>%
  write.csv("flow102.csv",row.names=F)

