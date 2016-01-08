######################### data.r #########################

## Load packages
library(dplyr)
library(tidyr)
library(sqldf)

## Check if CV is out of boundary
cvcheck=function(mu,cv,distr=1,perc=0){
  ifelse(
    distr,
    ifelse(
      perc,
      pmin(1/sqrt(3),(1/mu-1)/sqrt(3),cv),
      pmin(1/sqrt(3),cv)),
    cv)
}

## Data preparation
nodes=read.csv("data/d_nodes.csv")
acts=read.csv("data/d_acts.csv")%>%tbl_df()%>%
  mutate(CV=ifelse(MISS,CV*1.5,CV),
         P1=Mean-sqrt(3)*CV*abs(Mean),
         P2=Mean+sqrt(3)*CV*abs(Mean),
         L=P1,U=P2)%>%
  select(-UNITS,-MISS)
pars=read.csv("data/d_pars.csv")%>%tbl_df()%>%
  mutate(CV=ifelse(MISS,CV*1.5,CV),
         CV=cvcheck(Mean,CV,DISTR=="unif",PERC),
         P1=ifelse(
           DISTR=="unif",Mean-sqrt(3)*CV*abs(Mean),P1),
         P2=ifelse(
           DISTR=="unif",Mean+sqrt(3)*CV*abs(Mean),P2),
         P3=ifelse(
           DISTR=="triangle",3*Mean-P1-P2,NA),
         L=P1,U=P2)%>%
  select(-UNITS,-MISS,-PERC,-SEG)
# write.csv(pars,"data/d_pars1.csv",row.names=F,na="")
d0=expand.grid(ID=unique(pars$ID),Year=1600:2012)
pars=sqldf("select p.*, d.Year from pars p, d0 d where d.ID=p.ID and d.Year>=p.BEGIN and d.Year<=p.END")%>%select(-BEGIN,-END)
save(nodes,acts,pars,file="data.Rdata")

