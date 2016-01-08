######################### run.r #########################

## Load packages
library(dplyr)
library(tidyr)
library(triangle)

## Basic functions
# To keep the value nonnegetive
p=function(x,r=T)x*(x>0)
# Sum of array
suma=function(x,m=2)apply(x,m,sum)

## Data
load("data.Rdata")
doa=acts%>%tbl_df()%>%select(ID,Year,Mean)
dop=pars%>%tbl_df()%>%select(ID,Year,Mean)
da=rbind(doa,dop)
# Year
year=1600:2012
# Node IDs
node=nodes$ID
# Number of nodes
nn=length(node)
# Number of years
ny=length(year)

## Function for P flow calculation
pfcal=function(i){
  # Preparation
  require(dplyr)
  require(tidyr)
  ds=da[,c(1:2,i+2)]
  names(ds)=c("ID","Year","Value")
  data=ds%>%spread(ID,Value,0)
  # Initial value
  PF=array(0,c(nn,nn,ny),list(node,node,year))
  # Calculating
  P0=with(data,{source("pflow.r",T);PF})
  #======Output for figures======
  source("fig.r")
  fig(P0,data$DUHM,data$DRHM)%>%write.csv("fig.csv")
  #========================
  #======Output for 102 P flows======
  source("cflow.r");cf(P0)%>%write.csv("cflow.csv")
  #========================
  # Convert to matrix
  P0=as.data.frame(as.table(P0))
  names(P0)=c("From","To","Year","Flow")
  #======Output for basic P flows======
  P0%>%tbl_df()%>%filter(abs(Flow)>1e-6)%>%
    spread(Year,Flow,0)%>%mutate(Sample=i)%>%
    write.csv("pflow.csv",row.names=F)
}

## Running the program
pfcal(1)

