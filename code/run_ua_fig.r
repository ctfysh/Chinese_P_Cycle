######################### run_ua_fig.r #########################

## Load packages
library(dplyr)
library(tidyr)
library(triangle)

## Basic functions
# To keep the value nonnegetive
p=function(x,r=T)x*(x>0)
# Sum of array
suma=function(x,m=2)apply(x,m,sum)
# Sampling functions
source("cmc.r")
# Random number generation function
rands=function(x,n=1,gd=c("Year","GP"),id=c("ID","Year")){
  require(dplyr)
  require(tidyr)
  sbind=function(x)Reduce(rbind,x)
  if(!exists("GP",x)) x$GP=""
  gp=unique(substr(x$GP,1,1))
  x0=NULL
  for(i in gp){
    if(i=="S"){
      # Structure random variables
      x1=x%>%filter(substr(GP,1,1)=="S")
      g=attr(group_by_(x1,.dots=gd),"indices")
      s=sbind(lapply(g,function(i){
        para=parlist(x1[i+1,])
        with(x1[i+1,],rstrv(n,DISTR,para,L,U))
      }))
      if(n==1){
        x2=data.frame(x1[unlist(g)+1,id,drop=F],X1=s)
      }else{
        x2=data.frame(x1[unlist(g)+1,id,drop=F],s)
      }
    }else if(i=="O"){
      # Ordered random variables
      x1=x%>%filter(substr(GP,1,1)=="O")
      ss=sbind(strsplit(as.character(x1$GP),"_"))
      x1=x1%>%mutate(
        ORD=as.numeric(ss[,2]),GP=ss[,1])%>%
        arrange(GP,ORD)
      g=attr(group_by_(x1,.dots=gd),"indices")
      s=sbind(lapply(g,function(i){
        para=parlist(x1[i+1,])
        with(x1[i+1,],rordv(n,DISTR,para,L,U))
      }))
      if(n==1){
        x2=data.frame(x1[unlist(g)+1,id,drop=F],X1=s)
      }else{
        x2=data.frame(x1[unlist(g)+1,id,drop=F],s)
      }
    }else{
      # Independent random variables
      x1=x%>%filter(!(substr(GP,1,1)%in%c("S","O")))
      para=parlist(x1)
      s=with(x1,rindv(n,DISTR,para,L,U))
      if(n==1){
        x2=data.frame(x1[,id],X1=s)
      }else{
        x2=data.frame(x1[,id],s)
      }
    }
    x0=rbind(x0,x2)
  }
  x0
}

## Data
load("data.Rdata")
doa=acts%>%tbl_df()%>%select(ID,Year,Mean)
dop=pars%>%tbl_df()%>%select(ID,Year,Mean)
# Sample size
n=10000
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
  source("fig2a_2e.r")
  PF=fig(P0)%>%as.data.frame()%>%
    cbind(Year=as.numeric(row.names(.)),.)
  row.names(PF)=NULL
  PF%>%cbind(Sample=i,.)
}

## Computing
system.time({
  # Sample size for each loop
  k=500
  # Loops
  m=n/k
  # Upper subscript bound
  u=k+2
  # Lower subscript bound
  l=3
  set.seed(12345)
  for(j in 1:m){
    # Uncertainty for both
    daa=acts%>%tbl_df()%>%rands(k)
    dap=pars%>%tbl_df()%>%rands(k)
    da1=rbind(daa[,c(1:2,l:u)],dap[,c(1:2,l:u)])
    # Uncertainty for only activity data
    dop1=dop%>%tbl_df()%>%cbind(replicate(k-1,.$Mean))
    names(dop1)=names(da1)
    dop1=dop1%>%filter(rowSums(.[l:u])!=0)
    da2=rbind(daa[,c(1:2,l:u)],dop1)
    # Uncertainty for only parameters
    doa1=doa%>%tbl_df()%>%cbind(replicate(k-1,doa$Mean))
    names(doa1)=names(da1)
    doa1=doa1%>%filter(rowSums(.[l:u])!=0)
    da3=rbind(doa1,dap[,c(1:2,l:u)])
    # Data combination
    dd=list(da1,da2,da3)
    rm(daa,dap,dop1,doa1,da1,da2,da3)
    gc()
    for(i in 1:3){
      da=dd[[i]]
      library(foreach)
      library(doParallel)
      cl=makeCluster(20)
      registerDoParallel(cl)
      clusterExport(
        cl,c("nn","ny","node","year","p","suma"))
      # Parallel computing
      pf=foreach(x=1:k,.combine=rbind)%dopar%pfcal(x)
      save(pf,file=paste0("results/fig_",i,"_",j,".Rdata"))
      stopCluster(cl)
      # stopImplicitCluster()
    }
    rm(pf,dd);gc()
  }
})

