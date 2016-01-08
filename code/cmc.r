######################### cmc.r #########################

## Truncated distribution
rtrunc=function(n,spec,para,aa=0,bb=Inf){
  pn=formalArgs(paste0("r",spec))[-1]
  if(length(para)!=length(pn))
    stop("the length of para is not correct")
  names(para)=pn
  if(length(aa)!=length(bb))
    stop("the length of argument aa is not equal to bb")
  k=length(aa)
  if(any(aa>bb))
    stop("argument aa is greater than bb")
  if(k==1){
    aa=rep(aa,n)
    bb=rep(bb,n)
    para=outer(rep(1,n),para)
  }else if(n!=k){
    stop("length of argument aa and bb is not equal to 1 or n")
  }else{
    para=outer(rep(1,n),para)
  }
  pp=runif(n,min=0,max=1)
  G1=function(q)
    do.call(paste0("p",spec),as.list(data.frame(q,para)))
  G2=function(p)
    do.call(paste0("q",spec),as.list(data.frame(p,para)))
  cc=G2(G1(aa)+pp*(G1(bb)-G1(aa)))
  pmin(pmax(aa,cc),bb)
}

## example
# rtrunc(5,"norm",c(0,1))
# rtrunc(5,"norm",c(0,1),1:5,2:6)
# rtrunc(5,"norm",c(mean=0,sd=1),1:5,2:6)

## Independent random variables
rindv=function(n,spec,para,aa,bb){
  if(length(aa)!=length(bb))
    stop("the length of argument aa is not equal to bb")
  k=length(aa)
  spec=as.character(spec)
  if(length(spec)==1)
    spec=rep(spec,k)
  if(length(spec)!=k)
    stop("the length of argument spec is not equal to aa or bb")
  if(length(para)==1)
    para=lapply(1:k,function(i)para)
  if(length(para)!=k)
    stop("the length of argument para is not equal to aa or bb")
  t(sapply(1:k,function(i){
    args=c(n,spec[i],para[i],aa[i],bb[i])
    do.call(rtrunc,args)
  }))
}

## example
# rindv(4,c("norm","unif","triangle"),list(c(mean=0.5,sd=0.05),c(min=0.5,max=0.75),c(a=0,b=1,c=0.4)),c(0,0.5,0),c(1,0.75,1))

## Structure random variables
rstrv=function(n,spec,para,aa,bb,m=1){
  if(length(aa)!=length(bb))
    stop("the length of argument aa is not equal to bb")
  k=length(aa)
  spec=as.character(spec)
  if(length(spec)==1)
    spec=rep(spec,k)
  if(length(spec)!=k)
    stop("the length of argument spec is not equal to aa or bb")
  if(length(para)==1)
    para=lapply(1:k,function(i)para)
  if(length(para)!=k)
    stop("the length of argument para is not equal to aa or bb")
  if(sum(aa)>m|sum(bb)<m)
    stop("out of boundary")
  x=matrix(NA,k,n)
  p=outer(aa,rep(1,n))
  q=outer(bb,rep(1,n))
  for(j in 1:n){
    s=sample(1:k)
    for(i in 1:(k-1)){
      l=max(p[s[i],j],m-sum(q[-s[i],j]))
      u=min(q[s[i],j],m-sum(p[-s[i],j]))
      args=c(1,spec[s[i]],para[s[i]],l,u)
      x[s[i],j]=p[s[i],j]=q[s[i],j]=do.call(rtrunc,args)
    }
    x[s[k],j]=m-sum(x[-s[k],j])
  }
  if(any(is.na(x)))
    stop("missing data in the sampling")
  x
}

## example
# rstrv(4,c("norm","unif","triangle"),list(c(mean=0.5,sd=0.05),c(min=0.5,max=0.75),c(a=0,b=1,c=0.4)),c(0,0.5,0),c(1,0.75,1))

## Ordered random variables
rordv=function(n,spec,para,aa,bb){
  if(length(aa)!=length(bb))
    stop("the length of argument aa is not equal to bb")
  k=length(aa)
  spec=as.character(spec)
  if(length(spec)==1)
    spec=rep(spec,k)
  if(length(spec)!=k)
    stop("the length of argument spec is not equal to aa or bb")
  if(length(para)==1)
    para=lapply(1:k,function(i)para)
  if(length(para)!=k)
    stop("the length of argument para is not equal to aa or bb")
  if(any(diff(bb)<0))
    stop("out of boundary")
  x=matrix(NA,k,n)
  lo=rep(aa[1],n)
  for(i in 1:k){
    l=pmax(lo,aa[i])
    u=rep(bb[i],n)
    args=list(n,spec[i],para[[i]],l,u)
    x[i,]=lo=do.call(rtrunc,args)
  }
  if(any(is.na(x)))
    stop("missing data in the sampling")
  x
}

## example
# rordv(4,c("norm","unif","triangle"),list(c(mean=0.5,sd=0.05),c(min=0.5,max=0.75),c(a=0,b=1,c=0.4)),c(0,0.5,0),c(0.6,0.75,1))

# Distribution parameters
parlist=function(x,d,p){
  # x is a data.frame which contains d=DISTR, p=P1, P2...
  n=nrow(x)
  if(missing(d))
    d="DISTR"
  if(missing(p))
    p=names(x)[grep("^P[0-9]*[0-9]$",names(x))]
  pn=lapply(paste0("r",as.matrix(x[,d])),function(x)
    formalArgs(x)[-1])
  lapply(1:n,function(i){
    y=as.numeric(x[i,p])
    y=y[1:length(pn[[i]])]
    names(y)=pn[[i]]
    y
  })
}

## example
# d=data.frame(spec=c("norm","unif","triangle"),mean=c(0.5,0.5,0.5),p1=c(0,0,0),p2=c(1,1,1),p3=c(NA,NA,0.5))
# parlist(d,"spec",c("p1","p2","p3"))
# rindv(4,d$spec,parlist(d,"spec",c("p1","p2","p3")),rep(0,3),rep(1,3))

