library(mvtnorm)
library(MASS)
library(ggplot2)
#Gaussian kernel function
kern=function(u){
return(exp(-u^2/2)/sqrt(2*pi))
}

#bandwidth is chosen by cv
hcv=function(x,y,h){
  M=rep(0,n)
  CV=rep(0,n)
  for(i in 1:n){
    s1=sum(kern((x[-i]-x[i])/h)*y[-i])	
    s2=sum(kern((x[-i]-x[i])/h))	
    M[i]=s1/s2
    CV[i]=(y[i]-M[i])^2
  }
  hcv=mean(CV)
  hcv
}
#the base measure function f
Fhat=function(y,beta){
  h2=optimize(hcv,c(0.1,2),x=Y,y=beta*X)$minimum
  W=kern((Y-y)/h2)
  fy=sum(beta*X*W)/sum(kern((Y-y)/h2)) 
  h1=optimize(hcv,c(0.01,2),x=beta*X,y=Y)$minimum
  Ehat=function(x){
    s1=sum(kern((beta*X-x)/h1)*Y)	
    s2=sum(kern((beta*X-x)/h1))	
    return(s1/s2)
  }
  a=0
  b=fy
  m=100
  delta=(b-a)/m
  height=rep(0,(m+1))
  for(k in 1:(m+1)){
   height[k]=Ehat(x=a+(k-1)*delta)
   }
  wts=c(1,rep(2,m-1),1)*delta/2
  I=sum(height*wts)
  f2=exp(I)
  f1=exp(fy*y)
  f=f1/f2
  fhat=1/f
  list(fhat=fhat)
}

