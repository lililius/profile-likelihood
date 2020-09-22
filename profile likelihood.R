library(mvtnorm)
library(MASS)
#Gaussian kernel function
kern=function(u){
return(exp(-u^2/2)/sqrt(2*pi))
}
#bandwidth is chosen by CV
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
#profile likelihood 	
PL=function(beta){
fhat=rep(0,n)
bhat=rep(0,n)
fy=rep(0,n)
h1=optimize(hcv,c(0.01,2),x=beta*X,y=Y)$minimum
h2=optimize(hcv,c(0.01,2),x=Y,y=beta*X)$minimum
print(c(h1,h2,beta))
h3=density(Y)$bw
for(i in 1:n){
   W=kern((Y[-i]-Y[i])/h2)
   #the kernel estimator of E(¦ÂX|y)
   fy[i]=sum(beta*X[-i]*W)/sum(kern((Y[-i]-Y[i])/h2)) 
   Ehat=function(x){
      s1=sum(kern((beta*X-x)/h1)*Y)	
      s2=sum(kern((beta*X-x)/h1))	
      return(s1/s2)
   }	
  a=0
  b=fy[i]
  m=50
  delta=(b-a)/m
  height=rep(0,(m+1))
  for(k in 1:(m+1)){
    height[k]=Ehat(x=a+(k-1)*delta)
   }
  wts=c(1,rep(2,m-1),1)*delta/2
  I=sum(height*wts)
  f2=exp(I)
  f1=exp(fy[i]*Y[i])
  f=f1/f2
  fhat[i]=1/f
}
F1=matrix(0,n,n)
for(j in 1:n){
   P1=exp(beta*X*Y[j])*fhat[j]
   P2=kern((Y[-j]-Y[j])/h3)/h3
   P2hat=mean(P2)
   F1[,j]=P1/P2hat
}
bf1=as.vector(rowMeans(F1))
bhat=log(bf1)
M=beta*X*Y-bhat+log(fhat)
pl=-sum(M)
pl
}

