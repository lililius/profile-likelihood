#pairwise form of marginal likelihood in Ning et al.(2017)
pfML=function(beta){
Log=NULL
for(i in 1:(n-1)){
   for(j in setdiff((1:n),c(1:i))){
      if(is.matrix(X)){
        R=exp(-(Y[i]-Y[j])*(X[i,]-X[j,])%*%beta)
      }else{
        R=exp(-(Y[i]-Y[j])*beta*(X[i]-X[j]))
       }
      Log=cbind(Log,log(1+R))
   }
}
S=sum(Log)
pfML=1/choose(n,2)*S
pfML
}
