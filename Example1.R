nrep=100#ÖØ¸´´ÎÊı
n=100
mu=2
betapl=rep(0,nrep)
betaNing=rep(0,nrep)
for(irep in 1:nrep){
X=rnorm(n,6,1)
eps=rnorm(n,0,1)	
Y=X*mu+eps	
shape=5
betapl[irep]=optimize(PL,c(0.01,3))$minimum
betaNing[irep]=optimize(pfML,c(0.01,3))$minimum
}
beta.mean=mean(betapl)
beta.sd=sd(betapl)
beta.mse=(beta.sd)^2+(beta.mean-mu)^2
beta.median=median(betapl)
beta.bias=mean(abs(betapl-mu))
betaNing.mean=mean(betaNing)
betaNing.sd=sd(betaNing)
betaNing.mse=(betaNing.sd)^2+(betaNing.mean-mu)^2
betaNing.median=median(betaNing)
betaNing.bias=mean(abs(betaNing-mu))

list(beta.mean=beta.mean,beta.median=beta.median,beta.mse=beta.mse, beta.bias=beta.bias,beta.sd=beta.sd)
list(betaNing.mean=betaNing.mean,betaNing.median=betaNing.median,betaNing.mse=betaNing.mse,betaNing.bias=betaNing.bias,betaNing.sd=betaNing.sd)
