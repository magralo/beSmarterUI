remove(list = ls())
#setwd("C:/ANDRES/Portatil/Bayesian Econometrics/UserInterface/MateoV1/BS-UId/Data")
set.seed(12345)
MD<-1000  #Dataset Size
B<-c(0.5,0.8,-3)
B1<-c(-2.5,-3.5,0)
B2<-c(1,1,0)
set.seed(12345)
X1<-matrix(cbind(rnorm(MD,0,1),rnorm(MD,0,1),rnorm(MD,0,1)),MD,length(B))
X2<-matrix(cbind(rnorm(MD,0,1),rnorm(MD,0,1),rnorm(MD,0,1)),MD,length(B))
X3<-matrix(cbind(rnorm(MD,0,1),rnorm(MD,0,1),rnorm(MD,0,1)),MD,length(B))
X4<-matrix(rnorm(MD,1,1),MD,1)
#Taking into account the theoretic framework, the following vector is the difference between normal stochastic errors!!!
e<-mvrnorm(n = MD, mu=c(0,0), Sigma=matrix(c(1,0.6,0.6,1),2,2))
V1<-B2[1]+X1%*%B+B1[1]*X4+e[,1]
V2<-B2[2]+X2%*%B+B1[2]*X4+e[,2]
V3<-B2[3]+X3%*%B+B1[3]*X4
yl<-cbind(V1,V2)
ylMax<-apply(yl,1,max)
y<-apply(yl,1,function(x){which(x==max(x))})
y=ifelse(ylMax<0,3,y)
table(y)
SimMultProb<-cbind(y,X1,X2,X3,X4)
#SimMultProb<-cbind(y,X1[,1],X2[,1],X3[,1],X1[,2],X2[,2],X3[,2],X1[,3],X2[,3],X3[,3],X4)
colnames(SimMultProb)<-c("y","x11","x12","x13","x21","x22","x23","x31","x32","x33","x4")
#y~x11+x12+x13+x21+x22+x23+x31+x32+x33+x4
write.csv(SimMultProb,file="14SimMultProbmodel.csv", row.names=FALSE)


#bayesm
Data<- read.table(file="14SimMultProbmodel.csv",header=TRUE,sep=",")
Xa<-as.matrix(Data[,2:10])
Xd<-as.matrix(Data[,11])
XMPP<- createX(3, na=3, nd=2, Xa=Xa, Xd=Xd, INT = TRUE, DIFF = TRUE, base = 3)

#X=createX(p=3,na=3,nd=1,Xa=cbind(X1,X2,X3),Xd=X4,DIFF=TRUE,base=3)
Data1=list(p=3,y=y,X=XMPP)
Mcmc1=list(R=1100,burn=100,keep=1)
out=rmnpGibbs(Data=Data1,Mcmc=Mcmc1)
cat(" Summary of Betadraws ",fill=TRUE)
betatilde=out$betadraw/sqrt(out$sigmadraw[,1])
attributes(betatilde)$class="bayesm.mat"
summary(betatilde)
summary(out$betadraw)
cat(" Summary of Sigmadraws ",fill=TRUE)
sigmadraw=out$sigmadraw/out$sigmadraw[,1]
attributes(sigmadraw)$class="bayesm.var"
summary(sigmadraw)
summary(out$sigmadraw)



library(mlogit)
mode<-as.factor(y)
dat<-data.frame(mode,X1[,1],X2[,1],X3[,1],X1[,2],X2[,2],X3[,2],X1[,3],X2[,3],X3[,3],X4)
colnames(dat)<-c("mode","V1.1","V1.2","V1.3","V2.1","V2.2","V2.3","V3.1","V3.2","V3.3","V4")
attach(dat)
Exper<- mlogit.data(dat, shape = "wide", varying=2:10, choice = "mode")
res.mlogit<- mlogit(mode ~ V1 + V2 + V3 | V4, data=Exper, probit=TRUE)
summary(res.mlogit)

