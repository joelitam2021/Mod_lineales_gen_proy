library(readxl)
library(tidyverse)
library(R2OpenBUGS)
library(R2jags)

base <- read.csv("tabla_escuela_bajas.csv")



n<-nrow(base)
par(mfrow=c(2,2))
plot(base$n)
plot(base$n/base$totEstado)

abline(h=1,col=2)

#-Defining data-
#data2<-list("n"=n,"y"=base$n,"ne"=base$tot_estado,"C"=base$escuela,"P"=base$estado,"A"=base$Age)
data2<-list("n"=n,"y"=base$n,"ne"=base$totEstado,"C"=base$escuela,"P"=base$cveEstado)


#-Defining inits-
initsd<-function(){list(alpha=0,beta=rep(0,n),gama=rep(0,n),yf1=rep(1,n))}

#-Selecting parameters to monitor-
parsd<-c("theta","alpha.adj","beta.adj","gama.adj","yf1")

#-Running code-
#OpenBUGS
ej7d.sim<-bugs(data2,initsd,parsd,model.file="uachEdo.txt",
               n.iter=50000,n.chains=2,n.burnin=5000)

#ej7d.sim<-jags(data2,initsd,parsd,model.file="uachEdo.txt",
#               n.iter=50000,n.chains=2,n.burnin=5000,n.thin=1)




#-Monitoring chain-
ej7.sim<-ej7d.sim

#Traza de la cadena
#traceplot(ej7.sim)

#Cadena

#OpenBUGS
outd<-ej7d.sim$sims.list


z<-outa$theta
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
#OpenBUGS
outd.sum<-ej7d.sim$summary


#Tabla resumen
out<-outd
out.sum<-outd.sum
out.sum.t<-out.sum[grep("theta",rownames(out.sum)),c(1,3,7)]
out.sum.t<-cbind(out.sum.t,apply(out$theta,2,prob))
dimnames(out.sum.t)[[2]][4]<-"prob"
print(out.sum.t)

#DIC
#OpenBUGS
outd.dic<-ej7d.sim$DIC

print(outd.dic)

#Estimaciones
outd.p<-outd.sum[grep("theta",rownames(outd.sum)),]

#x vs. y
xmin<-0
xmax<-50
ymin<--1
ymax<-3
par(mfrow=c(1,1))
#plot(data$n/data$tot_estado,type="p",col="grey50",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
plot(base$n/base$totEstado,type="p",col="grey50",xlim=c(xmin,xmax),ylim=c(ymin,ymax))



#

out.p<-outd.p
points(out.p[,1],col=2,pch=16,cex=0.5)
segments(1:45,out.p[,3],1:45,out.p[,7],col=2)

