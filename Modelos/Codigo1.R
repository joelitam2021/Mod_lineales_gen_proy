library(readxl)
library(tidyverse)
library(R2OpenBUGS)


#--- Funciones utiles ---
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

#-Lectura de datos-
base <- read_excel("./2022-04-25-BDUACh_v2.xlsx")

vigencia <- base |> 
  group_by(estado) |> 
  count(vigencia) |> 
  pivot_wider(names_from = vigencia, 
              values_from = n, values_fill = 0) |> 
  rename("baja"="2", "vigente"="1" )

alumno_x_edo <- base |> 
  group_by(estado) |> 
  count()

# calif_avg_x_edo <- base |> 
#   group_by(estado) |> 
#   summarise(avg_calif=mean(calif_avg))


#################################################################################
################# Modelo binomial (sin predictores)##############################
#####    obj - estimar probabilidades de permanecer vigentes por estado     #####
#################################################################################

base_final <- merge(vigencia,alumno_x_edo)

plot(base_final$vigente/base_final$n)
n <- nrow(base_final)



#-Defining data-
data<-list("n"=n,"y"=base_final$vigente,"ne"=base_final$n)

inits1<-function(){list(p=rep(0.5,n),a=1,b=1,yf1=rep(1,n))}
parsc<-c("p","eta","yf1")

#-Running code-
#OpenBUGS
ej1.sim<-bugs(data,inits1,parsc,model.file="modelo_bugs.txt",
              n.iter=100000,n.chains=2,n.burnin=10000)

#-Monitoring chain-
ej.sim<-ej1.sim

#Traza de la cadena
traceplot(ej1.sim)


#Cadena
#OpenBUGS
out<-ej1.sim$sims.list

z<-out$p[,2]
par(mfrow=c(2,2))
plot(z,type="l")
plot(cumsum(z)/(1:length(z)),type="l")
hist(z,freq=FALSE)
acf(z)

#Resumen (estimadores)
#OpenBUGS
out.sum<-ej1.sim$summary

#Tabla resumen
out.sum.t<-out.sum[grep("p",rownames(out.sum)),c(1,3,7)]
out.sum.t<-cbind(out.sum.t,apply(out$p,2,prob))
dimnames(out.sum.t)[[2]][4]<-"prob"
print(out.sum.t)

#DIC
#OpenBUGS
out.dic<-ej1.sim$DIC

#Estimaciones
out.p<-out.sum[grep("p",rownames(out.sum)),]
out.eta<-out.sum[grep("eta",rownames(out.sum)),]

#x vs. y
xmin<-0
xmax<-33
ymin<-0
ymax<-1
par(mfrow=c(1,1))
plot(base_final$vigente/base_final$n,type="p",col="grey50",xlim=c(xmin,xmax),ylim=c(ymin,ymax))

points(out.p[,1],col=2,pch=16,cex=0.5)
segments(1:31,out.p[,3],1:31,out.p[,7],col=2)

#probabilidad general
points(xmax,sum(base_final$vigente)/sum(base_final$n))

#probabilidad por estados
out.p<-out.eta
points(xmax,out.p[1],col=4,pch=16,cex=0.5)
segments(xmax,out.p[3],xmax,out.p[7],col=4)
