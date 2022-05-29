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



#considerando nivel de ingresos unicamente
### ESTE ES EL BUENO ###
    
 estudiantes_x_ing <- base |> 
                 group_by(nivel_ingresos) |> 
                 count()  
    
    
 estudiantes_vig <- base |> 
      group_by(nivel_ingresos) |> 
      count(vigencia) |> 
      pivot_wider(names_from = vigencia, 
                                 values_from = n, values_fill = 0) |> 
      rename("baja"="2", "vigente"="1" )
 

 
 bf <-merge(estudiantes_x_ing,estudiantes_vig) |> 
   select(-vigente)
 

 
 #AGREGAMOS MÁS VARIABLES PARA VER SI SE PUEDE MEJORAR EL DIC
 #considerando sexo, tipo de secundaria, nivel de ingresos, 
 # aprobación, piso de tierra
 
 estudiantes <- base |> 
   group_by(cve_sexo,Sec,nivel_ingresos,Aprobar,piso_de_tierra) |> 
   count()        
 
 
 estudiantes_vig <- base |> 
   group_by(cve_sexo,Sec,nivel_ingresos,Aprobar,piso_de_tierra) |> 
   count(vigencia) |> 
   pivot_wider(names_from = vigencia, 
               values_from = n, values_fill = 0) |> 
   rename("baja"="2", "vigente"="1" )
 
 
 bf <-merge(estudiantes,estudiantes_vig) |> 
   select(-vigente)
 
 
 
 #considerando tipo de secundaria, nivel de ingresos, 
 
 estudiantes <- base |> 
   group_by(Sec,nivel_ingresos) |> 
   count()        
 
 
 estudiantes_vig <- base |> 
   group_by(Sec,nivel_ingresos) |> 
   count(vigencia) |> 
   pivot_wider(names_from = vigencia, 
               values_from = n, values_fill = 0) |> 
   rename("baja"="2", "vigente"="1" )
 
 
 bf <-merge(estudiantes,estudiantes_vig) |> 
   select(-vigente)
 
 
 
 #considerando sexo, nivel de ingresos
  estudiantes <- base |> 
   group_by(cve_sexo,nivel_ingresos) |> 
   count()        
 
 
 estudiantes_vig <- base |> 
   group_by(cve_sexo,nivel_ingresos) |> 
   count(vigencia) |> 
   pivot_wider(names_from = vigencia, 
               values_from = n, values_fill = 0) |> 
   rename("baja"="2", "vigente"="1" )
 
 
 bf <-merge(estudiantes,estudiantes_vig) |> 
   select(-vigente)
 
 
 
 #considerando nivel de ingresos, Aprobar
  estudiantes <- base |> 
   group_by(nivel_ingresos, Aprobar) |> 
   count()        
 
 
 estudiantes_vig <- base |> 
   group_by(nivel_ingresos, Aprobar) |> 
   count(vigencia) |> 
   pivot_wider(names_from = vigencia, 
               values_from = n, values_fill = 0) |> 
   rename("baja"="2", "vigente"="1" )
 
 
 bf <-merge(estudiantes,estudiantes_vig) |> 
   select(-vigente)
 
 
 
 #considerando sexo, tipo de secundaria,aprobación, piso de tierra
 
 estudiantes <- base |> 
   group_by(cve_sexo,Sec,Aprobar,piso_de_tierra) |> 
   count()        
 
 
 estudiantes_vig <- base |> 
   group_by(cve_sexo,Sec,Aprobar,piso_de_tierra) |> 
   count(vigencia) |> 
   pivot_wider(names_from = vigencia, 
               values_from = n, values_fill = 0) |> 
   rename("baja"="2", "vigente"="1" )
 
 
 bf <-merge(estudiantes,estudiantes_vig) |> 
   select(-vigente)
 
 
 
 #################################################################################


n<-nrow(bf)
par(mfrow=c(2,2))
plot(bf$baja)
plot(bf$baja/bf$n*100)

#-Defining data-
data<-list("n"=n,"y"=bf$baja,"ne"=bf$n/100)


#-Defining inits-
inits<-function(){list(theta=rep(1,n),a=1,b=1,yf1=rep(1,n))}

#-Selecting parameters to monitor-
pars<-c("theta","eta","yf1")

#-Running code-
#OpenBUGS
ej.sim<-bugs(data,inits,pars,model.file="ef_int.txt",
               n.iter=100000,n.chains=2,n.burnin=10000)


#Traza de la cadena
traceplot(ej.sim)

#Cadena
out<-ej.sim$sims.list


#Resumen (estimadores)
out.sum<-ej.sim$summary

#Tabla resumen
out.sum.t<-out.sum[grep("theta",rownames(out.sum)),c(1,3,7)]
out.sum.t<-cbind(out.sum.t,apply(out$theta,2,prob))
dimnames(out.sum.t)[[2]][4]<-"prob"
print(out.sum.t)

#DIC
#OpenBUGS
out.dic<-ej.sim$DIC
print(out.dic)


#Estimaciones
out.p<-out.sum[grep("theta",rownames(out.sum)),]
out.eta<-out.sum[grep("eta",rownames(out.sum)),]

#x vs. y
xmin<-0
xmax<-10
ymin<-0
ymax<-50
par(mfrow=c(1,1))
plot(bf$baja/bf$n*100,type="p",col="grey50",xlim=c(xmin,xmax),ylim=c(ymin,ymax))

#
points((1:8)+0.2,out.p[,1],col=4,pch=16,cex=0.5)
segments((1:8)+0.2,out.p[,3],(1:8)+0.2,out.p[,7],col=4)
#
points(xmax-0.2,sum(bf$baja)/sum(bf$n)*100)

