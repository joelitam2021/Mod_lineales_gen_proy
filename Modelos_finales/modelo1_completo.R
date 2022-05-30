library(readxl)
library(tidyverse)
library(R2OpenBUGS)



#--- Funciones utiles ---
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

#-Lectura de datos-
base <- read_excel("./Data/2022-04-25-BDUACh_v2.xlsx")

#1.- Vigente
#2.- Baja definitiva
 estudiantes <- base |> 
                 group_by(cve_sexo,Sec,leng_indigena, Nivel_estudios_jefe,
                          indice) |> 
                 count()        
    
    
 estudiantes_vig <- base |> 
      group_by(cve_sexo,Sec,leng_indigena, Nivel_estudios_jefe,
               indice) |> 
      count(vigencia) |> 
      pivot_wider(names_from = vigencia, 
                                 values_from = n, values_fill = 0) |> 
      rename("baja"="2", "vigente"="1" )
 
 
 # estudiantes_vig_cp <- base |> 
 #   group_by(cve_sexo,Sec,nivel_ingresos) |> 
 #   summarise(avg_calif=mean(calif_avg))

 
 bf <-merge(estudiantes,estudiantes_vig) |> 
   select(- vigente)
 
#################################################################################
################# Modelo Poisson ##############################
#####    obj - estimar probabilidades de permanecer vigentes por estado #####
#####              considerado var. explicativas                    #####
#################################################################################


n<-nrow(bf)
par(mfrow=c(2,2))
plot(bf$baja)
plot(bf$baja/bf$n)

#-Defining data-
data<-list("n"=n,"y"=bf$baja,"ne"=bf$n,"S"=bf$cve_sexo,
           "I"=bf$leng_indigena, "H"=bf$Sec, "J"=bf$Nivel_estudios_jefe, "K"=bf$indice)


#-Defining inits-
inits<-function(){list(alpha=0,beta=rep(0,2),gama=rep(0,3), delta=rep(0,2),
                       etha=rep(0,2), etha2=rep(0,1),yf1=rep(1,n))}

#-Selecting parameters to monitor-
pars<-c("alpha.adj","beta.adj","gama.adj", "delta.adj","etha.adj", "etha2.adj",
        "yf1")

#-Running code-
#OpenBUGS
ej.sim<-bugs(data,inits,pars,model.file="modelo_Poisson_Estudios_leng_completo.txt",
               n.iter=50000,n.chains=2,n.burnin=5000, debug = TRUE)


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




