library(readxl)
library(tidyverse)
library(R2OpenBUGS)


## Modelo con secu, ingreso, lenguaindigena,indice, escolaridad padre
#--- Funciones utiles ---
prob<-function(x){
  out<-min(length(x[x>0])/length(x),length(x[x<0])/length(x))
  out
}

#-Lectura de datos-
base <- read_excel("2022-04-25-BDUACh_v2.xlsx")
base %>% count(indice)

#1.- Vigente
#2.- Baja definitiva
estudiantes <- base |> 
                 group_by(cve_sexo,Sec,Nivel_estudios_jefe, lengua_indigena,
                          indice, nivel_ingresos, piso_de_tierra) |> 
               count()        
 
 
    
 estudiantes_vig <- base |> 
      group_by(cve_sexo,Sec,Nivel_estudios_jefe, lengua_indigena,
               indice, nivel_ingresos, piso_de_tierra) |> 
      count(vigencia) |> 
      pivot_wider(names_from = vigencia, 
                                 values_from = n, values_fill = 0) |> 
      rename("baja"="2", "vigente"="1" )

 
 bf <-merge(estudiantes,estudiantes_vig) |> 
   select(- vigente)
 ##  1 tiene carencias, 2 no tiene carencias
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
data<-list("n"=n,"y"=bf$baja,"ne"=bf$n,
           "S"=bf$cve_sexo,"I"=bf$Nivel_estudios_jefe,
           "H"=bf$Sec,"K"=bf$lengua_indigena,
           "M"=bf$indice, "N"=bf$nivel_ingresos,
           "P"=bf$piso_de_tierra)


#-Defining inits-
inits<-function(){list(alpha=0,beta1=rep(0,2),beta2=rep(0,3), beta3=rep(0,2),
                       beta4=rep(0,2),beta5=rep(0,2),
                       beta6=rep(0,8), beta7=rep(0,2), yf1=rep(1,n))}


#-Selecting parameters to monitor-
pars<-c("alpha.adj","beta1.adj","beta2.adj","beta3.adj",
        "beta4.adj","beta5.adj","beta6.adj","beta7.adj","yf1")

#-Running code-
#OpenBUGS
ej.sim<-bugs(data,inits,pars,model.file="R_Modelo2.txt",
               n.iter=50000,n.chains=2,n.burnin=5000, debug = TRUE)


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




