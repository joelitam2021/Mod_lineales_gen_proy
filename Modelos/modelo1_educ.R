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
base %>% count(Nivel_estudios_jefe)

#1.- Licenciatura y posgrado
#2.- Bachillerato y técnico superior universitario
#3.- Educación básica: prim y secu

 estudiantes <- base |> 
                 group_by(cve_sexo,Sec,Nivel_estudios_jefe) |> 
                 count()        
    
    
 estudiantes_vig <- base |> 
      group_by(cve_sexo,Sec,Nivel_estudios_jefe) |> 
      count(vigencia) |> 
      pivot_wider(names_from = vigencia, 
                                 values_from = n, values_fill = 0) |> 
      rename("baja"="2", "vigente"="1" )
 
 
 # estudiantes_vig_cp <- base |> 
 #   group_by(cve_sexo,Sec,nivel_ingresos) |> 
 #   summarise(avg_calif=mean(calif_avg))

 
 bf <-merge(estudiantes,estudiantes_vig) |> 
   select(-baja)
 
#################################################################################
################# Modelo Poisson ##############################
#####    obj - estimar probabilidades de permanecer vigentes por estado #####
#####              considerado var. explicativas                    #####
#################################################################################


n<-nrow(bf)
par(mfrow=c(2,2))
plot(bf$vigente)
plot(bf$vigente/bf$n)

#-Defining data-
data<-list("n"=n,"y"=bf$vigente,"ne"=bf$n,"S"=bf$cve_sexo,
           "I"=bf$Nivel_estudios_jefe, "H"=bf$Sec)


#-Defining inits-
inits<-function(){list(alpha=0,beta=rep(0,2),gama=rep(0,3), delta=rep(0,2), yf1=rep(1,n))}

#-Selecting parameters to monitor-
pars<-c("alpha.adj","beta.adj","gama.adj", "delta.adj", "yf1")

#-Running code-
#OpenBUGS
ej.sim<-bugs(data,inits,pars,model.file="modelo_Poisson_Estudios.txt",
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




