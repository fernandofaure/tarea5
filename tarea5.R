####   TAREA 5   ####


library(RJSONIO)
library(foreign)
library(ggplot2)
library(quantmod)
library(tidyverse)
library(dplyr)


#------Pregunta 2-------#
#1

getSymbols(c("MSFT"), src="yahoo", from=as.Date("2000-01-01 "), to=as.Date("2018-08-31"), 
           periodicity="monthly")

getSymbols("AAPL", src="yahoo", from=as.Date("2000-01-01 "), to=as.Date("2018-08-31"), 
           periodicity="monthly")

for (i in 1:224) {
  MSFT$n[i]=i
} 
for (i in 1:224) {
  AAPL$n[i]=i
} 

data= merge(AAPL,MSFT, by.x = "n",by.y="n")

#2

func.fin<-function(x){
  #retornos
  if(x==1){
    for(i in 2:224){
      data$retorno[1]<-Delt(data$MSFT.Close[1],data$MSFT.Open[1])
      data$retorno[i]<-Delt(data$MSFT.Close[i],data$MSFT.Close[i-1])
    }
    #retornos acumulados
    for(i in 1:224){
      data$retorno.ac[i=1]<-data$retorno[i=1]
      data$retorno.ac[i]<-Delt(data$MSFT.Close[i],data$MSFT.Close[1])
    }  
    #retorno promedio
    p.ret.MSFT<-sum(data$retorno)/224
    #calculamos el retorno acumulado promedio
    p.ret.ac.MSFT<-sum(data$retorno.ac)/224
    # desviaciones respecto a la media elevadas a la cuarta del retorno
    for(i in 1:224 ){
      data$cuarta.ret[i]=(data$retorno[i]-p.ret.MSFT)^{4}
    }
    sum.cuarta.ret.MSFT<-sum(data$cuarta.ret)
    # suma  de las desviaciones respecto a la media elevadas a la cuarta del retorno acumulado
    for(i in 1:224 ){
      data$cuarta.ret.ac[i]=(data$retorno.ac[i]-p.ret.ac.MSFT)^{4}
    }
    sum.cuarta.ret.ac.MSFT<-sum(data$cuarta.ret.ac)
    #suma de las desviaciones respecto a la media elevadas al cubo del retorno
    for(i in 1:224 ){
      data$cubos.ret[i]=(data$retorno[i]-p.ret.MSFT)^{3}
    }
    sum.cubos.ret.MSFT<-sum(data$cubos.ret)
    #suma de las desviaciones respecto a la media elevadas al cubo del retorno acumulado
    for(i in 1:224 ){
      data$cubos.ret.ac[i]=(data$retorno.ac[i]-p.ret.ac.MSFT)^{3}
    }
    sum.cubos.ret.ac.MSFT<-sum(data$cubos.ret.ac)
    #suma de las desviaciones respecto a la media elevadas al cuadrado del retorno
    for(i in 1:224 ){
      data$cuadrado.ret[i]=(data$retorno[i]-p.ret.MSFT)^{2}
    }
    sum.cuadrados.ret.MSFT<-sum(data$cuadrado.ret)
    #suma de las desviaciones respecto a la media elevadas al cuadrado del retorno acumulado
    for(i in 1:224 ){
      data$cuadrado.ret.ac[i]=(data$retorno.ac[i]-p.ret.ac.MSFT)^{2}
    }
    sum.cuadrados.ret.ac.MSFT<-sum(data$cuadrado.ret.ac)
    # Skeness del retorno
    Skeness.ret.MSFT<- (sum.cubos.ret.MSFT/224)/((sum.cuadrados.ret.MSFT/x)^{3/2})
    #Skeness del retorno acumulado
    Skeness.ret.ac.MSFT<- (sum.cubos.ret.ac.MSFT/224)/((sum.cuadrados.ret.ac.MSFT/x)^{3/2})
    #Kurtosis del retorno
    Kurtosis.ret.MSFT<-(sum.cuarta.ret.MSFT/224)/((sum.cuadrados.ret.MSFT/x)^{2})
    #Kurtosis del retorno acumulado
    Kurtosis.ret.ac.MSFT<-(sum.cuarta.ret.ac.MSFT/224)/((sum.cuadrados.ret.ac.MSFT/x)^{2})
    # Jarque Bera
    JB.ret.MSFT<-224*(((Skeness.ret.MSFT^{2})/6) + (Kurtosis.ret.MSFT-3)^{2}/24)
    #Definimos el estadistico de Jarque Bera para el retorno acumulado
    JB.ret.ac.MSFT<-224*(((Skeness.ret.ac.MSFT^{2})/6) + (Kurtosis.ret.ac.MSFT-3)^{2}/24)
    
    
    data.ret = data.frame(data$retorno,data$n, data$retorno.ac)
    Hipotesis_Nula<-c("El retorno sigue una distribución normal")
    Cantidad_de_precios<-224
    Test_Jarque_Bera_para_Retorno<-"-"
    Valor_critico_al_90<-ifelse(JB.ret.MSFT>qchisq(0.90,2),"Se rechaza H_0","No se rechaza H_0")
    Valor_critico_al_95<-ifelse(JB.ret.MSFT>qchisq(0.95,2),"Se rechaza H_0","No se rechaza H_0")
    Valor_critico_al_99<-ifelse(JB.ret.MSFT>qchisq(0.99,2),"Se rechaza H_0", "No se rechaza H_0")
    tabla.ret = rbind(Test_Jarque_Bera_para_Retorno,Hipotesis_Nula, Cantidad_de_precios, JB.ret.MSFT,Valor_critico_al_90,Valor_critico_al_95,Valor_critico_al_99)
    
    Hipotesis__Nula<-c("El retorno acumulado sigue una distribución normal")
    Test_Jarque_Bera_para_Retorno_Acumulado<-"-"
    Valor_critico_al__90<-ifelse(JB.ret.ac.MSFT>qchisq(0.90,2),"Se rechaza H_0","No se rechaza H_0")
    Valor_critico_al__95<-ifelse(JB.ret.ac.MSFT>qchisq(0.95,2),"Se rechaza H_0","No se rechaza H_0")
    Valor_critico_al__99<-ifelse(JB.ret.ac.MSFT>qchisq(0.99,2),"Se rechaza H_0", "No se rechaza H_0")
    tabla.ret.ac = rbind(Test_Jarque_Bera_para_Retorno_Acumulado,Hipotesis__Nula, Cantidad_de_precios, JB.ret.ac.MSFT,Valor_critico_al_90,Valor_critico_al__95,Valor_critico_al__99)
    
    
    grafRet = data.ret %>% ggplot(aes(n,retorno))+geom_line()
    grafRetacum= data.ret %>% ggplot(aes(n,retorno.ac))+geom_line()
    
    mostrar<-list(grafRet,tabla.ret,grafRetacum,tabla.ret.ac)
    return(mostrar)
  }
  else {
    for(i in 2:224){
      data$retorno[1]<-Delt(data$MSFT.Close[1],data$MSFT.Open[1])
      data$retorno[i]<-Delt(data$MSFT.Close[i],data$MSFT.Close[i-1])}
    
    #retornos acumulados
    for(i in 1:224){
      data$retorno.ac[1]<-data$retorno[i=1]
      data$retorno.ac[i]<-Delt(data$MSFT.Close[i],data$MSFT.Close[1])
    }  
    #retorno promedio
    p.ret.MSFT<-sum(data$retorno)/224
    #calculamos el retorno acumulado promedio
    p.ret.ac.MSFT<-sum(data$retorno.ac)/224
    # desviaciones respecto a la media elevadas a la cuarta del retorno
    for(i in 1:224 ){
      data$cuarta.ret[i]=(data$retorno[i]-p.ret.MSFT)^{4}
    }
    sum.cuarta.ret.MSFT<-sum(data$cuarta.ret)
    # suma  de las desviaciones respecto a la media elevadas a la cuarta del retorno acumulado
    for(i in 1:224 ){
      data$cuarta.ret.ac[i]=(data$retorno.ac[i]-p.ret.ac.MSFT)^{4}
    }
    sum.cuarta.ret.ac.MSFT<-sum(data$cuarta.ret.ac)
    #suma de las desviaciones respecto a la media elevadas al cubo del retorno
    for(i in 1:224 ){
      data$cubos.ret[i]=(data$retorno[i]-p.ret.MSFT)^{3}
    }
    sum.cubos.ret.MSFT<-sum(data$cubos.ret)
    #suma de las desviaciones respecto a la media elevadas al cubo del retorno acumulado
    for(i in 1:224 ){
      data$cubos.ret.ac[i]=(data$retorno.ac[i]-p.ret.ac.MSFT)^{3}
    }
    sum.cubos.ret.ac.MSFT<-sum(data$cubos.ret.ac)
    #suma de las desviaciones respecto a la media elevadas al cuadrado del retorno
    for(i in 1:224 ){
      data$cuadrado.ret[i]=(data$retorno[i]-p.ret.MSFT)^{2}
    }
    sum.cuadrados.ret.MSFT<-sum(data$cuadrado.ret)
    #suma de las desviaciones respecto a la media elevadas al cuadrado del retorno acumulado
    for(i in 1:224 ){
      data$cuadrado.ret.ac[i]=(data$retorno.ac[i]-p.ret.ac.MSFT)^{2}
    }
    sum.cuadrados.ret.ac.MSFT<-sum(data$cuadrado.ret.ac)
    # Skeness del retorno
    Skeness.ret.MSFT<- (sum.cubos.ret.MSFT/224)/((sum.cuadrados.ret.MSFT/x)^{3/2})
    #Skeness del retorno acumulado
    Skeness.ret.ac.MSFT<- (sum.cubos.ret.ac.MSFT/224)/((sum.cuadrados.ret.ac.MSFT/x)^{3/2})
    #Kurtosis del retorno
    Kurtosis.ret.MSFT<-(sum.cuarta.ret.MSFT/224)/((sum.cuadrados.ret.MSFT/x)^{2})
    #Kurtosis del retorno acumulado
    Kurtosis.ret.ac.MSFT<-(sum.cuarta.ret.ac.MSFT/224)/((sum.cuadrados.ret.ac.MSFT/x)^{2})
    # Jarque Bera
    JB.ret.MSFT<-224*(((Skeness.ret.MSFT^{2})/6) + (Kurtosis.ret.MSFT-3)^{2}/24)
    #Definimos el estadistico de Jarque Bera para el retorno acumulado
    JB.ret.ac.MSFT<-224*(((Skeness.ret.ac.MSFT^{2})/6) + (Kurtosis.ret.ac.MSFT-3)^{2}/24)
    
    
    data.ret = data.frame(data$retorno,data$n, data$retorno.ac)
    Hipotesis_Nula<-c("El retorno sigue una distribución normal")
    Cantidad_de_precios<-224
    Test_Jarque_Bera_para_Retorno<-"-"
    Valor_critico_al_90<-ifelse(JB.ret.MSFT>qchisq(0.90,2),"Se rechaza H_0","No se rechaza H_0")
    Valor_critico_al_95<-ifelse(JB.ret.MSFT>qchisq(0.95,2),"Se rechaza H_0","No se rechaza H_0")
    Valor_critico_al_99<-ifelse(JB.ret.MSFT>qchisq(0.99,2),"Se rechaza H_0", "No se rechaza H_0")
    tabla.ret = rbind(Test_Jarque_Bera_para_Retorno,Hipotesis_Nula, Cantidad_de_precios, JB.ret.MSFT,Valor_critico_al_90,Valor_critico_al_95,Valor_critico_al_99)
    
    Hipotesis__Nula<-c("El retorno acumulado sigue una distribución normal")
    Test_Jarque_Bera_para_Retorno_Acumulado<-"-"
    Valor_critico_al__90<-ifelse(JB.ret.ac.MSFT>qchisq(0.90,2),"Se rechaza H_0","No se rechaza H_0")
    Valor_critico_al__95<-ifelse(JB.ret.ac.MSFT>qchisq(0.95,2),"Se rechaza H_0","No se rechaza H_0")
    Valor_critico_al__99<-ifelse(JB.ret.ac.MSFT>qchisq(0.99,2),"Se rechaza H_0", "No se rechaza H_0")
    tabla.ret.ac = rbind(Test_Jarque_Bera_para_Retorno_Acumulado,Hipotesis__Nula, Cantidad_de_precios, JB.ret.ac.MSFT,Valor_critico_al_90,Valor_critico_al__95,Valor_critico_al__99)
    
    
    grafRet = data.ret %>% ggplot(aes(n,retorno))+geom_line()
    grafRetacum= data.ret %>% ggplot(aes(n,retorno.ac))+geom_line()
    
    
    for(i in 2:224){
      data$retornoA[1]<-Delt(data$AAPL.Close[1],data$AAPL.Open[1])
      data$retornoA[i]<-Delt(data$AAPL.Close[i],data$AAPL.Close[i-1])
    }
    
    for(i in 2:224){
      data$retorno.acA[i=1]<-data$retorno[i=1]
      data$retorno.acA[i]<-Delt(data$AAPL.Close[i],data$AAPL.Close[1])
    }  
    
    m.ret.AAPL<-sum(data$retornoA)/224
    
    m.ret.ac.AAPL<-sum(data$retorno.acA)/224
    
    for(i in 1:224){
      data$cuarta.retA[i]=(data$retornoA[i]-m.ret.AAPL)^{4}
    }
    sum.cuarta.ret.AAPL<-sum(data$cuarta.retA)
    
    for(i in 1:224 ){
      data$cuarta.ret.acA[i]=(data$retorno.acA[i]-m.ret.ac.AAPL)^{4}
    }
    sum.cuarta.ret.ac.AAPL<-sum(data$cuarta.ret.acA)
    
    for(i in 1:224 ){
      data$cubos.retA[i]=(data$retornoA[i]-m.ret.AAPL)^{3}
    }
    sum.cubos.ret.AAPL<-sum(data$cubos.retA)
    
    for(i in 1:224 ){
      data$cubos.ret.acA[i]=(data$retorno.acA[i]-m.ret.ac.AAPL)^{3}
    }
    sum.cubos.ret.ac.AAPL<-sum(data$cubos.ret.acA)
    
    for(i in 1:224 ){
      data$cuadrado.retA[i]=(data$retornoA[i]-m.ret.AAPL)^{2}
    }
    sum.cuadrados.ret.AAPL<-sum(data$cuadrado.retA)
    
    for(i in 1:224 ){
      data$cuadrado.ret.acA[i]=(data$retorno.acA[i]-m.ret.ac.AAPL)^{2}
    }
    sum.cuadrados.ret.ac.AAPL<-sum(data$cuadrado.ret.acA)
    
    Skeness.ret.AAPL<- (sum.cubos.ret.AAPL/224)/((sum.cuadrados.ret.AAPL/224)^{3/2})
    
    Skeness.ret.ac.AAPL<- (sum.cubos.ret.ac.AAPL/224)/((sum.cuadrados.ret.ac.AAPL/x)^{3/2})
    
    Kurtosis.ret.AAPL<-(sum.cuarta.ret.AAPL/224)/((sum.cuadrados.ret.AAPL/224)^{2})
    
    Kurtosis.ret.ac.AAPL<-(sum.cuarta.ret.ac.AAPL/224)/((sum.cuadrados.ret.ac.AAPL/224)^{2})
    
    JB.ret.AAPL<-224*(((Skeness.ret.AAPL^{2})/6) + (Kurtosis.ret.AAPL-3)^{2}/24)
    
    JB.ret.ac.AAPL<-224*(((Skeness.ret.ac.AAPL^{2})/6) + (Kurtosis.ret.ac.AAPL-3)^{2}/24)
    
    
    data.ret = data.frame(data$retornoA,data$n, data$retorno.acA)
    Hipotesis_Nula<-c("El retorno sigue una distribución normal")
    Cantidad_de_precios<-224
    Test_Jarque_Bera_para_Retorno<-"-"
    Valor_critico_al_90a<-ifelse(JB.ret.AAPL>qchisq(0.90,2),"Se rechaza H_0","No se rechaza H_0")
    Valor_critico_al_95a<-ifelse(JB.ret.AAPL>qchisq(0.95,2),"Se rechaza H_0","No se rechaza H_0")
    Valor_critico_al_99a<-ifelse(JB.ret.AAPL>qchisq(0.99,2),"Se rechaza H_0", "No se rechaza H_0")
    tabla.retA = rbind(Test_Jarque_Bera_para_Retorno,Hipotesis_Nula, Cantidad_de_precios, JB.ret.AAPL,Valor_critico_al_90a,Valor_critico_al_95a,Valor_critico_al_99a)
    
    Hipotesis__Nula<-c("El retorno acumulado sigue una distribución normal")
    Test_Jarque_Bera_para_Retorno_Acumulado<-"-"
    Valor_critico_al__90a<-ifelse(JB.ret.ac.AAPL>qchisq(0.90,2),"Se rechaza H_0","No se rechaza H_0")
    Valor_critico_al__95a<-ifelse(JB.ret.ac.AAPL>qchisq(0.95,2),"Se rechaza H_0","No se rechaza H_0")
    Valor_critico_al__99a<-ifelse(JB.ret.ac.AAPL>qchisq(0.99,2),"Se rechaza H_0", "No se rechaza H_0")
    tabla.ret.acA = rbind(Test_Jarque_Bera_para_Retorno_Acumulado,Hipotesis__Nula, Cantidad_de_precios, JB.ret.ac.AAPL,Valor_critico_al_90a,Valor_critico_al__95a,Valor_critico_al__99a)
    
    
    grafRetA = data.ret %>% ggplot(aes(n,retornoA))+geom_line()
    grafRetacumA= data.ret %>% ggplot(aes(n,retorno.acA))+geom_line()
    
    
    resultado<-list(grafRetA,tabla.retA,grafRetacumA,tabla.ret.acA,grafRet,tabla.ret,grafRetacum,tabla.ret.ac)
    return(resultado)
  }
}
func.fin(2)


#######Pregunta 3
#a
set.seed(100)
reps=10000
betas=matrix(NA,nrow=reps,ncol=8)
beta0=2
beta1=2.5
beta2=1
n=c(50,100,500,1000)
for(j in 1:length(n)){
  x1<-rnorm(n[j],20,1)
  x2=0.8*x1+rnorm(n[j],0,1)
  for(i in 1:reps){
    u<-rnorm(n[j],0,1)
    y=beta0+beta1*x1+beta2*x2+u
    model=lm(y~x1)
    betas[i,j]=model$coef[1]
    betas[i,j+4]=model$coef[2]
    
  }
  
}
betas_df<-data.frame(betas)
apply(betas_df,2,mean)
apply(betas_df,2,var)

#b
graf1b<-ggplot(betas_df, aes(X5))+geom_histogram(col="red", bins=50)
graf2b<-ggplot(betas_df, aes(X6))+geom_histogram(col="red", bins=50)
graf3b<-ggplot(betas_df, aes(X7))+geom_histogram(col="red", bins=50)
graf4b<-ggplot(betas_df, aes(X8))+geom_histogram(col="red", bins=50)
#c
set.seed(100)
reps=10000
betas=matrix(NA,nrow=reps,ncol=8)
beta0=2
beta1=2.5
beta2=1
n=c(50,100,500,1000)
for(j in 1:length(n)){
  x1<-rnorm(n[j],20,1)
  x2=runif(n[j],0,1)
  for(i in 1:reps){
    u<-rnorm(n[j],0,1)
    y=beta0+beta1*x1+beta2*x2+u
    model=lm(y~x1)
    betas[i,j]=model$coef[1]
    betas[i,j+4]=model$coef[2]
    
  }
  
}
betas_df<-data.frame(betas)
apply(betas_df,2,mean)
apply(betas_df,2,var)

graf1c<-ggplot(betas_df, aes(X5))+geom_histogram(col="red", bins=50)
graf2c<-ggplot(betas_df, aes(X6))+geom_histogram(col="red", bins=50)
graf3c<-ggplot(betas_df, aes(X7))+geom_histogram(col="red", bins=50)
graf4c<-ggplot(betas_df, aes(X8))+geom_histogram(col="red", bins=50)

graf1c
graf2c
graf3c
graf4c
