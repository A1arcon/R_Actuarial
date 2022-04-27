
###Conmutados Actuariales###
setwd("~/Actuaría/AMAT/R básico/Edgar/Bases de Datos/Conmutados")
library(XLConnect)
Vida<-readWorksheetFromFile("Conmutados Actuariales.xlsx",sheet="Conmutados")
View(Vida)

#Calculamos las Px
Vida$px<-1-Vida$qx

#Establecemos un radix
radix<-1000000

#Calculamos lx
for(i in 1:length(Vida$Edad)){
  
  #El radix son todos los individuos vivos en la primera edad.
  if(i==1){
    lx<-c(radix)
    next #Ve al siguiente valor de "i" una vez hecho lo anterior.
  }
  
  #Realizamos el cálculo de los demás
  lx[i]<-Vida$px[i-1]*lx[i-1]
  
}

#Guardamos los valores en la base de datos:
Vida$lx<-lx ; rm(lx,radix)

#Podemos calcular dx ya de la siguiente manera:
Vida$dx <- Vida$lx*Vida$qx

#Establecemos una tasa de interés
Tasa<-0.055 #=5.50%

#Calculamos las Dx
Vida$Dx<-Vida$lx*(1+Tasa)^(-Vida$Edad)

#Calculamos las Nx
i<-1 ; Nx<-c()
attach(Vida)
while(i<=length(Edad)){
  
  Nx[i]<-sum(Dx[i:length(Edad)])
  i<-i+1
  
}
detach(Vida)

#Guardamos los valores en la base de datos:
Vida$Nx<-Nx ; rm(Nx)

#Calculamos las Sx
i<-1 ; Sx<-c()
attach(Vida)
repeat{
  
  Sx[i]<-sum(Nx[i:length(Edad)])
  i<-i+1
  
  if(i>length(Edad)){break}
  
}
detach(Vida)

#Guardamos los valores en la base de datos:
Vida$Sx<-Sx ; rm(Sx)

#Calculamos las Cx
Vida$Cx<-Vida$dx*(1+Tasa)^-(Vida$Edad+1)

#Calculamos las Mx
i<-1 ; Mx<-c()
attach(Vida)
while(i<=length(Edad)){
  
  Mx[i]<-sum(Cx[i:length(Edad)])
  i<-i+1
  
}
detach(Vida)

#Guardamos los valores en la base de datos:
Vida$Mx<-Mx ; rm(Mx)

#Calculamos las Rx
i<-1 ; Rx<-c()
attach(Vida)
repeat{
  
  Rx[i]<-sum(Mx[i:length(Edad)])
  i<-i+1
  
  if(i>length(Edad)){break}
  
}
detach(Vida)

#Guardamos los valores en la base de datos:
Vida$Rx<-Rx ; rm(Rx,i,Tasa)
View(Vida)
#Podemos aplicar el siguiente truco para facilitar llamadas a la base:
rownames(Vida)<-Vida$Edad

#Algunas especificaciones
Vida[12,]
Vida["12",]
Vida[as.character(12),]
Vida["12","qx"]


#a) Calcular la probabilidad de que una persona de edad 12 sobreviva hasta edad 14
prod(Vida[1:2,"px"])

#b) Calcular la probabilidad de que una persona de edad 30 sobreviva hasta edad 65
View(Vida[as.character(30:64),])
prod(Vida[as.character(30:64),"px"])

#Bajo el principio de equivalencia:

#c) Calcular la tasa de interes anual "i" tal que el valor presente actuarial de $300 sean $100
#para una persona de edad 25 en un periodo de 40 años.

tPx <- prod(Vida[as.character(25:(25+39)),"px"])
abs(tPx - Vida["65","lx"]/Vida["25","lx"]) #Comprobación 40p25
V_t <- solve(300*tPx,100)  
i = 1/(V_t^(1/40)) - 1

#Comprobación
300*tPx*(1+i)^(-40)

#d) Calcula la prima neta única de un seguro dotal puro para una persona 
#de edad 28 temporal 42 años con una tasa de interés anual del 5.5% que tiene
#una suma asegurada de $10,000.
V_t <- (1+0.055)^(-42)
tPx <- prod(Vida[as.character(28:(28+42-1)),"px"])
tPx <- Vida[as.character(28+42),"lx"]/Vida[as.character(28),"lx"]
PNU<-V_t*tPx*10000 ; PNU

#Lo mismo pero con los conmutados
Dotal_Puro<-Vida[as.character(28+42),"Dx"]/Vida["28","Dx"]
PNU<-10000*Dotal_Puro ; PNU

#e)Haz el mismo cálculo pero para una persona de edad 25 temporal 1
V_t <- (1+0.055)^(-1)
tPx <- prod(Vida[as.character(25:(25+1-1)),"px"])
PNU<-V_t*tPx*10000 ; PNU

#Lo mismo pero con los conmutados
Dotal_Puro<-Vida[as.character(25+1),"Dx"]/Vida["25","Dx"]
PNU<-10000*Dotal_Puro ; PNU

#f) Calcula la prima neta única de un seguro que paga en caso de fallecimiento
#para una persona de edad 70 temporal 2 con una tasa de interés anual del 5.5% que tiene
#una suma asegurada de $1,000,000 al final del año de fallecimiento.
SA <- 1000000
V_t <- c((1+0.055)^-1,(1+0.055)^-2)
qx <- Vida[as.character(70:71),"qx"]
tPx <- c(1,Vida["70","px"])
#Cálculo
PNU <- SA * sum(V_t*qx*tPx) ; PNU

#Lo mismo pero con conmutados
Seguro_Temporal<-(Vida["70","Mx"]-Vida[as.character(70+2),"Mx"])/
                  Vida["70","Dx"]
PNU<-SA*Seguro_Temporal ; PNU
