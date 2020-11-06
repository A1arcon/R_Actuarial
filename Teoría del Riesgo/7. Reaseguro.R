
# Stop loss

#Número de simulaciones de N's que tendremos
n=100000

#N~Bin(size=5,prob=0.15)
size=5 ; prob=0.15

#Nivel de retención
M=2

#P[X<=M]:=a
a = 0.9

#Obtengamos N's
regresa.Ns<-function(){
  
  #Genera una N
  N<-rbinom(n = 1,size = size,prob = prob)
  
  #Calcula los daños
  if(N>0){
    Xj <- sample(x = 1:3,size = N,
                 replace = T,prob = c(0.8,0.1,0.1)) #Genera las que hubo.
  }else{
    Xj <- 0 #Si no hubo, el total es cero.
  }
  
  Na = ifelse(N==0,0,sum(Xj<=M))
  NR = sum(Xj>M)
  
  aux<-c(N,Na,NR)
  names(aux)<-c("N","NA","NR")
  
  #Regresa unas N
  return(aux)
  
}

#
set.seed(2012)
Ns = replicate(n = n, #Número de veces
               expr = regresa.Ns()) #Expresión

#Notemos que Ns es una matriz
dim(Ns)

#Veamos un poco
Ns[,1:10]

#Veamos un poco de cada una
apply(X = Ns,MARGIN = 1,FUN = summary)

#Veamos las probabilidades de cada una
ProbNs <- apply(X = Ns,MARGIN = 1,
                FUN = function(x){table(x)/length(x)})

#Comparemos contra sus densidades según el resultado visto en las notas:

#Para N

#Valores
dbinom(x = 0:5,size = size,prob = prob) #Reales
ProbNs$N #Muestrales


#Barplot
#Reales
barplot(dbinom(x = 0:5,size = size,prob = prob), ylab=expression(P(N==x)), 
        main="Densidad de N",col="gold")
#Muestrales
barplot(ProbNs$N,col="skyblue",add=T,axisnames = F)

#Para NA

#Valores
dbinom(x = 0:5,size = size,prob = prob*a) #Reales
ProbNs$`NA` #Muestrales


#Barplot
#Reales
barplot(dbinom(x = 0:5,size = size,prob = prob*a), ylab=expression(P(N==x)), 
        main="Densidad de NA",col="gold")
#Muestrales
barplot(ProbNs$`NA`,col="skyblue",add=T,axisnames = F)

#Para NR

#Valores
dbinom(x = 0:5,size = size,prob = prob*(1-a)) #Reales
ProbNs$NR #Muestrales


#Barplot
#Reales
barplot(dbinom(x = 0:5,size = size,prob = prob*(1-a)), ylab=expression(P(N==x)), 
        main="Densidad de N",col="gold")
#Muestrales
barplot(ProbNs$NR,col="skyblue",add=T,axisnames = F)
