# Familia (a,b,0) ---------------------------------------------------------

#Recursión
pk<-function(k,a,b,p0){
  
  while(k>0){
    return((a+b/k)*pk(k-1,a,b,p0))
  }
  
  return(p0)
  
}

cociente<-function(k,a,b,p0){
  k*pk(k,a,b,p0)/pk(k-1,a,b,p0)
}

#Poisson(lambda)
lambda<-2

a<-0
b<-lambda
p0<-exp(-lambda)

Teóricos<-dpois(x = 0:10,
                lambda = lambda)
Familia<-sapply(0:10, pk, a=a, b=b, p0=p0)

#Comparación
plot(Teóricos~Familia,
     col=rainbow(length(Familia)), pch=16)
abline(a = 0, b = 1, col="blue", lwd = 1)

#Gráfico de densidad
plot(x = 0:10,y = Familia,
     col=rainbow(length(Familia)), pch=16)

#Gráfico del cociente:
plot(x = 1:10,y = sapply(X = 1:10,
                FUN = cociente,a=a,b=b,p0=p0),
     col=rainbow(length(Familia)), pch=16,ylab = expression(k*p[k]/p[k-1]))

#Binomial(10,0.5)
p0<-dbinom(x = 0,size = 10,prob = 0.5)
p1<-dbinom(x = 1,size = 10,prob = 0.5)
p2<-dbinom(x = 2,size = 10,prob = 0.5)

# 1*a + 1*b   = p1/p0
# 1*a + 0.5*b = p2/p1

A<-matrix(data = c(1,1,1,0.5),nrow = 2)
colnames(A)<-c("a","b")

X<-solve(A,c(p1/p0,p2/p1))

a<-X["a"] 
b<-X["b"]

Teóricos<-dbinom(x = 0:10,size = 10,prob = 0.5)
Familia<-sapply(0:10, pk, a=a, b=b, p0=p0)

# Bueno... difieren salvo detales numéricos
#sum(abs(Teóricos-Familia))+1000-1000

#Comparación
plot(Teóricos~Familia,
     col=rainbow(length(Familia)), pch=16)
abline(a = 0, b = 1, col="blue", lwd = 1)

#Gráfico de densidad
plot(x = 0:10,y = Familia,type="h",
     col=rainbow(length(Familia)), pch=16)

#Gráfico del cociente:
plot(x = 1:10,y = sapply(X = 1:10,FUN = cociente,a=a,b=b,p0=p0),
     col=rainbow(length(Familia)), pch=16,ylab = expression(k*p[k]/p[k-1]))

#Binomial negativa(10,0.5)
p0<-dnbinom(x = 0,size = 10,prob = 0.5)
p1<-dnbinom(x = 1,size = 10,prob = 0.5)
p2<-dnbinom(x = 2,size = 10,prob = 0.5)

A<-matrix(data = c(1,1,1,0.5),nrow = 2)
colnames(A)<-c("a","b")

X<-solve(A,c(p1/p0,p2/p1))

a<-X["a"] 
b<-X["b"]

Teóricos<-dnbinom(x = 0:10,size = 10,prob = 0.5)
Familia<-sapply(0:10, pk, a=a, b=b, p0=p0)

#Comparación
plot(Teóricos~Familia,
     col=rainbow(length(Familia)), pch=16)
abline(a = 0, b = 1, col="blue", lwd = 1)

#Gráfico de densidad
plot(x = 0:10,y = Familia,
     col=rainbow(length(Familia)), pch=16)

#Gráfico de CDF (hasta 10)
plot(x = 0:10,y = cumsum(Familia),type="s",
     col=rainbow(length(Familia)), pch=16)

#Gráfico del cociente:
plot(x = 1:10,y = sapply(X = 1:10,FUN = cociente,a=a,b=b,p0=p0),
     col=rainbow(length(Familia)), pch=16,ylab = expression(k*p[k]/p[k-1]))


# Familia (a,b,1) ---------------------------------------------------------

#Cero truncado

#(Último ejemplo anterior)

#p1T inicial 
p1T<-p1/(1-p0) ; p1T

#p2T recursiva
p2T<-p1T*(a+b/2) ; p2T

#p2T directa
p2T<-p2/(1-p0) ; p2T

#Cero modificado

#p0M dado:
p0M<-0.27

#p1M inicial 
p1M<-p1*(1-p0M)/(1-p0) ; p1M

#p2M recursiva
p2M<-p1M*(a+b/2) ; p2M

#p2M directa
p2M<-p2*(1-p0M)/(1-p0) ; p2M

#Utilizando la función anterior
pk(k = 1,a = a,b = b,p0 = p0M) #No funciona
p1M

#Debemos crear una ligera modificación
#Recursión
pkM<-function(k,a,b,p0,p0M=p0){
  
  
  while(k>1){
    aux=(a+b/k)*pkM(k-1,a,b,p0,p0M)
    names(aux)=paste("P[X=",as.character(k),"]",sep = "")
    return(aux)
  }
 
  #Cuando vale 1, ¡OJO! UTILIZA LA pk anterior.
  if(k==1){
  aux=pk(1,a,b,p0)*(1-p0M)/(1-p0)
  names(aux)=paste("P[X=",as.character(k),"]",sep = "")
  return(aux)
  }
  
  else{
  aux=p0M
  names(aux)=paste("P[X=0]")
  return(aux)
  }
  
}

#Experimentemos
sapply(X = 0:2, FUN = pkM,a=a,b=b,p0=p0)==c(p0,p1,p2)

#Ahora con los modificados
sapply(X = 0:2, FUN = pkM,a=a,b=b,p0=p0,p0M=p0M)==c(p0M,p1M,p2M)

# dato perturbador
sum(sapply(X = 0:10, FUN = pkM,a=10,b=b,p0=p0))

