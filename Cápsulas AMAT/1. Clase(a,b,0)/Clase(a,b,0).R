
# Clase (a,b,0) ---------------------------------------------------------

# Recursión
pk<-function(k,a,b,p0){
  
  while(k>0){
    return((a+b/k)*pk(k-1,a,b,p0))
  }
  
  return(p0)
  
}

# Ecuación lineal
fk<-function(k,a,b,p0){
  
  k*pk(k,a,b,p0)/pk(k-1,a,b,p0)

}


#Binomial(n,p)
n = 5 ; p = 0.5
a = -p/(1-p) ; b = (n+1)*p/(1-p) ; p0 = dbinom(0,n,p)
Teóricos<-dbinom(x = 0:n,size = n,prob = p)
Clase<-sapply(0:n, pk, a=a, b=b, p0=p0)

# Primero veamos que tenemos toda la probabilidad
sum(Clase)

tabla<-rbind(Teóricos,Clase)
colnames(tabla)<-0:n

#Comparación
tabla

#Gráfico de densidad
plot(x = -1:(n+1),y = c(0,Clase,0),type="h",
     lwd=5,col=rainbow(length(Clase)+2),
     ylab=expression(p[k]),xlab="k",
     main=expression(Bin(5,0.5)))
abline(h=0,v=0,lwd=0.5)

#Gráfico del fk:
plot(x = 1:n,y = sapply(X = 1:n,FUN = fk,a=a,b=b,p0=p0),
     col=rainbow(length(Clase)), main = "Función lineal",
     pch=16,ylab = expression(k*p[k]/p[k-1]))
abline(a=b,b=a,lty=2)

# Conclusión.

# Estas familias paramétricas nos ayudarán a modelar el número 
# de siniestros de una cia. aseguradora así como a facilitar 
# los cálculos del cáculo total de los montos de reclamación. 
# ¡Pero eso será asunto de otros video!



