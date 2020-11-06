

# Fórmula de Panjer -------------------------------------------------------

# Severidad Y
fy <- c(0.3,0.5,0.2)
names(fy)<-0:2

# Probabilidades de la severidad
fy

# Calculamos la esperanza, segundo momento y varianza de la severidad (Y).
EY <- sum(0:2*fy)
EY2<- sum((0:2)^2*fy)
VY <- EY2 -  EY^2

# Asumimos que la distribución de la frecuencia (N) es Binomial(3,0.5)
n = 3 ; p = 0.5
library(actuar)
?aggregateDist #Arroja la función de Distribución de 
Fs <- aggregateDist(method = "recursive",model.sev = fy,
                    model.freq = "binomial", size = n, prob = p)

# Imagen de Fs es lo siguiente:
Is <- c(0,Fs(knots(Fs))) #P[S<=t]

# Función de masa de probabilidad:
fs<-diff(Is)
names(fs)<- 0:(2*n) # ¡Todo el soporte de S está aquí!
fs
sum(fs)

# Vemos la función de masa de S.
plot(x = -1:(2*n+1),y = c(0,fs,0),type="h",
     lwd=5,col=rainbow(length(fs)+2),
     ylab="P(S=s)",xlab="s",
     main="Probabilidades de S")
abline(h=0,v=0,lwd=0.5)


# Generadora de momentos
Ms <- function(t){
  sum(exp(t*0:(2*n))*fs)
}

# Es uno:
Ms(0)

library(pracma)
#Esperanza
dM0 <- fderiv(Ms,x = 0,n = 1) ; dM0

# E[S]=E[N]*E[Y]
(n*p)*(EY)

#Segundo momento
d2M0 <- fderiv(Ms,x = 0,n = 2) ; d2M0

# Varianza
d2M0 - (dM0)^2

# Var(S) = Var(N)(E[Y])^2 + Var(Y)*E[N]
(n*p*(1-p))*(EY)^2 + (VY)*(n*p)



