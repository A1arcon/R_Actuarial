
# Clásica -----------------------------------------------------------------

# Credibilidad Completa/Total ---------------------------------------------

#Primeramente vamos a proponer un modelo con el que vamos a trabajar
# Supongamos entonces que nuestra información es la siguiente:
set.seed(4)
histórico = rgamma(150,shape = 20,rate = 1/100) # De aquí aprenderé.
MASS::truehist(histórico)

# Supongamos que no sabemos de dónde viene la información.
# De igual manera, siendo suficientemente buenos, nos damos 
# cuenta que un buen modelo puede venir de la familia de 
# distribuciones gamma.

library(fitdistrplus)
parámetros<-fitdist(data = histórico,distr = "gamma")$estimate
parámetros

# Entonces aquí decimos que tal vez:
alpha = 19.99 ; beta = 0.01

# De esta manera ya tenemos planteado un modelo para nuestros datos.

# La  prima de riesgo (esperanza) teórica de este modelo es:
mu <- alpha/beta ; mu ; mean(histórico)
# La varianza "teórica" de mis datos es:
sigma2 <- alpha/(beta^2) ; sigma2 ; var(histórico)

# La vida continúa... y ponemos a prueba nuestro modelo.
m = 125 # Cantidad de NUEVOS registros al día de hoy.

#Pensemos que eso no lo sabemos~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(10) #Escenario 1
S1 = rgamma(m,shape = 20,rate = 1/100) # Estos son los nuevos siniestros.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Nuestra muestra se ve así:
MASS::truehist(S1)

# Queremos ver ahora si podemos aplicar credibilidad completa(k,p).
k = 0.05 ; p = 0.9
# Para eso, se supone que la mínima "m" que necesitamos es:
# Var(S)*(z_{(p+1)/2}/(k*E[S]))^2. Es decir:
z = qnorm((p+1)/2)
min.m = ceiling(sigma2*(z/(k*mu))^2)

# Además:
min.m <= m 
# Tenemos suficientes periodos para pensar en usar 
# credibilidad completa(k,p) (Bajo hipótesis de normalidad).

# Para verificar que nuestra muestra se comporta con nuestra teoría:
var(S1)/m <= (k*mu/z)^2 #¡OJO! Var(S.barra) = Var(S)/m

# Entonces hay que tener cuidado pues según la teoría hay
# que comparar con la varianza de S.barra.

# Si esto no es algo que les parezca satisfactorio, dada una muestra
# podemos calcular un estimador de Var(S.barra). ¿Cómo? Bootstrap.
?boot::boot # https://www.datacamp.com/community/tutorials/bootstrap-r
set.seed(1995)
promedio<-function(datos,i){mean(datos[i])}
S.barra <- boot::boot(data = S1,statistic = promedio,R = 200000)$t

# Hacer bootstrap no me va a dar nueva información como tal.
# Solo para que vean
mean(S.barra) ; mean(S1)

# Pero en la varianza
var(S.barra) ; var(S1)/m #<- Esta es la varianza que buscamos.

# De cualquier modo:
var(S.barra) <= (k*mu/z)^2

# En resumen tenemos: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Esta es una condición que nos permitirá o no realizar C.C.(k,p)
# Condición 1)
min.m <= m 
# Estos nos dice que podemos verificar C.C.(k,p)
  
# Estos son indicios de que C.C.(k,p)
# Condición 2)
(sigma2/mu)*(z/k)^2<=sum(S1) # Cota inferior.

# Condición 3)
var(S1)/m <= (k*mu/z)^2 # Cota superior.

# Esto es un buen indicio de que se satisfaga C.C.(k,p) ~~~~~~~~~~~~~~~~~~~~~~~

# ¡Esto nos está diciendo que en efecto nuestro modelo PUEDE seguir vigente!
# En otras palabras, PUEDE TENER credibilidad completa(k,p). Por lo que no
# necesitaríamos actualizar la prima.

# ¿Cuándo sucedería que no...? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Imagina que lo anterior, no pasó y más bien...

# La vida continúa... y ponemos a prueba nuestro modelo.
m = 125 # Cantidad de NUEVOS registros al día de hoy.

#Pensemos que eso no lo sabemos~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(10) #Escenario 2
S2 = rgamma(m,shape = 30,rate = 1/135) # Estos son los nuevos siniestros.
# Podemos explicar el INCREMENTO por quizás la inflación.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Nuestra muestra se ve así:
MASS::truehist(S2)

# Queremos ver ahora si podemos aplicar credibilidad completa(k,p).
k = 0.05 ; p = 0.9
# Para eso, se supone que la mínima "m" que necesitamos es:
# Var(S)*(z_{(p+1)/2}/(k*E[S]))^2. Es decir:
z = qnorm((p+1)/2)
min.m = ceiling(sigma2*(z/(k*mu))^2)

# Además:
min.m <= m 
# Tenemos suficientes periodos para pensar en usar 
# credibilidad completa(k,p) (Bajo hipótesis de normalidad).

# Para verificar que nuestra muestra se comporta con nuestra teoría:
var(S2)/m <= (k*mu/z)^2 #¡OJO! Var(S.barra) = Var(S)/m

# ****** Metodología SoA ******
(sigma2/mu)*(z/k)^2<=sum(S2)

# Y sin embargo...
abs(mean(S2)-mu) <= k*mu
abs(mean(S2)-mu) # ¡Que podría ser una diferencia considerable!

# En resumen tenemos: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Esta es una condición que nos permitirá o no realizar C.C.(k,p)
# Condición 1)
min.m <= m 
# Estos nos dice que podemos verificar C.C.(k,p)

# Estos son indicios de que C.C.(k,p)
# Condición 2)
(sigma2/mu)*(z/k)^2<=sum(S2) # Cota inferior.

# Condición 3)
var(S2)/m <= (k*mu/z)^2 # Cota superior. ¡FALLA!

# Esto es un MAL indicio de que se satisfaga C.C.(k,p) ~~~~~~~~~~~~~~~~~~~~~~~

# ¡Esto nos da indicios de que la muestra ha cambiado, parece que ha AUMENTADO!
# Por lo que deberíamos replantear el modelo. Para una nueva prima de riesgo.

# ¿Qué sucedería si los montos DISMINUYERAN? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Imagina que lo anterior, no pasó y más bien...

# La vida continúa... y ponemos a prueba nuestro modelo.
m = 125 # Cantidad de NUEVOS registros al día de hoy.

#Pensemos que eso no lo sabemos~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(10) #Escenario 3
S3 = rgamma(m,shape = 12,rate = 1/70) # Estos son los nuevos siniestros.
# Podemos explicar la disminución quizás debido a una crisis.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Nuestra muestra se ve así:
MASS::truehist(S3)

# Queremos ver ahora si podemos aplicar credibilidad completa(k,p).
k = 0.05 ; p = 0.9
# Para eso, se supone que la mínima "m" que necesitamos es:
# Var(S)*(z_{(p+1)/2}/(k*E[S]))^2. Es decir:
z = qnorm((p+1)/2)
min.m = ceiling(sigma2*(z/(k*mu))^2)

# Además:
min.m <= m 
# Tenemos suficientes periodos para pensar en usar 
# credibilidad completa(k,p) (Bajo hipótesis de normalidad).

# Para verificar que nuestra muestra se comporta con nuestra teoría:
var(S3)/m <= (k*mu/z)^2 #¡OJO! Var(S.barra) = Var(S)/m

# ****** Metodología SoA ******
(sigma2/mu)*(z/k)^2<=sum(S3)

# Y sin embargo...
abs(mean(S3)-mu) <= k*mu
abs(mean(S3)-mu) # ¡Que podría ser una diferencia considerable!

# En resumen tenemos: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Esta es una condición que nos permitirá o no realizar C.C.(k,p)
# Condición 1)
min.m <= m 
# Estos nos dice que podemos verificar C.C.(k,p)

# Estos son indicios de que C.C.(k,p)
# Condición 2)
(sigma2/mu)*(z/k)^2<=sum(S3) # Cota inferior. ¡FALLA!

# Condición 3)
var(S3)/m <= (k*mu/z)^2 # Cota superior.

# Esto es un MAL indicio de que se satisfaga C.C.(k,p) ~~~~~~~~~~~~~~~~~~~~~~~

# ¡Esto nos da indicios de que la muestra ha cambiado, parece que ha DISMINUIDO!
# Por lo que deberíamos replantear el modelo. Para una nueva prima de riesgo.

# ¡OJO! Lo correcto en realidad sería, que siempre intentaramos 
# ver qué tanto se aleja realmente la media muestral de la media
# teórica. Si estas se alejan, aunque se satisfaga credibilidad 
# completa, lo ideal entonces sería volver a realizar un modelo.

# Entonces... no vayan por favor a "tirar un edificio"... 
# veamos un último escenario.

# La vida continúa... y ponemos a prueba nuestro modelo.
m = 125 # Cantidad de NUEVOS registros al día de hoy.

#Pensemos que eso no lo sabemos~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(10) #Escenario 4
S4 = rchisq(m,df = 5000) # Estos son los nuevos siniestros.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Nuestra muestra se ve así:
MASS::truehist(S4)

# Queremos ver ahora si podemos aplicar credibilidad completa(k,p).
k = 0.05 ; p = 0.9
# Para eso, se supone que la mínima "m" que necesitamos es:
# Var(S)*(z_{(p+1)/2}/(k*E[S]))^2. Es decir:
z = qnorm((p+1)/2)
min.m = ceiling(sigma2*(z/(k*mu))^2)

# Además:
min.m <= m 
# Tenemos suficientes periodos para pensar en usar 
# credibilidad completa(k,p) (Bajo hipótesis de normalidad).

# Para verificar que nuestra muestra se comporta con nuestra teoría:
var(S4)/m <= (k*mu/z)^2 #¡OJO! Var(S.barra) = Var(S)/m

# ****** Metodología SoA ******
(sigma2/mu)*(z/k)^2<=sum(S4)

# En resumen tenemos: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Esta es una condición que nos permitirá o no realizar C.C.(k,p)
# Condición 1)
min.m <= m 
# Estos nos dice que podemos verificar C.C.(k,p)

# Estos son indicios de que C.C.(k,p)
# Condición 2)
(sigma2/mu)*(z/k)^2<=sum(S4) # Cota inferior.

# Condición 3)
var(S4)/m <= (k*mu/z)^2 # Cota superior.

# Esto es un buen indicio de que se satisfaga C.C.(k,p) ~~~~~~~~~~~~~~~~~~~~~~~

# ¡Esto nos está diciendo que en efecto nuestro modelo PUEDE seguir vigente!
# En otras palabras, PUEDE TENER credibilidad completa(k,p). Por lo que no
# necesitaríamos actualizar la prima. 

# Y sin embargo...
abs(mean(S4)-mu) <= k*mu
abs(mean(S4)-mu) # ¡Se alejan demasiado!
# ¡ESTO NO PUEDE SER! Por eso las condiciones son simplemente INDICIOS.

# Conclusión: Este método tiene una debilidad grande y es que nos  
# estamos basando demasiado en el supuesto de normalidad.
# Perdiendo un poco el objetivo principal.

# Advertidos están.

# Credibilidad Clásica - Bootstrap ----------------------------------------

# ¿Entonces qué se puede hacer? Una alternativa que el autor (Egar) propone,
# es precisamente utilizar bootstrap. La idea es que con una misma muestra
# veamos mediante bootstrap, si se satisface la definición de credibilidad
# completa(k,p).

# Función para el bootstrap.
condición <- function(datos,índice){
  abs(mean(datos[índice])-mu)<=k*mu
}

# Escenario 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

set.seed(26)
# Realizamos el bootstrap en este escenario.
Por.def.1<-boot::boot(data = S1,statistic = condición,R = 99999)$t
# Con esto veremos si se satisface la definición de CC(k,p).
sum(Por.def.1)/length(Por.def.1)>=p

# Escenario 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Realizamos el bootstrap en este escenario.
Por.def.2<-boot::boot(data = S2,statistic = condición,R = 99999)$t
# Con esto veremos si se satisface la definición de CC(k,p).
sum(Por.def.2)/length(Por.def.2)>=p

# Escenario 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Realizamos el bootstrap en este escenario.
Por.def.3<-boot::boot(data = S3,statistic = condición,R = 99999)$t
# Con esto veremos si se satisface la definición de CC(k,p).
sum(Por.def.3)/length(Por.def.3)>=p

# Escenario 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Realizamos el bootstrap en este escenario.
Por.def.4<-boot::boot(data = S4,statistic = condición,R = 99999)$t
# Con esto veremos si se satisface la definición de CC(k,p).
sum(Por.def.4)/length(Por.def.4)>=p

# Escenario 5* (Extra) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ¿Y si la muestra cambia pero muuuy poquito?

#Pensemos que eso no lo sabemos~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(10) #Escenario 4
S5 = rgamma(n = m,shape = 20,rate = 1/95)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Realizamos el bootstrap en este escenario.
Por.def.5<-boot::boot(data = S5,statistic = condición,R = 99999)$t
# Con esto veremos si se satisface la definición de CC(k,p).
sum(Por.def.5)/length(Por.def.5)>=p
sum(Por.def.5)/length(Por.def.5) 
# En efecto... casi... casi alcanza la C.C.(k,p), PERO NO.  

# Cada una de estas pruebas muestra de una forma un tanto más sólida si
# debería o no satisfacer que el modelo explica los datos vía C.C.(k,p).

# Siempre será tema de discusión si se debe cambiar el modelo o no en estos
# casos.

# Credibilidad Parcial ----------------------------------------------------

# Entonces, recordemos entonces el escenario 1 en credibilidad completa:

# Esta vez, tendremos menos observaciones.

# La vida continúa... y ponemos a prueba nuestro modelo.
m = 25 # Cantidad de NUEVOS registros al día de hoy.

#Pensemos que eso no lo sabemos~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(707) #Escenario 1
S1 = rgamma(m,shape = 20,rate = 1/100) # Estos son los nuevos siniestros.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Nuestra muestra se ve así:
MASS::truehist(S1)

# Queremos ver ahora si podemos aplicar credibilidad completa(k,p).
k = 0.05 ; p = 0.9
# Para eso, se supone que la mínima "m" que necesitamos es:
# Var(S)*(z_{(p+1)/2}/(k*E[S]))^2. Es decir:
z = qnorm((p+1)/2)
min.m = ceiling(sigma2*(z/(k*mu))^2)

# Además:
min.m <= m 
# NO Tenemos suficientes periodos para pensar en usar 
# credibilidad completa(k,p) (Bajo hipótesis de normalidad).

# Por lo que aquí NO es válido usar la hipótesis de normalidad
# bajo cc(k,p).

# Más importante aún, si intentamos verificar cc(k,p).
set.seed(26)
# Función para el bootstrap.
condición <- function(datos,índice){
  abs(mean(datos[índice])-mu)<=k*mu
}
# Realizamos el bootstrap en este escenario.
cond1<-boot::boot(data = S1,statistic = condición,R = 99999)$t
# Con esto veremos si se satisface la definición de CC(k,p).
sum(cond1)/length(cond1)>=p
# No se satisface.

# Pero podemos acudir a Credibilidad Parcial(k,p,a) (CP(k,p,a)).
# Esto significa darle un peso diferente a la prima de riesgo,
# que solo "usar" los componentes teóricos. En este caso, se le
# da peso a la transformación: a*S.barra + (1-a)*E[S].
# Donde el factor de credibilidad "a" se calcula a partir de 
# nuestro modelo y la muestra como:
a = min(sqrt(m/((sigma2/mu^2)*(z/k)^2)),1) ; a
# Aquí obtuvimos a<1. Si a>=1 quiere decir que nuestra prima teórica
# necesitaría ser reemplazada. Un análisis mayor con la muestra
# sería necesario. O considerar esperar más tiempo para verificar
# credibilidad completa. En pocas palabras, a>=1 es señal de que
# algo podría estar saliendo mal.

# De tal manera que nuestra mejor propuesta es verificar si S.gorro
# satisface credibilidad parcial(k,p,a).
abs((a*mean(S1)+(1-a)*mu)-mu) <= k*mu

# Para una alternativa para ver si se satisface credibilidad parcial(k,p,a)
# es de igual manera usar bootstrap. Pero al tener pocos datos, hacer esto
# puede a veces ser muy volátil.
set.seed(4)
# Función para el bootstrap.
condición <- function(datos,índice){
  abs(a*mean(datos[índice])+(1-a)*mu-mu)<=k*mu
     #______________________________#
}
# Realizamos el bootstrap en este escenario.
version.cp<-boot::boot(data = S1,statistic = condición,R = 99999)$t
# Con esto veremos si se satisface la definición de CC(k,p).
p.cp<-sum(version.cp)/length(version.cp); p.cp>=p

# O lo que es equivalente con CC(k/a,p)
set.seed(4)
# Función para el bootstrap.
condición <- function(datos,índice){
  abs(mean(datos[índice])-mu)<=k/a*mu
                              #___#
}
# Realizamos el bootstrap en este escenario.
version.cc<-boot::boot(data = S1,statistic = condición,R = 99999)$t
# Con esto veremos si se satisface la definición de CC(k,p).
p.cc<-sum(version.cc)/length(version.cc); p.cc>=p

# Esto pues como ya vimos, CP(k,p,a) <=> CC(k/a,p)
p.cp==p.cc

# Esta verificación es la que en realidad nos puede decir que satisface
# credibilidad parcial(k,p,a).

# En este caso, podemos calibrar la prima de riesgo del modelo como:
a*mean(S1)+(1-a)*mu
# Pensando a la prima de tarifa, como calculada por algno de los principios,
# si ya tenemos una prima de tarifa dada, podemos quedarnos tranquilos
# si esta nueva prima de riesgo continúa siendo menor que la prima de tarifa,
# esto por la propiedad de cota inferior que es muy importante.

# Recordemos que:
# P.Riesgo = sum(S)/m :=S.barra < P.Tarifa 
# <=> 
# Total Pagado sum(S) < n*P.Tarifa = Total Cobrado

# Veamos un escenario donde las cosas cambian un poco ~~~~~~~~~~~~~~~~~~~~~

# Entonces, recordemos entonces el escenario 2.

# Esta vez, tendremos menos observaciones.

# La vida continúa... y ponemos a prueba nuestro modelo.
m = 25 # Cantidad de NUEVOS registros al día de hoy.

#Pensemos que eso no lo sabemos~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set.seed(10) #Escenario 2
S2 = rgamma(m,shape = 30,rate = 1/135) # Estos son los nuevos siniestros.
# Podemos explicar el incremento por quizás la inflación.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Nuestra muestra se ve así:
MASS::truehist(S2)

# Queremos ver ahora si podemos aplicar credibilidad completa(k,p).
k = 0.05 ; p = 0.9
# Para eso, se supone que la mínima "m" que necesitamos es:
# Var(S)*(z_{(p+1)/2}/(k*E[S]))^2. Es decir:
z = qnorm((p+1)/2)
min.m = ceiling(sigma2*(z/(k*mu))^2)

# Además:
min.m <= m 
# NO Tenemos suficientes periodos para pensar en usar 
# credibilidad completa(k,p) (Bajo hipótesis de normalidad).

# Espero que a estas alturas se den cuenta que min.m no cambia pues depende
# únicamente del modelo que se está proponiendo.

# Calculando el factor de credibilidad:
a = min(sqrt(m/((sigma2/mu^2)*(z/k)^2)),1) ; a

# Podríamos ver si satisface CP(k,p,a)
abs((a*mean(S2)+(1-a)*mu)-mu) <= k*mu # Parece que no.

# Okay, veamos si es cierto que con esta "a", en promedio,
# se satisface cp(k,p,a), lo hacemos vía bootstrap.
set.seed(26)
# Función para el bootstrap.
condición <- function(datos,índice){
  abs(mean(datos[índice])-mu)<=k/a*mu
  #___#
}
# Realizamos el bootstrap en este escenario.
version.cc<-boot::boot(data = S2,statistic = condición,R = 99999)$t
# Con esto veremos si se satisface la definición de CC(k,p).
p.cc<-sum(version.cc)/length(version.cc); p.cc>=p
# Pues en efecto, la distancia es grande:
abs(mean(S2)-mu)<=k/a * mu
# Así que a pesar de esto, no se satisface credibilidad parcial(k,p,a).
# Es aquí donde deberíamos tomar la decisión o bien de replantear el modelo
# o bien de esperar el número de periodos necesario. Ambos caminos, deben
# considerarse viendo los pros y contras, siempre basándose en la observación.

# Conclusión: Credibilidad clásica nos ayuda a verificar si podemos
# seguir considerando como vigente un modelo propuesto. Sin embargo,
# y como siempre en estadística, un análisis lógico y de un experto
# de área es necesario. Esto es, que la toma de decisiones se haga
# siempre observando un panorama general de las cosas.

# Bayesiana ---------------------------------------------------------------

set.seed(2012) ; n = 24
#Supongamos que yo no conozco de dónde viene esta muestra:~~~~~~~~~~~~~
p = 0.75
muestra <- rbinom(n,size = 1,prob = p)
#prob representa la theta que queremos estimar.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#La probabilidad de 1 empírica es:
mean(muestra)

#Nuestro objetivo es hacer inferencia sobre el parámetro
#theta, que es el de interés.

# Supongamos que según nuestra experiencia profesional,
# nosotros pensamos que la probabilidad tiene una dist.
# inicial: Theta~beta(a=7,b=3)

a = 7
b = 3

#Implícitamente, decimos que la media es:
a/(a+b)

#La distribución inicial se ve:
#P(Theta)
d.inicial = function(theta){dbeta(theta,a,b)}
plot(d.inicial, col = "red", lwd = 2,
     ylab = expression(f[Theta](theta)),
     xlab = expression(theta),
     main=expression(paste("Distribución inicial: ",P(Theta))))
grid()
abline(h=0,v=0,col="blue")

# Asumiendo que nuestros datos siguen una distribución
# X~Bernoulli(Theta), la verosimilitud es:
#P(X|Theta)
verosimilitud = function(theta,x){
                dbinom(x = x,size = 1,prob = theta)
                }
verosimilitud(0.8,1) #Arroja un número
verosimilitud(0.4,c(0,1,0,1,1,0)) #Arroja un vector
#Entonces la  distribución final, viene dada por:
#P(Theta|X)
d.posterior = function(theta){
              
              #Proceso de actualización
                numerador = function(theta){
                  aux = 1
                  for(x in muestra){
                    aux = verosimilitud(theta,x)*aux
                  }
                  aux*d.inicial(theta)
                }
              #Constante de integración
                denominador = integrate(f = numerador,
                                        lower = 0,upper = 1)$value
              #Posterior
                return(numerador(theta)/denominador)
                
              }

#Vemos que es de densidad en (0,1):
integrate(d.posterior,0,1)$value

#Que de hecho, según nuestros resultados, coincide con:
d.final = function(x){
                    #Estos son los parámetros finales.
            dbeta(x,n*mean(muestra)+a,n*(1-mean(muestra))+b)
          }

#Comparamos evaluando en algunos x
3:9/10
round(d.final(3:9/10),3)
round(d.posterior(3:9/10),3)

#Posterior:
library(latex2exp)
plot(d.posterior, col = "red", lwd = 2,
     from = 0 , to = 1,
     ylab = TeX('$f_{\\Theta | \\underline{X}} (\\theta)$'),
     xlab = expression(theta),
     main = TeX('Distribución final: $P(\\Theta | \\underline{X})$'))
plot(d.final,col="green",lwd=2,add=T)
grid()
abline(h=0,v=0,col="blue")

#Comparación entre final e inicial:
plot(d.final,from = 0, to = 1,
     main="Comparación",
     ylab = "Densidades",
     xlab = expression(theta),
     lwd=2,col="red",lty=1) ; grid()
plot(d.inicial,from = 0, to = 1,add=T,lwd=3,col="blue",lty=2)
#Ejes
abline(h = 0,v = 0,lwd=2)
#Leyenda
legend("topleft", 
       legend=c("Final","Inicial"),
       col=c("red","blue"), lty=c(1,2),lwd=3)

#Con lo que ya podemos hacer inferencia para el valor de theta.

# Pero antes de hacer eso, veamos qué 
# sucede si la muestra aumenta.

# Supongamos que hicimos otra encuesta y 
# (sin saber lo que pondremos ahora) tenemos 
# la siguiente muestra:

set.seed(12) 
m = 200 #Encuestas nuevas
muestra.nueva = rbinom(m,size = 1,prob = p)

muestra.total = c(muestra,muestra.nueva)
length(muestra.total)

# Ahora sí, saltándonos muchos pasos,
# Nuestra nueva densidad final es:

d.posterior.2.0 = function(x){
  dbeta(x,(n+m)*mean(muestra.total)+a,(n+m)*(1-mean(muestra.total))+b)
}
plot(d.posterior.2.0,col="blue")

#Esto si vamos desde la inicial, hasta esta nueva final.

#Nota: Hacer esto, es equivalente a hacer:

#Tomas como iniciales, los de la final anterior:
A = n*mean(muestra)+a
B = n*(1-mean(muestra))+b

d.final.2.0 = function(x){
  dbeta(x,m*mean(muestra.nueva)+A,m*(1-mean(muestra.nueva))+B)
}
plot(d.final.2.0,col="green",
     add=T)

#De esta manera ahora tenemos el siguiente comportamiento:
plot(d.final.2.0,from = 0, to = 1,
     main="Comparación",
     ylab = "Densidades",
     xlab = expression(theta),
     lwd=2,col="red",lty=1);grid()
plot(d.final,from = 0, to = 1,add=T,lwd=3,col="blue",lty=2)
plot(d.inicial,from = 0, to = 1,add=T,lwd=3,col="green",lty=3)
#Ejes
abline(h = 0,v = 0,lwd=2)
#Leyenda
legend("topleft", 
       legend=c("Final 2.0","Final 1.0","Inicial"),
       col=c("red","blue","green"), lty=c(1,2,3),lwd=3)

# Ahora sí, podemos hacer una mejor inferencia sobre el parámetro

A2 = m*mean(muestra.nueva)+A
B2 = m*(1-mean(muestra.nueva))+B

# Como nosotros conocemos la distribución final:

#Esperanza:
E=A2/(A2+B2) ; E
#Desviación estándar:
DE=sqrt(A2*B2/((A2+B2+1)*(A2+B2)^2)) ; DE
#Nos da el intervalo:
c(E-DE,E+DE)
#El valor real era:
p

# Credibilidad Bayesiana --------------------------------------------------

# Ya con todo lo anterior, nos queda simplemente obtener la prima
# de credibilidad bayesiana:

#Prima inicial
a/(a+b)

#Primera Prima final
z=(a+b)/(n+a+b) ; z
Theta = (1-z)*mean(muestra) + z*a/(a+b) 
Theta

#Segunda Prima final

#De igual manera se puede actualizar de dos formas equivalentes:

#Ajustando con toda la información, la inicial:
z=(a+b)/(n+m+a+b) ; z
Theta2.op1 = (1-z)*mean(muestra.total) + z*a/(a+b) 
#Prima final (opción 1)
Theta2.op1

#Ajustando la prima final anterior:
z = (A+B)/(m+A+B) ; z
Theta2.op2 = (1-z)*mean(muestra.nueva) + z*Theta
#Prima final (opción 2)
Theta2.op2


